(ns repl-tooling.repl-client.clojurescript
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [cljs-blob-contents]])
  (:require [repl-tooling.repl-client :as client]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [repl-tooling.eval :as eval]
            [repl-tooling.features.autocomplete :as f-auto]
            [repl-tooling.repl-client.cljs.autocomplete :as cljs-auto]
            [repl-tooling.editor-helpers :as helpers]))

(def blob (cljs-blob-contents))

(defn evaluate-code [in pending command opts callback]
  (let [id (gensym)
        code (str "(cljs.core/pr-str (try (clojure.core/let [res\n(do\n" command
                  "\n)] ['" id " :result (cljs.core/pr-str res)]) (catch :default e "
                  "['" id " :error (cljs.core/pr-str e)])))\n")]
    (swap! pending assoc id {:callback callback :opts opts})
    (when-let [ns-name (:namespace opts)]
      (async/put! in (str "(ns " ns-name ")")))
    (async/put! in code)
    id))

(defn- generic-autocomplete [repl ns-name prefix]
  (js/Promise. (fn [resolve]
                 (cljs-auto/complete repl
                                     ns-name
                                     prefix
                                     #(if-let [res (:result %)]
                                        (resolve (helpers/read-result res))
                                        (resolve []))))))

(defn- lumo-autocomplete [repl ns-name prefix]
  (js/Promise. (fn [resolve]
                 (eval/evaluate repl
                                `(lumo.repl/get-completions ~prefix cljs.core/js->clj)
                                {:namespace ns-name :ignore true}
                                #(if-let [res (:result %)]
                                   (resolve (helpers/read-result res))
                                   (resolve []))))))

(defn- detect-autocomplete [repl ns-name text prefix row col state]
  (let [treat (fn [{:keys [result]}]
                (if result
                  (swap! state assoc :autocomplete-kind :lumo)
                  (swap! state assoc :autocomplete-kind :generic)))]
    (.
      (js/Promise. (fn [resolve]
                     (eval/evaluate repl
                                    "(require 'lumo.repl)"
                                    {:ignore true}
                                    treat)))
      (then (fn []
              (f-auto/complete repl ns-name text prefix row col))))))

(defrecord Evaluator [in pending state]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (evaluate-code in pending command opts callback))
  (break [this id])

  f-auto/AutoComplete
  (complete [repl ns-name text prefix row col]
    (case (:autocomplete-kind @state)
      nil (detect-autocomplete repl ns-name text prefix row col state)
      :lumo (lumo-autocomplete repl ns-name prefix)
      :generic (generic-autocomplete repl ns-name prefix))))

(defn- treat-result-of-call [out pending output-fn]
  (if-let [pendency (and (vector? out) (some->> out first (get @pending)))]
    (let [[id key parsed] out
          opts (:opts pendency)
          ignore? (:ignore opts)
          result (merge {:as-text out key parsed}
                        (:pass opts))]
      ((:callback pendency) result)
      (swap! pending dissoc id)
      (when-not ignore? (output-fn result)))
    (output-fn {:out out})))

(defn- pending-evals [pending output-fn out]
  (try
    (treat-result-of-call (-> out reader/read-string reader/read-string)
                          pending output-fn)
    (catch :default _
      (output-fn {:out out}))))

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket! session-name host port)
        pending-cmds (atom {})]
    (async/go-loop []
      (if-let [output (async/<! out)]
        (do
          (pending-evals pending-cmds on-output output)
          (recur))
        (on-output nil)))
    (let [evaluator (->Evaluator in pending-cmds (atom {}))]
      (eval/evaluate evaluator blob {} #(prn :EVAL-RES %))
      evaluator)))
