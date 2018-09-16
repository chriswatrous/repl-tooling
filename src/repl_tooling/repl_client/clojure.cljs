(ns repl-tooling.repl-client.clojure
  (:require-macros [repl-tooling.repl-client.clj-helper :refer [blob-contents]])
  (:require [repl-tooling.repl-client.protocols :as repl]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [cljs.core.async :as async :refer-macros [go go-loop]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def blob (blob-contents))
; (def ^:private fs (js/require "fs"))
; (def blob (.. fs (readFileSync "../resources/blob.clj") str))

(defn- next-eval! [state]
  (when (= (:state @state) :ready)
    (when-let [cmd (-> @state :pending first)]
      (swap! state (fn [s]
                     (-> s
                         (update :pending #(->> % (drop 1) vec))
                         (assoc :processing cmd)
                         (assoc :state :evaluating))))
      (prn [:RAW-IN (:cmd cmd)])
      (async/put! (:channel-in @state) (:cmd cmd)))))

(defn- add-to-eval-queue! [id chan cmd state]
  (swap! state update :pending conj {:cmd cmd :channel chan :id id})
  (next-eval! state))

(defn- prepare-opts [repl {:keys [filename row col namespace]}]
  (let [state (-> repl :session deref :state)
        set-params #(case %
                      {:repl-tooling/param :unrepl/sourcename} (str filename)
                      {:repl-tooling/param :unrepl/column} (or col 0)
                      {:repl-tooling/param :unrepl/line} (or row 0)
                      %)]
    (when namespace
      (add-to-eval-queue! (gensym) (async/chan) (str "(ns " namespace ")") state))
    (when (or filename row col)
      (add-to-eval-queue! (gensym) (async/chan)
                          (->> @state :actions :set-source (map set-params))
                          state))))

(declare repl)
(defrecord Evaluator [session]
  eval/Evaluator
  (evaluate [this command opts callback]
    (let [id (gensym)
          chan (async/chan)
          state (:state @session)]
      (prepare-opts this opts)
      (add-to-eval-queue! id chan (str command "\n") state)
      (go (callback (<! chan)))
      id))

  (break [this id]
    (when (-> @session :state deref :processing :id (= id))
      ; RESET connection, because UNREPL's interrupt is not working!
      ; First, clear all pending evals
      (some-> @session :state deref :processing :channel (doto
                                                           (async/put! {})
                                                           (async/close!)))

      (doseq [pending (-> @session :state deref :pending-evals)]
        (async/put! (:channel pending) {})
        (async/close! (:channel pending)))

      (client/disconnect! (:session-name @session))
      (let [evaluator (repl (:session-name @session)
                            (:host @session) (:port @session)
                            (-> @session :state deref :on-output))]
        (reset! session @(:session evaluator))))))

(deftype TaggedObj [tag obj]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#")
    (-write writer tag)
    (-write writer " ")
    (-write writer (pr-str obj))))

(defn- default-tags [tag data]
  (TaggedObj. tag data))

(deftype IncompleteStr [string]
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer (pr-str (str (first string) " ..."))))

  IMeta
  (-meta [coll] {:get-more (-> string second :repl-tooling/...)}))

(def ^:private decoders
  (let [param-decoder (fn [p] {:repl-tooling/param p})
        more-decoder (fn [{:keys [get]}] {:repl-tooling/... get})
        ns-decoder identity]
    {'unrepl/param param-decoder
     ; 'unrepl/ns ns-decoder
     ; 'unrepl.java/class identity
     'unrepl/... more-decoder
     'unrepl/string #(IncompleteStr. %)}))

(defn- eval-next! [state]
  (swap! state assoc :state :ready)
  (next-eval! state))

(defn- start-eval! [{:keys [actions]} state]
  (swap! state update :processing #(assoc %
                                          :interrupt (:interrupt actions)
                                          :background (:background actions))))

(defn- parse-res [result]
  (let [to-string #(cond
                     (instance? IncompleteStr %)
                     %

                     (not (coll? %)) (pr-str %)

                     (and (map? %) (:repl-tooling/... %))
                     (with-meta '... {:get-more (:repl-tooling/... %)})

                     :else %)]
    (if (coll? result)
      {:as-text (walk/prewalk to-string result)}
      {:as-text (to-string result)})))

(defn- send-result! [res exception? state]
  (let [parsed (parse-res res)
        msg (assoc parsed (if exception? :error :result) (pr-str res))
        on-out (:on-output @state)]
    (on-out msg)
    (when-let [chan (-> @state :processing :channel)]
      (async/put! chan msg)
      (async/close! chan))))

(defn- send-output! [out state]
  (let [on-out (:on-output @state)]
    (on-out {:out out})))

(defn- treat-unrepl-message! [raw-out state]
  (let [parsed (try (reader/read-string {:readers decoders :default default-tags} raw-out)
                 (catch :default e))
        [cmd args] (when (vector? parsed) parsed)]
    (case cmd
      :prompt (eval-next! state)
      :started-eval (start-eval! args state)
      :eval (send-result! args false state)
      :exception (send-result! args true state)
      :out (send-output! args state)
      :nothing-really)))

(defn- treat-hello! [hello state]
  (let [[_ res] (reader/read-string {:readers decoders} hello)]
    (swap! state assoc
           :session (:session res)
           :actions (:actions res))))

(defn- treat-all-output! [raw-out state]
  (prn [:RAW (str raw-out)])

  (if-let [hello (re-find #"\[:unrepl/hello.*" (str raw-out))]
    (treat-hello! hello state)
    (if (:session @state)
      (treat-unrepl-message! raw-out state))))
      ; ((:on-output @session) {:unexpected (str raw-out)}))))

(defn repl [session-name host port on-output]
  (let [[in out] (client/socket2! session-name host port)
        state (atom {:state :starting
                     :processing nil
                     :pending []
                     :channel-in in
                     :on-output on-output})
        session (atom {:state state
                       :session-name session-name
                       :host host
                       :port port})
        pending-cmds (atom {})]
    (async/put! in blob)
    (go-loop [string (<! out)]
      (when string
        (treat-all-output! string state)
        (recur (<! out))))
    (->Evaluator session)))

(defrecord SelfHostedCljs [evaluator pending]
  eval/Evaluator
  (evaluate [_ command opts callback]
    (let [id (gensym)
          ; code `(clojure.core/let [res# ~command]
          ;         [(quote ~id) res#])
          code (str "(clojure.core/let [res " command "\n] ['" id " (pr-str res)])\n")]
          ; code (str/replace-first code #"___COMMAND___" (str code))]

      (swap! pending assoc id callback)
      (prn [:EVAL? code])
      (async/put! (-> evaluator :session deref :state deref :channel-in) code)
      ; (eval/evaluate evaluator
      ;                `(clojure.core/let [res# ~command]
      ;                   [(quote ~id) res#])
      ;                {}
      ;                identity)
      (swap! (:session evaluator) assoc :pending [])
      id))

  (break [this id]))

(defn- pending-evals-for-cljs [pending output-fn]
  (fn [{:keys [out as-text] :as res}]
    (prn [:CLJS-OUT res])
    (prn [:CLJS-OUT as-text])
    (prn [:CLJS-OUT out])
    ; (prn)
    (if (and out (str/starts-with? out "["))
      (let [[id parsed] (reader/read-string {:default default-tags} out)]
        (if-let [callback (get @pending id)]
          (do (callback {:result parsed}) (swap! pending dissoc id))
          (output-fn {:out out :result [id parsed]})))
      (output-fn {:out out}))))

; (defn- cljs-repl [clj-evaluator]
;     (->SelfHostedCljs clj-evaluator pending))

(defn self-host [clj-evaluator command]
  (let [pending (atom {})
        cljs-repl (->SelfHostedCljs clj-evaluator pending)
        old-fn (-> clj-evaluator :session deref :state deref :on-output)]
    ; #_
    ; (eval/evaluate clj-evaluator command {} identity)
    (swap! (-> clj-evaluator :session deref :state)
           assoc :on-output (pending-evals-for-cljs pending old-fn))

    ; (js/Promise. (fn [resolve]
    ;                (swap! (-> clj-evaluator :session deref :state)
    ;                       assoc :on-output (pending-evals-for-cljs pending old-fn))
    ;                (js/setTimeout #(resolve cljs-repl) 500)))))

    ; #_
    (js/Promise. (fn [resolve]
                   (eval/evaluate clj-evaluator command {}
                                  (fn [{:keys [error]}]
                                    (when error
                                      (resolve nil))))
                   ; CLJS self-hosted REPL never returns, so we'll just set a timeout
                   (js/setTimeout #(resolve cljs-repl) 500)))))
    ; cljs-repl))

    ;                (eval/evaluate cljs-repl command {}
    ;                               (fn [{:keys [error] :as foo}]
    ;                                 (prn [:RESULT foo])
    ;                                 (if error
    ;                                   (resolve nil)
    ;                                   (resolve cljs-repl))))))))

#_#_
(shadow.cljs.devtools.api/repl :dev)
(ns repl-tooling.repl-client.clojure)

#_
(async/put! (-> clj-evaluator :session deref :state deref :channel-in) "(+ 1 2)\n")

#_#_#_
(def pending (atom {}))
(def cljs-repl (->SelfHostedCljs clj-evaluator pending))
(def old-fn (-> clj-evaluator :session deref :state deref :on-output))
#_#_
(def new-fn (pending-evals-for-cljs pending old-fn))
(swap! (-> clj-evaluator :session deref :state)
       assoc :on-output new-fn)

#_
(eval/evaluate clj-evaluator cmd {} (fn [{:keys [error]}]))
