(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [clojure.walk :as walk]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (as-html [this ratom root?]))

(defprotocol Parseable
  (as-renderable [self repl]))

(declare ->indexed)
(defn- parse-inner-root [objs more-fn a-for-more]
  (let [inner (cond-> (mapv #(as-html (deref %) % false) objs)
                      more-fn (conj a-for-more))]
    (->> inner
         (interpose [:div {:class "whitespace"} " "])
         (map #(with-meta %2 {:key %1}) (range)))))

(defn parse-inner-for-map [objs more-fn a-for-more]
  (let [sep (cycle [[:div {:class "whitespace"} " "]
                    [:div {:class "coll whitespace"} ", "]])
        inner (->> objs
                   (mapcat #(-> % deref :obj))
                   (map #(as-html (deref %) % false)))]
    (-> inner
        (interleave sep)
        butlast
        (cond-> more-fn (conj (second sep) a-for-more))
        (->> (map #(with-meta %2 {:key %1}) (range))))))

(defrecord Indexed [open obj close kind expanded? more-fn repl]
  Renderable
  (as-html [_ ratom root?]
    (let [reset-atom #(let [new-idx (->indexed % repl)]
                        (swap! ratom
                               (fn [indexed]
                                 (assoc indexed
                                        :obj (vec (concat obj (:obj new-idx)))
                                        :more-fn (:more-fn new-idx)))))
          a-for-more [:a {:href "#"
                          :on-click (fn [e]
                                      (.preventDefault e)
                                      (more-fn repl false reset-atom))}
                      "..."]]

      [:div {:class ["row" kind]}
       [:div {:class ["coll" kind]}
        (when root?
          [:a {:class ["chevron" (if expanded? "opened" "closed")] :href "#"
               :on-click (fn [e]
                           (.preventDefault e)
                           (swap! ratom update :expanded? not))}])
        [:div {:class "delim open"} open]
        [:div {:class "inner"} (if (#{"map"} kind)
                                 (parse-inner-for-map obj false a-for-more)
                                 (parse-inner-root obj more-fn a-for-more))]
        [:div {:class "delim close"} close]]

       (when (and root? expanded?)
         [:div {:class "children"}
          [:<>
           (cond-> (mapv #(as-html (deref %) % true) obj)
                   more-fn (conj a-for-more)
                   :then (->> (map (fn [i e] [:div {:key i :class "row"} e]) (range))))]])])))

(defrecord Leaf [obj]
  Renderable
  (as-html [_ _ _]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:div {:class tp} (pr-str obj)])))

(defn- ->indexed [obj repl]
  (let [more-fn (eval/get-more-fn obj)
        children (mapv #(as-renderable % repl) (eval/without-ellision obj))]
    (cond
      (vector? obj) (->Indexed "[" children "]" "vector" false more-fn repl)
      (set? obj) (->Indexed "#{" (vec children) "}" "set" false more-fn repl)
      (map? obj) (->Indexed "{" (vec children) "}" "map" false more-fn repl)
      (seq? obj) (->Indexed "(" children ")" "list" false more-fn repl))))

(extend-protocol Parseable
  helpers/WithTag
  (as-renderable [self repl]
    (let [tag (helpers/tag self)
          subelement (-> self helpers/obj (as-renderable repl))]
      (r/atom
        (reify Renderable
          (as-html [_ ratom root?]
            [:div {:class "tagged"} [:span {:class "tag"} tag]
             (as-html @subelement subelement root?)])))))

  default
  (as-renderable [obj repl]
    (r/atom
      (cond
        (coll? obj) (->indexed obj repl)
        :else (->Leaf obj)))))

(defn parse-result
  "Will parse a result that comes from the REPL in a r/atom so that
it'll be suitable to be rendered with `view-for-result`"
  [result repl]
  (let [parsed (helpers/parse-result result)
        res (:result parsed)]
    (if res
      (as-renderable res repl)
      (with-meta (as-renderable (:error result) repl) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state repl]
  (as-html @state state true))
