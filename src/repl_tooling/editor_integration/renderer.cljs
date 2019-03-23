(ns repl-tooling.editor-integration.renderer
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [repl-tooling.eval :as eval]
            [clojure.walk :as walk]
            [repl-tooling.editor-helpers :as helpers]))

(defprotocol Renderable
  (as-html [this ratom root?])
  (as-text [this ratom root?]))

(defprotocol Parseable
  (as-renderable [self repl]))

(defn- parse-inner-root [objs more-fn a-for-more]
  (let [inner (cond-> (mapv #(as-html (deref %) % false) objs)
                      more-fn (conj a-for-more))]
    (->> inner
         (interpose [:span {:class "whitespace"} " "])
         (map #(with-meta %2 {:key %1}) (range)))))

(defn parse-inner-for-map [objs more-fn a-for-more]
  (let [sep (cycle [[:span {:class "whitespace"} " "]
                    [:span {:class "coll whitespace"} ", "]])
        inner (->> objs
                   (mapcat #(-> % deref :obj))
                   (map #(as-html (deref %) % false)))]
    (-> inner
        (interleave sep)
        butlast
        vec
        (cond-> more-fn (conj (second sep) a-for-more))
        (->> (map #(with-meta %2 {:key %1}) (range))))))

(defrecord ObjWithMore [obj-atom more-fn attributes-atom expanded? repl]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class ["browseable"]}
     [:div {:class ["object"]}
      (as-html @obj-atom obj-atom root?)
      (when more-fn
        [:a {:href "#"
             :on-click (fn [e]
                         (.preventDefault e)
                         (more-fn repl #(swap! ratom assoc
                                               :more-fn nil
                                               :expanded? true
                                               :attributes-atom (as-renderable (:attributes %) repl))))}
         (when root? "...")])]
     (when (and root? expanded?)
       [:div {:class "row children"}
        [as-html @attributes-atom attributes-atom true]])]))

(defn- assert-root [txt]
  (if (-> txt first (= :row))
    txt
    [:row txt]))

(declare ->indexed)
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
        [:div {:class "delim opening"} open]
        [:div {:class "inner"} (if (#{"map"} kind)
                                 (parse-inner-for-map obj more-fn a-for-more)
                                 (parse-inner-root obj more-fn a-for-more))]
        [:div {:class "delim closing"} close]]

       (when (and root? expanded?)
         [:div {:class "children"}
          [:<>
           (cond-> (mapv #(as-html (deref %) % true) obj)
                   more-fn (conj a-for-more)
                   :then (->> (map (fn [i e] [:div {:key i :class "row"} e]) (range))))]])]))

  (as-text [_ ratom root?]
    (let [children (map #(as-text @% % false) obj)
          toggle #(swap! ratom update :expanded? not)
          extract-map #(-> % second (str/replace #"^\[" "") (str/replace #"\]$" ""))
          txt (if (= "map" kind)
                [:text (->> obj
                            (map #(extract-map (as-text @% % false)))
                            (str/join ", ")
                            (#(str open % close)))]
               [:text (str open (->> children (map second) (str/join " ")) close)])
          rows (if root?
                 [:row [:expand (if expanded? "-" "+") toggle] txt]
                 txt)]
      (if expanded?
        (apply conj rows (map #(assert-root (as-text @% % true)) obj))
        rows))))

(defrecord Leaf [obj]
  Renderable
  (as-html [_ _ _]
    (let [tp (cond
               (string? obj) "string"
               (number? obj) "number"
               (boolean? obj) "bool"
               (nil? obj) "nil"
               :else "other")]
      [:div {:class tp} (pr-str obj)]))
  (as-text [_ _ _]
    [:text (pr-str obj)]))

(defn- ->indexed [obj repl]
  (let [more-fn (eval/get-more-fn obj)
        children (mapv #(as-renderable % repl) (eval/without-ellision obj))]
    (cond
      (vector? obj) (->Indexed "[" children "]" "vector" false more-fn repl)
      (set? obj) (->Indexed "#{" (vec children) "}" "set" false more-fn repl)
      (map? obj) (->Indexed "{" (vec children) "}" "map" false more-fn repl)
      (seq? obj) (->Indexed "(" children ")" "list" false more-fn repl))))

(defrecord IncompleteStr [string repl]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class "string big"}
     [:span (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
     (when-let [get-more (eval/get-more-fn string)]
       [:a {:href "#"
            :on-click (fn [e]
                        (.preventDefault e)
                        (get-more repl #(prn (swap! ratom assoc :string %))))}
         "..."])
     "\""])
  (as-text [_ ratom root?]
    (if root?
      [:row
       [:text (-> string eval/without-ellision pr-str (str/replace #"\"$" ""))]
       [:button "..." #(let [f (eval/get-more-fn string)]
                         (f repl (fn [obj]
                                   (if (string? obj)
                                     (reset! ratom (->Leaf obj))
                                     (swap! ratom assoc :string obj))
                                   (%))))]
       [:text "\""]]
      [:text (pr-str string)])))

(defrecord Tagged [tag subelement]
  Renderable
  (as-html [_ ratom root?]
    [:div {:class "tagged"} [:span {:class "tag"} tag]
     [as-html @subelement subelement root?]]))

(extend-protocol Parseable
  helpers/IncompleteStr
  (as-renderable [self repl]
    (r/atom (->IncompleteStr self repl)))

  helpers/Browseable
  (as-renderable [self repl]
    (let [{:keys [object attributes]} self]
      (r/atom (->ObjWithMore (as-renderable object repl)
                             (eval/get-more-fn self)
                             (as-renderable attributes repl)
                             false
                             repl))))

  helpers/WithTag
  (as-renderable [self repl]
    (let [tag (helpers/tag self)
          subelement (-> self helpers/obj (as-renderable repl))]
      (r/atom (->Tagged tag subelement))))

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
  (let [parsed (helpers/parse-result result)]
    (if (contains? parsed :result)
      (as-renderable (:result parsed) repl)
      (with-meta (as-renderable (:error result) repl) {:error true}))))

(defn view-for-result
  "Renders a view for a result. If it's an error, will return a view
suitable for error backtraces. If it's a success, will return a success
view. Expects a r/atom that comes from `parse-result`"
  [state]
  [as-html @state state true])

(defn txt-for-result
  "Renders a view for a result, but in textual format. This view will be
in a pseudo-hiccup format, like so:
[:row [:expand \"+\" some-fn]
      [:text \"(1 2 3 4 5 6\"]
      [:button \"...\" some-fn]
      [:text \")\"]]

Where :row defines a row of text, :text a fragment, :button a text that's
associated with some data (to be able to ellide things) and :expand is to
make a placeholder that we can expand (+) or collapse (-) the structure"
  [state]
  (def state state)
  (assert-root (as-text @state state true)))

; (def a (txt-for-result state))
;
; (def repl (-> @repl-tooling.integration.ui/state :repls :eval))
; ((get-in a [2 2]) repl #(prn (type %)))
