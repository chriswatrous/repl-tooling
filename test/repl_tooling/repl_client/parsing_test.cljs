(ns repl-tooling.repl-client.parsing-test
  (:require [clojure.test :refer [async testing is] :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [devcards.core :as cards :include-macros true]
            [repl-tooling.eval :as eval]
            [repl-tooling.editor-helpers :as helpers]
            [repl-tooling.eval-helpers :refer-macros [eval-on-repl eval-and-parse]]
            [repl-tooling.repl-client.clojure :as clj]))

(set! cards/test-timeout 8000)
(cards/deftest evaluate-ellisions
  (async done
    (async/go
     (client/disconnect! :clj-ellisions-1)
     (let [repl (clj/repl :clj-ellisions-1 "localhost" 2233 identity)]
       (testing "objects without get-more"
         (check (eval/get-more-fn (:result (eval-and-parse "'(1 2 3)"))) => nil)
         (check (eval/get-more-fn (:result (eval-and-parse "[1 2 3]"))) => nil)
         (check (eval/get-more-fn (:result (eval-and-parse "20"))) => nil)
         (check (eval/get-more-fn (:result (eval-and-parse "\"SOME STR\""))) => nil))

       (testing "ellisions on lists"
         (let [res (eval-and-parse "(range)")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           (check (:as-text res) => "(0 1 2 3 4 5 6 7 8 9 ...)")
           (check (eval/without-ellision (:result res)) => '(0 1 2 3 4 5 6 7 8 9))
           (ellide-fn repl #(async/put! ellided %))
           (check (eval/without-ellision (async/<! ellided)) =>
                  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))))

       (testing "ellisions on vectors"
         (let [res (eval-and-parse "(vec (range 100))")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           (check (:as-text res) => "[0 1 2 3 4 5 6 7 8 9 ...]")
           (check (eval/without-ellision (:result res)) => [0 1 2 3 4 5 6 7 8 9])
           (check (eval/without-ellision (:result res)) => vector?)
           (ellide-fn repl #(async/put! ellided %))
           (check (async/<! ellided) => vector?)
           (check (eval/without-ellision (async/<! ellided)) =>
                  [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19])))

       (testing "ellisions on sets"
         (let [res (eval-and-parse "(set (range 100))")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           ; (check (:as-text res) => #"#\{.*\.{3}\}")
           (check (count (eval/without-ellision (:result res))) => 10)
           (check (eval/without-ellision (:result res)) => set?)
           (ellide-fn repl #(async/put! ellided %))
           (check (async/<! ellided) => set?)
           (check (-> ellided async/<! eval/without-ellision count) => 20)))

       (testing "ellisions on maps"
         (let [res (eval-and-parse "(into {} (map vector (range 100) (range 100)))")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           ; (check (:as-text res) => #"\{.*\.{3}\}")))
           (check (count (eval/without-ellision (:result res))) => 10)
           (check (eval/without-ellision (:result res)) => map?)
           (ellide-fn repl #(async/put! ellided %))
           (check (async/<! ellided) => map?)
           (check (-> ellided async/<! eval/without-ellision count) => 20)))

       (testing "ellisions on strings"
         (let [res (eval-and-parse "(apply str (range 100))")
               ellided (async/promise-chan)
               ellide-fn (eval/get-more-fn (:result res))]
           (check (:as-text res) => #"\".*\.{3}\"")
           (check (count (eval/without-ellision (:result res))) => 80)
           (ellide-fn repl #(async/put! ellided %))
           (check (-> ellided async/<! eval/without-ellision count) => 160)

           (testing "incomplete strings ellide to complete strings"
             (let [ellided-again (async/promise-chan)]
                ((-> ellided async/<! eval/get-more-fn) repl #(async/put! ellided-again %))
                (check (-> ellided-again async/<! count) => 190)
                (check (-> ellided-again async/<! eval/get-more-fn) => nil)))))

       (testing "no ellisions on taggable code"
         (let [code  "(tagged-literal 'foo/bar \"Baz\")"
               not-ellided (:result (eval-and-parse code))]
           (check (eval/get-more-fn not-ellided) => nil)))

       (testing "ellisions on taggable code"
         (let [ellided (-> "(do (defrecord Foo [a b]) (->Foo (range 20) 20))"
                           eval-and-parse :result)
               more-data (async/promise-chan)]
           (check (-> ellided helpers/obj :a count) => 11)))
           ; ; (check (-> ellided eval/without-ellision helpers/obj :a count) => 10)))
           ; ((eval/get-more-fn ellided) repl #(async/put! more-data %))
           ; (check (-> more-data async/<! helpers/obj :a count) => 21)))

       (testing "ellisions on browseable"
         (let [res (-> "java.util.List" eval-and-parse :result)
               more-data (async/promise-chan)
               even-more-data (async/promise-chan)
               ellide-fn (eval/get-more-fn res)]
           (check (eval/without-ellision res) => 'java.util.List)
           (ellide-fn repl #(async/put! more-data %))
           (check (-> more-data async/<! :attributes count) => 11)

           ((-> more-data async/<! eval/get-more-fn) repl #(async/put! even-more-data %))
           (check (-> even-more-data async/<! :attributes count) => 21)))

       (testing "expand ellisions till some function is true"
         (let [res (eval-and-parse "(range)")
               ellided (async/promise-chan)]
           (eval/more-until repl (:result res) #(some #{31} %) #(async/put! ellided %))
           (check (count (async/<! ellided)) => 41)))

       (client/disconnect! :clj-ellisions-1)
       (done)))))
