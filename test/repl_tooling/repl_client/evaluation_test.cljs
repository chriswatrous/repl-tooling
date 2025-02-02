(ns repl-tooling.repl-client.evaluation-test
  (:require [clojure.test :refer-macros [testing async is]]
            [devcards.core :as cards :include-macros true]
            [check.core :refer-macros [check]]
            [clojure.core.async :as async :include-macros true]
            [repl-tooling.repl-client :as client]
            [repl-tooling.eval :as eval]
            [repl-tooling.repl-client.clojure :as clj])
  (:require-macros [repl-tooling.eval-helpers :refer [eval-on-repl]]))

(set! cards/test-timeout 8000)
(cards/deftest clojure-evaluation
  (async done
    (client/disconnect! :evaluation-test)
    (async/go
     (let [out (async/chan)
           repl (clj/repl :evaluation-test "localhost" 2233 #(some->> % (async/put! out)))]
       (testing "evaluating request-response"
         (is (= {:result "3" :as-text "3"} (eval-on-repl "(+ 1 2)")))
         (is (= {:result "3" :as-text "3"} (async/<! out))))

       (testing "capturing output"
         (is (= {:result "nil" :as-text "nil"} (eval-on-repl "(println :foobar)")))
         (is (= {:out ":foobar\n"} (async/<! out)))
         (is (= {:result "nil" :as-text "nil"} (async/<! out))))

       (testing "passing args to result"
         (let [res (async/promise-chan)]
           (eval/evaluate repl "(+ 2 3)" {:pass {:literal true}} #(async/put! res %))
           (is (= {:as-text "5" :result "5" :literal true} (async/<! res)))
           (is (= {:as-text "5" :result "5" :literal true} (async/<! out)))))

       (testing "passing parameters to evaluation"
         (let [res (async/promise-chan)]
           (eval/evaluate repl "(/ 10 0)" {:filename "foo.clj" :row 12 :col 0}
                          #(async/put! res %))
           (is (re-find #"foo\.clj\" 12" (-> res async/<! :error)))))

       (testing "canceling an evaluation"
         (let [aux-repl (clj/repl :evaluation-test-break "localhost" 2233 identity)
               res (async/promise-chan)]
           (eval/evaluate repl "(Thread/sleep 5000)" {} #(async/put! res %))
           (async/<! (async/timeout 500))
           (eval/break repl aux-repl)
           (is (-> res async/<! :error))
           (client/disconnect! :evaluation-test-break)))

       (testing "bust evaluations"
         (let [res (async/chan)]
           (doseq [n (range 5)]
             (eval/evaluate repl (str ":foo" n) {} #(async/put! res %)))
           (is (= ":foo0" (-> res async/<! :result)))
           (is (= ":foo1" (-> res async/<! :result)))
           (is (= ":foo2" (-> res async/<! :result)))
           (is (= ":foo3" (-> res async/<! :result)))
           (is (= ":foo4" (-> res async/<! :result)))))

       (testing "bust evaluations with blocks"
         (let [res (async/chan)]
           (doseq [n (range 5)]
             (eval/evaluate repl (str "(str " n ")") {} #(async/put! res %)))
           (is (= "\"0\"" (-> res async/<! :result)))
           (is (= "\"1\"" (-> res async/<! :result)))
           (is (= "\"2\"" (-> res async/<! :result)))
           (is (= "\"3\"" (-> res async/<! :result)))
           (is (= "\"4\"" (-> res async/<! :result)))))

      (client/disconnect! :evaluation-test)
      (done)))))
