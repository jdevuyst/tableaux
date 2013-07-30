(ns tableaux.rewrite-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
            [tableaux.rewrite :refer :all]
            [tableaux.util :as u]))

(deftest rewritingsystem-tests
  (testing "post"
    (is (= (-> (rewriting-system [(fn [v] {:k #{v}})])
               (post [1 2 3])
               (.logs))
           {:k #{1 2 3}}))
    (is (= (-> (->RewritingSystem [(fn [v] {:k1 #{(dec v)}})
                                   (fn [v] {:k1 #{(inc v)}})
                                   (fn [v] {:k2 #{v}})]
                                  {:a #{1}}
                                  (list {:a #{1}}))
               (post [5])
               (.history)
               (first))
           {:k1 #{4 6} :k2 #{5}})))
  (testing "query"
    (is (= (-> (->RewritingSystem {}, {:a #{1 2}}, (list))
               (query :a))
           #{1 2}))
    (is (= (-> (->RewritingSystem {}, {:a #{1 2}}, (list))
               (query :b))
           #{})))
  (testing "since"
    (is (= (-> (rewriting-system [(fn [x] {:k #{x}})])
               (post [1 2])
               (post [3 4])
               (since :X))
           {:k #{1 2 3 4}}))
    (is (= (-> (rewriting-system [(fn [x] {:k #{x}})])
               (mark :X)
               (post [1 2])
               (mark :Y)
               (post [3 4])
               (since :Y))
           {:k #{3 4}}))
    (is (= (-> (rewriting-system [(fn [x] {:k #{x}})])
               (mark :X)
               (post [1 2])
               (mark :marker)
               (post [])
               (mark :Y)
               (post [3 4])
               (since :marker))
           {:k #{3 4}})))
  (testing "process"
    (is (= (-> (->RewritingSystem [(fn [x] {:k #{x}})]
                                  {:a #{1 2 3} :b #{4 5}}
                                  (list {:a #{1 2 3} :b #{4 5}}))
               (process post
                        nil
                        [(fn
                           ([] :a)
                           ([rs val] #{(inc val)}))])
               (query :k))
           #{2 3 4}))
    (is (= (-> (fn [rs] (->> [(fn
                                ([] :k)
                                ([rs x] #{(inc x)}))]
                             (process rs post nil)))
               (iterate (-> (rewriting-system [(fn [x] {:k #{x}})])
                            (post [1])))
               (nth 9)
               (query :k))
           (set (range 1 11)))))
  (testing "Branching"
    (is (= (->> (-> (->RewritingSystem [(fn [x] {:k #{x}})]
                                       {}
                                       (list {:a #{[1 2] ['x 'y] [:A :B]} :b #{4 5}}))
                    (process (u/serialize post)
                             nil
                             [(fn
                                ([] :a)
                                ([rs x] [x]))]))
                (r/map #(query % :k))
                (r/fold (r/monoid conj hash-set)))
           (u/one-from-each [[1 2] ['x 'y] [:A :B]])))))

(run-tests)
