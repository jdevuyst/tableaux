(ns tableaux.bag-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
            [tableaux.bag :refer :all]
            [tableaux.util :as u]))

(deftest TupleBag-tests
  (testing "post"
    (is (= (-> (tuple-bag [(constantly [:k])])
               (post [1 2 3])
               (.everything))
           {:k #{1 2 3}}))
    (is (= (-> (->TupleBag [(constantly [:k1])
                            (constantly [:k2])]
                           {:a #{1}}
                           {:a #{1}}
                           (list {:a #{1}}))
               (post [5])
               (.history)
               (first))
           {:k1 #{5} :k2 #{5}})))
  (testing "query"
    (is (= (-> (->TupleBag [], {:a #{1 2}}, {:a #{1 2}}, (list))
               (query :a))
           #{1 2}))
    (is (= (-> (->TupleBag [], {:a #{1 2}}, {:a #{1 2}}, (list))
               (query :b))
           #{})))
  (testing "since"
    (is (= (-> (tuple-bag [(constantly [:k])])
               (post [1 2])
               (post [3 4])
               (since :X))
           {:k #{1 2 3 4}}))
    (is (= (-> (tuple-bag [(constantly [:k])])
               (mark :X)
               (post [1 2])
               (mark :Y)
               (post [3 4])
               (since :Y))
           {:k #{3 4}}))
    (is (= (-> (tuple-bag [(constantly [:k])])
               (mark :X)
               (post [1 2])
               (mark :marker)
               (post [])
               (mark :Y)
               (post [3 4])
               (since :marker))
           {:k #{3 4}})))
  (testing "newest"
    (is (= (-> (tuple-bag [(constantly [:k])])
               (post [1 2])
               (post [3])
               (newest :k))
           3))
    (is (= (-> (tuple-bag [(fn [x] [(first x)])])
               (post [[1 2]])
               (post [[3 4]])
               (post [[3 5]])
               (post [[6 6]])
               (post [[1 7]])
               (newest 3))
           [3 5])))
  (testing "process"
    (is (= (-> (->TupleBag [(constantly [:k])]
                           {:a #{1 2 3} :b #{4 5}}
                           {:a #{1 2 3} :b #{4 5}}
                           (list {:a #{1 2 3} :b #{4 5}}))
               (process :bogus-marker
                        [(fn
                           ([] :a)
                           ([tb val] #{(inc val)}))])
               (#(apply post %))
               (query :k))
           #{2 3 4}))
    (is (= (-> (fn [tb] (->> [(fn
                                ([] :k)
                                ([tb x] #{(inc x)}))]
                             (process tb :bogus-marker)
                             (apply post)))
               (iterate (-> (tuple-bag [(constantly [:k])])
                            (post [1])))
               (nth 9)
               (query :k))
           (set (range 1 11))))))

(deftest indexer-tests
  (testing "Indexers"
    (is (= (index-by-arity [1 2 3])
           [[:by-arity 3]]))
    (is (= (index-pairs-by-first [1 '(a b)])
           [[:pairs-by-first 1]]))
    (is (= (index-pairs-by-first-of-second [1 '(a b)])
           [[:pairs-by-first-of-second 'a]]))
    (is (= (index-pairs-by-first+firstsecond-of-second [1 '(a (b c))])
           [[:pairs-by-first+second-of-second 'a 'b]]))
    (is (= (index-pairs-by-first+firstsecond-of-second [1 '(a b)])
           [[:pairs-by-first+second-of-second 'a nil]]))
    (is (= (index-triples-by-first-second [1 2 3])
           [[:triples-by-first-second 1 2]]))
    (is (= (index-triples-by-first-third [1 2 3])
           [[:triples-by-first-third 1 3]]))
    (is (= (index-triples-by-second [1 2 3])
           [[:triples-by-second 2]]))
    (is (= (index-triples-by-third [1 2 3])
           [[:triples-by-third 3]]))))

(run-tests)