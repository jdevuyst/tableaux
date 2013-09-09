(ns tableaux.util-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
            [tableaux.util :refer :all]))

(deftest utility-tests
  (testing "mmap-merge"
    (is (= (mmap-merge {:a #{1}} {:a #{2}})
           {:a #{1 2}})))
  (testing "mmap-diff"
    (is (= (mmap-diff {:a #{1 2 3} :b #{4} :c #{5}}
                      {:a #{2 9} :b #{4}})
           {:a #{1 3} :c #{5}})))
  (testing "rjuxt"
    (is (= (->> (rjuxt range [4 5 6 7])
                (r/fold (r/monoid conj vector)))
           (for [x [4 5 6 7] y (range x)] [x y]))))
  (testing "empty? and not-empty?"
    (is (fold-empty? []))
    (is (not (fold-empty? [1]))))
  (testing "fold-as"
    (is (= (foldset [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
           #{1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16})))
  (testing "one-from-each"
    (is (= (foldset (one-from-each [[1 2] [:a :b] ['c 'd]]))
           '#{#{1 :a c} #{1 :a d} #{1 :b c} #{1 :b d}
              #{2 :a c} #{2 :a d} #{2 :b c} #{2 :b d}}))
    (is (= (foldset (one-from-each [[1 2] [] [:a :b] ['c 'd]]))
           '#{#{1 :a c} #{1 :a d} #{1 :b c} #{1 :b d}
              #{2 :a c} #{2 :a d} #{2 :b c} #{2 :b d}}))))

;(run-tests)