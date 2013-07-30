(ns tableaux.loggers-test
  (:require [clojure.test :refer :all]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :refer :all]
            [tableaux.tableau-rules :refer :all]))

(deftest logger-tests
  (testing "Primary loggers"
    (is (= (log-by-arity [1 2 3])
           {[:by-arity 3] #{[1 2 3]}}))
    (is (= (log-labels-by-prefix [1 '(a b)])
           {[:labels-by-prefix 'a] #{[1 '(a b)]}}))
    (is (= (log-labels-by-prefix2 [1 '(a (b c))])
           {[:labels-by-prefix2 'a 'b] #{[1 '(a (b c))]}}))
    (is (= (log-labels-by-prefix2 [1 '(a b)])
           {[:labels-by-prefix2 'a nil] '#{(1 (a b))}}))
    (is (= (log-edges-by-idx-src [1 2 3])
           {[:edges-by-idx-src 1 2] #{[1 2 3]}}))
    (is (= (log-edges-by-idx-dest [1 2 3])
           {[:edges-by-idx-dest 1 3] #{[1 2 3]}}))))

(run-tests)
