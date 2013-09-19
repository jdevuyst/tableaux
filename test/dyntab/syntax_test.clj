(ns dyntab.syntax-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
            [dyntab.syntax :refer :all]))

(deftest syntax-tests
  (testing "wwf?"
    (is (wff? :p))
    (is (wff? [:not :p]))
    (is (not (wff? [:not :a])))
    (is (wff? [:and :p :q]))
    (is (not (wff? [:and [] :q])))
    (is (wff? [:box :a :p]))
    (is (not (wff? [:box :q :p])))
    (is (wff? [:not [:and :p :q]]))
    (is (not (wff? [:and :p [:not :box :p]])))))

;(run-tests)