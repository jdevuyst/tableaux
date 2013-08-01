(ns tableaux.core-test
  (:require [clojure.test :refer :all]
            [tableaux.core :refer :all]))

(defn- is-valid [form doc]
  (is (valid? form) (str doc " - valid?"))
  (is (satisfiable? form)) (str doc " - satisfiable?"))

(deftest logic
  (testing "Basic tests"
    (is (satisfiable? :p))
    (is (satisfiable? [:and :p [:not [:box :a :p]]]))
    (is (= (count (construct-tableaux [:node0 [:not [:and [:and :p :q] [:and :r :s]]]]))
           4))
    (is (= (count (construct-tableaux [:node0 [:! [:! :p :q] :r]]))
           4))
    (is (= (count (construct-tableaux [:node0 [:and
                                               [:not [:! :p :r]]
                                               [:! :q :s]]]))
           4))
    (is (= (count (construct-tableaux [:node0 [:not [:box :a [:not [:and
                                                                    [:not [:! :p :r]]
                                                                    [:! :q :s]]]]]]))
           16))
    (is-valid [:not [:and [:not [:not :p]] [:not :p]]] "not elimination")
    (is-valid [:not [:and [:and :p :q] [:not :p]]] "and elimination")
    (is-valid [:not [:and [:and :p :q] [:not :q]]] "and elimination")
    (is-valid [:not [:and
                     [:and :p [:not [:and :p [:not :q]]]]
                     [:not :q]]] "modus ponens"))
  (testing "Hilbert axioms"
    (is-valid [:not [:and
                     :p
                     [:not :p]]] "P1")
    (is-valid [:not [:and
                     :p
                     [:and :q [:not :p]]]] "P2")
    (is-valid [:not [:and
                     [:not [:and :p [:and :q [:not :r]]]]
                     [:and
                      [:not [:and :p [:not :q]]]
                      [:and :p [:not :r]]]]] "P3")
    (is-valid [:not [:and
                     [:not [:and [:not :p] :q]]
                     [:and :q [:not :p]]]] "P4"))
  (testing "Modal axioms"
    (is-valid [:not [:and [:box :a [:not [:and :p [:not :q]]]]
                     [:and [:box :a :p] [:not [:box :a :q]]]]] "K")
    (is-valid [:not [:and
                     [:box :a :p]
                     [:not :p]]] "T")
    (is-valid [:not [:and
                     [:box :a :p]
                     [:not [:box :a [:box :a :p]]]]] "4")
    (is-valid [:not [:and
                     [:box :a :p]
                     [:box :a [:not :p]]]] "D")
    (is-valid [:not [:and
                     :p
                     [:not [:box :a [:not [:box :a [:not :p]]]]]]] "B")
    (is-valid [:not [:and
                     [:not [:box :a [:not :p]]]
                     [:not [:box :a [:not [:box :a [:not :p]]]]]]] "5")))

(run-tests)

; (explain [:not [:and [:and :p :q] [:and :r :s]]])
