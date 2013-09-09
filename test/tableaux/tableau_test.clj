(ns tableaux.tableau-test
  (:require [clojure.test :refer :all]
            [tableaux.syntax :refer :all]
            [clojure.core.reducers :as r]
            [tableaux.rewrite :as rw]
            [tableaux.tableau :refer :all]
            [tableaux.util :as u]))

; TODO: give print-graph and explain a better place!

(defn print-graph
  [rs]
  (println "Nodes:")
  (doseq [x (rw/query rs [:by-arity 1])] (println "-" x))
  (println "Edges:")
  (doseq [x (rw/query rs [:by-arity 3])] (println "-" x))
  (println "Labels:")
  (doseq [x (rw/query rs [:by-arity 2])] (println "-" x)))

(defn- construct-tableaux
[[start-node form]]
(second (saturate [(tableau start-node form)])))

(defn- satisfiable?
  [form]
  (->> (construct-tableaux [:start-node form])
       (r/filter consistent?)
       (u/fold-empty?)
       (not)))

(defn- valid?
  [form]
  (not (satisfiable? [:not form])))

(defn explain
[form]
(print (str "Constructing tableaux for " form "..."))
(let [open-tableaux (->> (construct-tableaux [:start-node form])
                         (r/filter consistent?)
                         (r/foldcat))
      sat (not (empty? open-tableaux))
      valid (valid? form)]
  (println " done.")
  (if sat
    (do
      (println "The formula is"
               (if valid
                 "valid."
                 "satisfiable but not valid."))
      (when (not valid)
        (if (= 1 (count open-tableaux))
          (println "One open tableau was constructed.")
          (println (count open-tableaux)
                   "open tableaux were constructed. Here's one."))
        (print-graph (first open-tableaux))
        (consistent? (first open-tableaux))))
    (println "The formula is not satisfiable."
             "Therefore, its negation is valid."))
  (first open-tableaux)))

(defn- is-valid [form doc]
  (is (satisfiable? form) (str doc " - satisfiable?"))
  (is (valid? form) (str doc " - valid?"))
  (is (valid? [:box :e form]) (str doc " - necessitation valid?")))

(deftest elementary-logic
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
    (is-valid [:not [:and :p [:not :p]]] "excluded middle")
    (is-valid (implies [:and (implies :p :q) :p]
                       :q) "modus ponens")))

(def A :p)
(def B :q)
(def C :r)

(deftest axioms
  (testing "Hilbert axioms"
    (is-valid (implies A A) "P1")
    (is-valid (implies A
                       (implies B A)) "P2")
    (is-valid (implies (implies A
                                (implies B C))
                       (implies (implies A B)
                                (implies A C))) "P3")
    (is-valid (implies (implies [:not A]
                                [:not B])
                       (implies B A)) "P4"))
  (testing "Modal axioms"
    (is-valid (implies [:box :a (implies A B)]
                       (implies [:box :a A]
                                [:box :a B])) "K")
    (is-valid (implies [:box :a A] A) "T")
    (is-valid (implies [:box :a A]
                       [:box :a [:box :a A]]) "4")
    (is-valid (implies [:box :a A]
                       (diamond :a A)) "D")
    (is-valid (implies A [:box :a (diamond :a A)]) "B")
    (is-valid (implies (diamond :a A)
                       [:box :a (diamond :a A)]) "5")))

;(run-tests)