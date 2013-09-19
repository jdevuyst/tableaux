(ns dyntab.tableau-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
            [dyntab.syntax :refer :all]
            [dyntab.bag :refer :all]
            [dyntab.tableau :refer :all]
            [dyntab.util :as u]
            [dyntab.syntax :as syntax]))

; TODO: give print-graph and explain a better place!

(defn print-graph
  [tb]
  (println "Nodes:")
  (doseq [x (query tb [:by-arity 1])] (println "-" x))
  (println "Edges:")
  (doseq [x (query tb [:by-arity 3])] (println "-" x))
  (println "Labels:")
  (doseq [x (query tb [:by-arity 2])] (println "-" x)))

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

(defn implies [x y]
  [:not [:and x [:not y]]])

(defn equiv [x y]
  [:and (implies x y) (implies y x)])

(defn diamond [idx form]
  [:not [:box idx [:not form]]])

;(defmacro defrules [fn-name tb-name queries & body]
;  (let [i (atom 0)]
;    `(do ~@(for [[q args] queries]
;             `(defn ~(symbol (str fn-name (swap! i inc)))
;                ([] ~q)
;                (~[tb-name args]
;                  (r/mapcat
;                    identity
;                    (for ~(->> (dissoc queries q)
;                               (r/mapcat
;                                 (fn [[k v]]
;                                   [v `(query ~tb-name ~k)]))
;                               (r/reduce conj []))
;                      (do ~@body)))))))))
;
;(defrules rule-box
;  tb
;  {[:pairs-by-first-of-second :box] [n [box1 idx1 form]]
;   [:by-arity 3] [idx2 src dest]}
;  [[dest form]])

(deftest tableau-rules
  (testing "Basic tableau rules"
    (is (= (index-pairs-by-first+firstsecond-of-second '[0 (:not (:not p))])
           [(rule-not-not)]))
    (is (= (u/foldset (rule-not-not nil [1 '(:not (:not p))]))
           #{[1 'p]}))
    (is (= (index-pairs-by-first-of-second '[0 (:and p q)])
           [(rule-and)]))
    (is (= (u/foldset (rule-and nil [1 '(:and p q)]))
           #{[1 'p] [1 'q]}))
    (is (= (index-pairs-by-first+firstsecond-of-second '[0 (:not (:and p q))])
           [(rule-not-and*)]))
    (is (= (count (rule-not-and*
                    nil
                    [1 '(:not (:and p q))]))
           1))
    (is (= (count (->> (rule-not-and*
                         nil
                         [1 '(:not (:and p q))])
                       first
                       (r/filter sequential?)
                       r/foldcat))
           2))
    (is (= (u/foldset (rule-not-and* nil [1 '(:not (:and p q))]))
           #{[[1 '(:not p)] [1 '(:not q)]]}))
    (is (= (index-pairs-by-first-of-second '[0 (:box p)])
           [(rule-box1)]))
    (is (= (-> (tuple-bag [index-triples-by-first-second])
               (post #{'(a 1 2) '(a 1 3)})
               (rule-box1 '(1 (:box a p)))
               u/foldset)
           #{[2 'p] [3 'p]}))
    (is (= (index-by-arity '[:a 0 1])
           [(rule-box2)]))
    (is (= (-> (tuple-bag [index-pairs-by-first+second-of-second])
               (post #{'(1 (:box a p)) [2]})
               (rule-box2 '(a 1 2))
               u/foldset)
           #{[2 'p]}))
    (is (= (index-pairs-by-first+firstsecond-of-second '[0 (:not (:box p))])
           [(rule-not-box)]))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (tuple-bag [])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (tuple-bag [index-triples-by-first-second
                                        index-by-arity])
                            (post ['[a 1 2]])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (tuple-bag [index-triples-by-first-second
                                        index-by-arity])
                            (post ['[3 [:not p]]])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (-> (tuple-bag [index-triples-by-first-second
                           index-by-arity])
               (post ['[a 1 2] '[2 [:not p]]])
               (rule-not-box [1 '(:not (:box a p))])
               u/foldset)
           #{})))
  (testing "Precondition rules"
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0] [1] [2]])
               (rule-precond1* [0 [:! :p]])
               u/foldset)
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0] [1] [2]])
               (rule-precond2* [0 [:! [:not :p]]])
               u/foldset)
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0]])
               (rule-precond3* [1])
               u/foldset)
           #{}))
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0 [:! :p :r]] [1 [:! :q :s]] [0] [1]])
               (rule-precond3* [3])
               u/foldset)
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]}))
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0]])
               (rule-precond4* [1])
               u/foldset)
           #{}))
    (is (= (-> (tuple-bag [index-by-arity
                           index-pairs-by-first-of-second
                           index-pairs-by-first+firstsecond-of-second])
               (post [[0 [:not [:! :p :r]]] [1 [:not [:! :q :s]]] [0] [1]])
               (rule-precond4* [3])
               u/foldset)
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]})))
  (testing "Closure rules"
    (is (= (with-redefs [syntax/Ind #{'a 'b}] (u/foldset (rule-T nil [1])))
           #{['a 1 1] ['b 1 1]}))
    (is (= (u/foldset (rule-B nil ['a 1 2]))
           #{['a 2 1]}))
    (is (= (-> (tuple-bag [index-triples-by-first-second
                           index-triples-by-first-third])
               (post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (rule-4 ['a 4 5])
               u/foldset)
           #{['a 1 5]}))
    (is (= (-> (tuple-bag [index-triples-by-first-second
                           index-triples-by-first-third])
               (post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (rule-4 ['a 1 4])
               u/foldset)
           #{['a 1 5]}))))

(deftest branching
  (testing "Branching"
    (is (= (->> (-> (->TupleBag [(constantly [:k])]
                                {}
                                {}
                                (list {:a #{[1 2] ['x 'y] [:A :B]} :b #{4 5}}))
                    (process :bogus-marker
                             [(fn
                                ([] :a)
                                ([tb x] [x]))]))
                (apply disjunctive-post)
                (r/map #(query % :k))
                (r/fold (r/monoid conj hash-set)))
           (u/foldset (u/one-from-each [[1 2] ['x 'y] [:A :B]]))))))

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