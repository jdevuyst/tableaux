(ns tableaux.cascade-test
  (:require [clojure.test :refer :all]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :refer :all]
            [tableaux.cascade :refer :all]
            [tableaux.syntax :as syntax]))

(deftest cascade
  (testing "Saturate tableaux"
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:not p))])
                       (rule-not-not)))
           0))
    (is (= (rule-not-not nil [1 '(:not (:not p))])
           #{[1 'p]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:and p q)])
                       (rule-and)))
           0))
    (is (= (rule-and nil [1 '(:and p q)])
           #{[1 'p] [1 'q]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:and p q))])
                       (fork-rule-not-and)))
           0))
    (is (= (fork-rule-not-and nil [1 '(:not (:and p q))])
           #{[[1 '(:not p)] [1 '(:not q)]]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:box p)])
                       (rule-box1)))
           0))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src])
               (rw/post #{'(a 1 2) '(a 1 3)})
               (rule-box1 '(1 (:box a p)))
               set)
           #{[2 'p] [3 'p]}))
    (is (> (count (get (log-by-arity '[:a 0 1])
                       (rule-box2)))
           0))
    (is (= (-> (rw/rewriting-system [log-labels-by-node-prefix-infix])
               (rw/post #{'(1 (:box a p)) [2]})
               (rule-box2 '(a 1 2))
               set)
           #{[2 'p]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:box p))])
                       (rule-not-box)))
           0))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [])
                            (rule-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[a 1 2]])
                            (rule-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[3 [:not p]]])
                            (rule-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-by-arity])
               (rw/post ['[a 1 2] '[2 [:not p]]])
               (rule-not-box [1 '(:not (:box a p))]))
           #{})))
  (testing "Precondition rules"
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0] [1] [2]])
               (fork-rule-precond1 [0 [:! :p]]))
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0] [1] [2]])
               (fork-rule-precond2 [0 [:! [:not :p]]]))
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [0])
               (fork-rule-precond3 [1]))
           #{}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0 [:! :p :r]] [1 [:! :q :s]] [0] [1]])
               (fork-rule-precond3 [3]))
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [0])
               (fork-rule-precond4 [1]))
           #{}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0 [:not [:! :p :r]]] [1 [:not [:! :q :s]]] [0] [1]])
               (fork-rule-precond4 [3]))
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]})))

(run-tests)
