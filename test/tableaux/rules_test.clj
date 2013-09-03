(ns tableaux.rules-test
  (:require [clojure.test :refer :all]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :refer :all]
            [tableaux.rules :refer :all]
            [tableaux.util :as u]
            [clojure.core.reducers :as r]
            [tableaux.syntax :as syntax]))

(deftest tableau-rules
  (testing "Basic tableau rules"
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:not p))])
                       (rule-not-not)))
           0))
    (is (= (u/foldset (rule-not-not nil [1 '(:not (:not p))]))
           #{[1 'p]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:and p q)])
                       (rule-and)))
           0))
    (is (= (u/foldset (rule-and nil [1 '(:and p q)]))
           #{[1 'p] [1 'q]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:and p q))])
                       (fork-rule-not-and)))
           0))
    (is (= (count (fork-rule-not-and
                    nil
                    [1 '(:not (:and p q))]))
           1))
    (is (= (count (->> (fork-rule-not-and
                         nil
                         [1 '(:not (:and p q))])
                       first
                       (r/filter sequential?)
                       r/foldcat))
           2))
    (is (= (u/foldset (fork-rule-not-and nil [1 '(:not (:and p q))]))
           #{[[1 '(:not p)] [1 '(:not q)]]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:box p)])
                       (rule-box1)))
           0))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src])
               (rw/post #{'(a 1 2) '(a 1 3)})
               (rule-box1 '(1 (:box a p)))
               u/foldset)
           #{[2 'p] [3 'p]}))
    (is (> (count (get (log-by-arity '[:a 0 1])
                       (rule-box2)))
           0))
    (is (= (-> (rw/rewriting-system [log-labels-by-node-prefix-infix])
               (rw/post #{'(1 (:box a p)) [2]})
               (rule-box2 '(a 1 2))
               u/foldset)
           #{[2 'p]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:box p))])
                       (rule-not-box)))
           0))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (rw/rewriting-system [])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[a 1 2]])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [x] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[3 [:not p]]])
                            (rule-not-box [1 '(:not (:box a p))])
                            u/foldset))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-by-arity])
               (rw/post ['[a 1 2] '[2 [:not p]]])
               (rule-not-box [1 '(:not (:box a p))])
               u/foldset)
           #{})))
  (testing "Precondition rules"
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0] [1] [2]])
               (fork-rule-precond1 [0 [:! :p]])
               u/foldset)
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0] [1] [2]])
               (fork-rule-precond2 [0 [:! [:not :p]]])
               u/foldset)
           (into #{} (for [i [0 1 2]] [[i :p] [i [:not :p]]]))))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [0])
               (fork-rule-precond3 [1])
               u/foldset)
           #{}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0 [:! :p :r]] [1 [:! :q :s]] [0] [1]])
               (fork-rule-precond3 [3])
               u/foldset)
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [0])
               (fork-rule-precond4 [1])
               u/foldset)
           #{}))
    (is (= (-> (rw/rewriting-system [log-by-arity
                                     log-labels-by-prefix
                                     log-labels-by-prefix2])
               (rw/post [[0 [:not [:! :p :r]]] [1 [:not [:! :q :s]]] [0] [1]])
               (fork-rule-precond4 [3])
               u/foldset)
           #{[[3 :p] [3 [:not :p]]]
             [[3 :q] [3 [:not :q]]]})))
  (testing "Closure rules"
    (is (= (with-redefs [syntax/Ind #{'a 'b}] (u/foldset (rule-T nil [1])))
           #{['a 1 1] ['b 1 1]}))
    (is (= (u/foldset (rule-B nil ['a 1 2]))
           #{['a 2 1]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-edges-by-idx-dest])
               (rw/post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (rule-4 ['a 4 5])
               u/foldset)
           #{['a 1 5]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-edges-by-idx-dest])
               (rw/post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (rule-4 ['a 1 4])
               u/foldset)
           #{['a 1 5]}))))

(run-tests)
