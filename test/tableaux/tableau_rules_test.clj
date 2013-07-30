(ns tableaux.tableau-rules-test
  (:require [clojure.test :refer :all]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :refer :all]
            [tableaux.tableau-rules :refer :all]
            [tableaux.syntax :as syntax]))

(deftest tableau-rules
  (testing "Basic tableau rules"
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:not p))])
                       (process-not-not)))
           0))
    (is (= (process-not-not nil [1 '(:not (:not p))])
           #{[1 'p]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:and p q)])
                       (process-and)))
           0))
    (is (= (process-and nil [1 '(:and p q)])
           #{[1 'p] [1 'q]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:and p q))])
                       (fork-process-not-and)))
           0))
    (is (= (fork-process-not-and nil [1 '(:not (:and p q))])
           #{[[1 '(:not p)] [1 '(:not q)]]}))
    (is (> (count (get (log-labels-by-prefix '[0 (:box p)])
                       (process-box1)))
           0))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src])
               (rw/post #{'(a 1 2) '(a 1 3)})
               (process-box1 '(1 (:box a p)))
               set)
           #{[2 'p] [3 'p]}))
    (is (> (count (get (log-by-arity '[:a 0 1])
                       (process-box2)))
           0))
    (is (= (-> (rw/rewriting-system [log-labels-by-node-prefix-infix])
               (rw/post #{'(1 (:box a p)) [2]})
               (process-box2 '(a 1 2))
               set)
           #{[2 'p]}))
    (is (> (count (get (log-labels-by-prefix2 '[0 (:not (:box p))])
                       (process-not-box)))
           0))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [])
                            (process-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[a 1 2]])
                            (process-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (with-redefs [gensym (fn [] 2)]
                        (-> (rw/rewriting-system [log-edges-by-idx-src
                                                  log-by-arity])
                            (rw/post ['[3 [:not p]]])
                            (process-not-box [1 '(:not (:box a p))])))
           #{[2] ['a 1 2] [2 [:not 'p]]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-by-arity])
               (rw/post ['[a 1 2] '[2 [:not p]]])
               (process-not-box [1 '(:not (:box a p))]))
           #{})))
  (testing "Closure rules"
    (is (= (with-redefs [syntax/Ind #{'a 'b}] (set (process-T nil [1])))
           #{['a 1 1] ['b 1 1]}))
    (is (= (set (process-B nil ['a 1 2]))
           #{['a 2 1]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-edges-by-idx-dest])
               (rw/post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (process-4 ['a 4 5])
               set)
           #{['a 1 5]}))
    (is (= (-> (rw/rewriting-system [log-edges-by-idx-src
                                     log-edges-by-idx-dest])
               (rw/post [['a 1 2] ['a 2 3] ['a 1 4] ['a 4 5]])
               (process-4 ['a 1 4])
               set)
           #{['a 1 5]}))))

(run-tests)
