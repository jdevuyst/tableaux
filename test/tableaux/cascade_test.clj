(ns tableaux.cascade-test
  (:require [clojure.test :refer :all]
            [tableaux.cascade :refer :all]
            [tableaux.tableau :as tab]
            [tableaux.rewrite :as rw]
            [tableaux.util :as u]
            [tableaux.loggers :as log]
            [tableaux.syntax :as syntax]
            [clojure.core.reducers :as r]))

(def t1 (tab/tableau :n :p))
(def t2 (-> (tab/tableau :n :r)
            (rw/post [[:m] [:a :n :m] [:m [:not :p]] [:m [:not :q]] [:n [:not :q]] [:m :r]])))
(def t3 (tab/tableau :n :s))
(def t4 (rw/post (tab/tableau :j :q)
                 [[:l] [:l :r] [:k] [:a :l :k] [:b :j :k] [:b :l :j]]))
(def t5 (rw/post (tab/tableau :k :q)
                 [[:l] [:a :k :l]]))
(def t6 (rw/post (rw/mark (tab/tableau :k :q) :synchronized)
                 [[:l]]))
(def t7 (rw/post (rw/mark (tab/tableau :l :q) :synchronized)
                 [[:k]]))

(deftest basic
  (testing "Basic cascade functionality"
    (testing "current-tab"
      (is (= (-> (rw/rewriting-system [log/log-labels-by-node])
                 (rw/post [[:t 50]])
                 (rw/post [[:t 100]])
                 (current-tab :t))
             100)))
    (testing "tableau-saturate"
      (is (= (count (saturate-tableau
                      nil
                      [:t (tab/tableau [:not [:not :p]])]))
             1))
      (is (= (count (saturate-tableau
                      nil
                      [:t (tab/tableau [:not [:and :p :q]])]))
             1))
      (is (= (count (->> (saturate-tableau
                           nil
                           [:t (tab/tableau [:not [:and :p :q]])])
                         first
                         (r/filter sequential?)
                         r/foldcat))
             2))
      (is (= (count (->> (saturate-tableau
                           nil
                           [:t (tab/tableau [:not [:and :p :q]])])
                         first
                         (r/mapcat identity)
                         (r/filter sequential?)
                         r/foldcat))
             0))
      (is (= (->> [:t (tab/tableau [:not [:not :p]])]
                  (saturate-tableau nil)
                  first
                  (r/map (fn [[t tab]]
                           [t (rw/query tab [:by-arity 2])]))
                  (u/foldset))
             #{[:t #{[:start-node [:not [:not :p]]]
                     [:start-node :p]}]}))
      (is (= (->> [:t (tab/tableau [:not [:and :p :q]])]
                  (saturate-tableau nil)
                  first
                  (r/map (fn [[t tab]]
                           [t (rw/query tab [:by-arity 2])]))
                  (u/foldset))
             #{[:t #{[:start-node [:not [:and :p :q]]]
                     [:start-node [:not :p]]}]
               [:t #{[:start-node [:not [:and :p :q]]]
                     [:start-node [:not :q]]}]})))
    (testing "Priming"
      (is (= (count (prime nil [:t (tab/tableau [:not [:! :p :q]])]))
             1))
      (is (= (->> (prime nil [:t (tab/tableau [:not [:! :p :q]])])
                  first
                  (#(get % nil))
                  (r/filter #(= 2 (count %)))
                  r/foldcat
                  first
                  second
                  (#(concat (rw/query % [:by-arity 1])
                            (rw/query % [:by-arity 2])
                            (rw/query % [:by-arity 3]))))
             [[:start-node] [:start-node [:not :q]]]))
      (is (= (with-redefs
               [gensym (constantly 500)]
               (->> (prime nil [:t (tab/tableau [:not [:! :p :q]])])
                    first
                    (#(get % nil))
                    (r/filter #(contains? #{1 3} (count %)))
                    u/foldset))
             #{[500] [:p :t 500]}))
      (is (= (with-redefs
               [gensym (constantly 500)]
               (-> (prime nil [:t (tab/tableau [:not [:! :p :q]])])
                   first
                   ;(println)
                   (get :t)
                   ;(r/filter #(contains? #{1 3} (count %)))
                   u/foldset))
             #{[:start-node [:not [:! :p :q]]]
               [:start-node :p]}))
      (is (= (empty? (first (prime nil [:t (tab/tableau [:! :p :q])])))))
      (is (= (->> (prime nil [:t (rw/post (rw/mark (tab/tableau :p) :primed)
                                          [[:start-node [:! :p :q]]])])
                  first
                  (#(get % nil))
                  (r/filter #(= 2 (count %)))
                  r/foldcat
                  first
                  second
                  (#(concat (rw/query % [:by-arity 1])
                            (rw/query % [:by-arity 2])
                            (rw/query % [:by-arity 3]))))
             [[:start-node] [:start-node :q]]))
      (is (= (->> (prime nil [:t (rw/post (rw/mark (tab/tableau [:! :p :q]) :primed)
                                          [[:start-node :p]])])
                  first
                  (#(get % nil))
                  (r/filter #(= 2 (count %)))
                  r/foldcat
                  first
                  second
                  (#(concat (rw/query % [:by-arity 1])
                            (rw/query % [:by-arity 2])
                            (rw/query % [:by-arity 3]))))
             [[:start-node] [:start-node :q]]))
      (is (= (-> (tableau-cascade [:not [:! :p :q]])
                 (#(rw/process %
                               (meta-dispatcher % :primed)
                               :primed
                               [prime]))
                 .history
                 (get [:by-arity 2]))
             (-> (tableau-cascade [:not [:! :p :q]])
                 (#(rw/process %
                               (meta-dispatcher % :primed)
                               :primed
                               [prime]))
                 (#(rw/process %
                               (meta-dispatcher % :primed)
                               :primed
                               [prime]))
                 .history
                 (get [:by-arity 2]))))
      (is (= (->> (tableau-cascade [:not [:! :p :q]])
                  (#(rw/process %
                                (meta-dispatcher % :primed)
                                :primed
                                [prime]))
                  rw/.history
                  (some #{:primed}))
             :primed))
      (comment is (let [casc (->> (tableau-cascade [:not [:! :p :q]])
                          (#(rw/process %
                                        (meta-dispatcher % :primed)
                                        :primed
                                        [prime])))]
            (= (rw/.logs casc)
               (rw/.logs (->> casc
                              (#(rw/process %
                                            (meta-dispatcher % :primed)
                                            :primed
                                            [prime]))))))))
    (testing "synchronization-range"
      (is (= (-> (rw/rewriting-system [log/log-by-arity
                                       log/log-labels-by-node
                                       log/log-edges-by-src
                                       log/log-edges-by-dest])
                 (rw/post #{[:t1 t1]
                            [:t2 t2]
                            [:t3 t3]
                            [:p :t1 :t3]
                            [:q :t3 :t2]})
                 (synchronization-range :t3))
             {:m #{:t2}, :n #{:t1 :t2}})))
    (testing "synchronize"
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t1 t1]
                                       [:t2 t2]
                                       [:t3 t3]
                                       [:t4 t4]
                                       [:t5 t5]
                                       [:p :t1 :t3]
                                       [:s :t3 :t2]
                                       [[:and [:not :r] :r] :t4 :t3]
                                       [[:and [:not :r] :r] :t3 :t5]}))]
               (synchronize casc [:t3 t3]))
             [{:t1 #{[:n] [:n :s] [:n :p]}
               :t4 #{[:n] [:n [:and [:not :r] :r]]}
               :t2 #{[:n] [:n :s]}
               :t3 #{[:n :p] [:n :r]}}]))
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t1 t1]
                                       [:t2 t2]
                                       [:t3 t3]
                                       [:t4 t4]
                                       [:t5 t5]
                                       [:p :t1 :t3]
                                       [:s :t3 :t2]
                                       [:p :t4 :t3]
                                       [[:and [:not :r] :r] :t3 :t5]}))]
               (synchronize casc [:t2 t2]))
             [{:t2 #{[:n :s]}
               :t3 #{[:n] [:m] [:n :r] [:m :s] [:n :s]}}]))
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t2 t2]
                                       [:t3 t3]
                                       [:r :t2 :t3]}))]
               (synchronize casc [:t2 t2]))
             [{:t2 #{[:n :s]}
               :t3 #{[:n] [:m] [:n :r]}}]))
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t4 t4]
                                       [:t5 t5]
                                       [:p :t4 :t5]}))]
               (synchronize casc [:t4 t4]))
             [{:t4 #{[:k :q] [:a :k :l]}
               :t5 #{[:l :r] [:a :l :k]}}]))
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t5 t5]
                                       [:t6 t6]
                                       [:q :t5 :t6]}))]
               (synchronize casc [:t6 t6]))
             [{:t5 #{[:l] [:l :q] [:k] [:k :q]}
               :t6 #{[:a :k :l] [:k :q]}}]))
      (is (= (let [casc (-> (rw/rewriting-system [log/log-by-arity
                                                  log/log-labels-by-node
                                                  log/log-edges-by-src
                                                  log/log-edges-by-idx-src
                                                  log/log-edges-by-dest])
                            (rw/post #{[:t5 t5]
                                       [:t7 t7]
                                       [:q :t5 :t7]}))]
               (synchronize casc [:t7 t7])))
          [{:t5 #{[:k]}
            :t7 #{[:a :k :l] [:k :q]}}])))
  ;    (testing "Synchronize backwards"
  ;      (is (= (let [[[t tab]] (synchronize-backward
  ;                               (-> (rw/rewriting-system [log/log-by-arity
  ;                                                         log/log-labels-by-node
  ;                                                         log/log-edges-by-src
  ;                                                         log/log-edges-by-dest
  ;                                                         log/log-edges-by-idx-src])
  ;                                   (rw/post #{[:t1 t1]
  ;                                              [:t2 t2]
  ;                                              [:t3 t3]
  ;                                              [:p :t1 :t2]
  ;                                              [:q :t1 :t3]}))
  ;                               [:t1 t1])]
  ;               (apply hash-set (concat (rw/query tab [:by-arity 1])
  ;                                       (rw/query tab [:by-arity 2])
  ;                                       (rw/query tab [:by-arity 3]))))
  ;             #{[:m] [:n] [:m :p] [:n :p] [:n :q] [:n :s] [:n :r] [:m :r] [:a :n :m]})))
  ;    (testing "Synchronize forwards"
  ;      (is (= (let [[[t tab]] (synchronize-forward
  ;                               (-> (rw/rewriting-system [log/log-by-arity
  ;                                                         log/log-labels-by-node
  ;                                                         log/log-edges-by-src
  ;                                                         log/log-edges-by-dest])
  ;                                   (rw/post #{[:t1 t1]
  ;                                              [:t2 t2]
  ;                                              [:t3 t3]
  ;                                              [:p :t1 :t2]
  ;                                              [:q :t1 :t3]
  ;                                              [[:not :q] :t2 :t3]}))
  ;                               [:t3 t3])]
  ;               (apply hash-set (concat (rw/query tab [:by-arity 1])
  ;                                       (rw/query tab [:by-arity 2])
  ;                                       (rw/query tab [:by-arity 3]))))
  ;             #{[:a :n :m] [:n :s] [:n :r] [:m :r] [:m] [:n]}))))
  (testing "Testing saturate"
    (is (= (count (construct-tableau-cascades [:not [:not :p]]))
           1))
    (is (= (-> (construct-tableau-cascades [:not [:not :p]])
               (first)
               (rw/newest [:labels-by-node :start-tableau])
               second ; get tableau
               (rw/query [:by-arity 2]))
           #{[:start-node [:not [:not :p]]]
             [:start-node :p]}))
    (is (= (count (construct-tableau-cascades [:not [:and :p :q]]))
           2))))

(defn- is-valid [form doc]
  (is (valid? form) (str doc " - valid?"))
  (is (satisfiable? form)) (str doc " - satisfiable?"))

(deftest logic
  (testing "Basic logic tests"
    (is (satisfiable? :p))
    (is (satisfiable? [:and :p [:not [:box :a :p]]]))
    (is (= (count (construct-tableau-cascades
                    [:not [:and
                           [:and :p :q]
                           [:and :r :s]]]))
           4))
    (is (= (count (construct-tableau-cascades
                    [:! :q :s]))
           2))
    (is (= (count (construct-tableau-cascades
                    [:! [:! :p :q] :r]))
           4))
    (is (= (count (construct-tableau-cascades
                    [:and
                     [:not [:! :p :r]]
                     [:! :q :s]]))
           4))
    (is (= (count (construct-tableau-cascades
                    [:not [:box :a [:not [:and
                                          [:not [:! :p :r]]
                                          [:! :q :s]]]]]))
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
    (is-valid [:not [:and
                     [:box :a [:not [:and :p [:not :q]]]]
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
                     [:not [:box :a [:not [:box :a [:not :p]]]]]]] "5"))
  (testing "Basic PAL tests"
    (is (satisfiable? [:! :p :q]))
    (is-valid [:! :p :p] "Atom invariance"))
  (testing "PAL reduction axioms"
    (is-valid (syntax/iff [:! :p :q]
                          (syntax/implies :p :q)) "RA1")
    (is-valid (syntax/iff [:! :p [:not :q]]
                          (syntax/implies :p [:not [:! :p :q]])) "RA2")
    (is-valid (syntax/iff [:! :p [:and :q :r]]
                          [:and [:! :p :q] [:! :p :r]]) "RA3")
    (is-valid (syntax/iff [:! :q [:box :a :p]]
                          (syntax/implies :q [:box :a (syntax/implies :q [:! :q :p])])) "RA4")
    (is-valid (syntax/iff [:! :p [:! :q :r]]
                          [:! [:and :p :q] :r]) "RA5")
    ))

(run-tests)

; (explain [:not [:and [:and :p :q] [:and :r :s]]])
