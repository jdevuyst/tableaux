(ns dyntab.cascade-test
  (:require [clojure.test :refer :all]
            [dyntab.cascade :refer :all]
            [dyntab.syntax :refer :all]
            [dyntab.tableau :as tab]
            [dyntab.bag :as bag]
            [dyntab.util :as u]
            [clojure.core.reducers :as r]))

; TODO: find a good place for the function below
(defn summarize-cascades [cascades]
  (println "Number of branches" (count cascades))
  (println "Largest number of tableaux"
           (->> cascades
                (r/map #(count (bag/query % [:by-arity 1])))
                r/foldcat
                (apply max)))
  (println "Largest number of worlds"
           (->> cascades
                (r/mapcat #(->> (bag/query % [:by-arity 1])
                                (r/map (fn [[t]]
                                         (count (bag/query (newest-tableau % t)
                                                          [:by-arity 1]))))))
                r/foldcat
                (apply max)))
  cascades)

(defn- construct-tableau-cascades [form]
  (saturate [(tableau-cascade form)]))

(def t1 (tab/tableau :n :p))
(def t2 (-> (tab/tableau :n :r)
            (bag/post [[:m] [:a :n :m] [:m [:not :p]] [:m [:not :q]] [:n [:not :q]] [:m :r]])))
(def t3 (tab/tableau :n :s))
(def t4 (bag/post (tab/tableau :j :q)
                 [[:l] [:l :r] [:k] [:a :l :k] [:b :j :k] [:b :l :j]]))
(def t5 (bag/post (tab/tableau :k :q)
                 [[:l] [:a :k :l]]))
(def t6 (bag/post (bag/mark (tab/tableau :k :q) :synchronized)
                 [[:l]]))
(def t7 (bag/post (bag/mark (tab/tableau :l :q) :synchronized)
                 [[:k]]))

(deftest basic
  (testing "Basic cascade functionality"
    (testing "newest-tableau"
      (is (= (-> (bag/tuple-bag [bag/index-pairs-by-first])
                 (bag/post [[:t t5]])
                 (bag/post [[:t t6]])
                 (newest-tableau :t)))))
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
                           [t (bag/query tab [:by-arity 2])]))
                  (u/foldset))
             #{[:t #{[:start-node [:not [:not :p]]]
                     [:start-node :p]}]}))
      (is (= (->> [:t (tab/tableau [:not [:and :p :q]])]
                  (saturate-tableau nil)
                  first
                  (r/map (fn [[t tab]]
                           [t (bag/query tab [:by-arity 2])]))
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
                  (#(concat (bag/query % [:by-arity 1])
                            (bag/query % [:by-arity 2])
                            (bag/query % [:by-arity 3]))))
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
      (is (= (->> (prime nil [:t (bag/post (bag/mark (tab/tableau :p) :primed)
                                          [[:start-node [:! :p :q]]])])
                  first
                  (#(get % nil))
                  (r/filter #(= 2 (count %)))
                  r/foldcat
                  first
                  second
                  (#(concat (bag/query % [:by-arity 1])
                            (bag/query % [:by-arity 2])
                            (bag/query % [:by-arity 3]))))
             [[:start-node] [:start-node :q]]))
      (is (= (->> (prime nil [:t (bag/post (bag/mark (tab/tableau [:! :p :q]) :primed)
                                          [[:start-node :p]])])
                  first
                  (#(get % nil))
                  (r/filter #(= 2 (count %)))
                  r/foldcat
                  first
                  second
                  (#(concat (bag/query % [:by-arity 1])
                            (bag/query % [:by-arity 2])
                            (bag/query % [:by-arity 3]))))
             [[:start-node] [:start-node :q]]))
      (is (= (-> (tableau-cascade [:not [:! :p :q]])
                 (#(->> (bag/process %
                               :primed
                               [prime])
                        (apply meta-post)))
                 .history
                 (get [:by-arity 2]))
             (-> (tableau-cascade [:not [:! :p :q]])
                 (#(->> (bag/process %
                               :primed
                               [prime])
                        (apply meta-post)))
                 (#(->> (bag/process %
                               :primed
                               [prime])
                        (apply meta-post)))
                 .history
                 (get [:by-arity 2]))))
      (is (= (->> (tableau-cascade [:not [:! :p :q]])
                 (#(->> (bag/process %
                               :primed
                               [prime])
                        (apply meta-post)))
                  bag/.history
                  (some #{:primed}))
             :primed))
      (comment is (let [casc (->> (tableau-cascade [:not [:! :p :q]])
                                  (#(bag/process %
                                                (meta-dispatcher % :primed)
                                                :primed
                                                [prime])))]
                    (= (bag/.bags casc)
                       (bag/.bags (->> casc
                                      (#(bag/process %
                                                    (meta-dispatcher % :primed)
                                                    :primed
                                                    [prime]))))))))
    (testing "synchronization-range"
      (is (= (-> (bag/tuple-bag [bag/index-by-arity
                                       bag/index-pairs-by-first
                                       bag/index-triples-by-second
                                       bag/index-triples-by-third])
                 (bag/post #{[:t1 t1]
                            [:t2 t2]
                            [:t3 t3]
                            [:p :t1 :t3]
                            [:q :t3 :t2]})
                 (synchronization-range :t3))
             {:m #{:t2}, :n #{:t1 :t2}})))
    (testing "synchronize"
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t1 t1]
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
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t1 t1]
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
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t2 t2]
                                       [:t3 t3]
                                       [:r :t2 :t3]}))]
               (synchronize casc [:t2 t2]))
             [{:t2 #{[:n :s]}
               :t3 #{[:n] [:m] [:n :r]}}]))
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t4 t4]
                                       [:t5 t5]
                                       [:p :t4 :t5]}))]
               (synchronize casc [:t4 t4]))
             [{:t4 #{[:k :q] [:a :k :l]}
               :t5 #{[:l :r] [:a :l :k]}}]))
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t5 t5]
                                       [:t6 t6]
                                       [:q :t5 :t6]}))]
               (synchronize casc [:t6 t6]))
             [{:t5 #{[:l] [:l :q] [:k] [:k :q]}
               :t6 #{[:a :k :l] [:k :q]}}]))
      (is (= (let [casc (-> (bag/tuple-bag [bag/index-by-arity
                                                  bag/index-pairs-by-first
                                                  bag/index-triples-by-second
                                                  bag/index-triples-by-first-second
                                                  bag/index-triples-by-third])
                            (bag/post #{[:t5 t5]
                                       [:t7 t7]
                                       [:q :t5 :t7]}))]
               (synchronize casc [:t7 t7])))
          [{:t5 #{[:k]}
            :t7 #{[:a :k :l] [:k :q]}}])))
  (testing "Testing saturate"
    (is (= (count (construct-tableau-cascades [:not [:not :p]]))
           1))
    (is (= (-> (construct-tableau-cascades [:not [:not :p]])
               (first)
               (bag/newest [:pairs-by-first :start-tableau])
               second ; get tableau
               (bag/query [:by-arity 2]))
           #{[:start-node [:not [:not :p]]]
             [:start-node :p]}))
    (is (= (count (construct-tableau-cascades [:not [:and :p :q]]))
           2))))

(defn- is-valid [form doc]
  (is (valid? form) (str doc " - valid?"))
  (is (satisfiable? form) (str doc " - satisfiable?"))
  (is (valid? [:box :e form]) (str doc " - necessitation valid?")))

(defn- is-equiv [form1 form2 doc]
  (is-valid (implies form1 form2) (str doc "(LR)"))
  (is-valid (implies form2 form1) (str doc "(RL)")))

(deftest elementary-logic
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
    (is-valid [:not [:and :p [:not :p]]] "excluded middle")
    (is-valid (implies [:and (implies :p :q) :p]
                       :q) "modus ponens")
    (testing "Basic PAL tests"
      (is (satisfiable? [:! :p :q]))
      (is-valid [:! :p :p] "Atom invariance"))
    (testing "Moore sentence"
      (is (satisfiable? [:! [:and :p [:not [:box :a :p]]] [:box :a :p]])))))

(def A :p)
(def B :q)
(def C :r)

; stress test:
;(def A (implies :p :p))
;(def B (implies :p [:box :a (diamond :a :p)]))

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
                       [:box :a (diamond :a A)]) "5"))
  (testing "PAL reduction axioms"
    (is-equiv [:! A :t]
              (implies A :t)
              "RA1")
    (is-equiv [:! A [:not B]]
              (implies A
                       [:not [:! A B]])
              "RA2")
    (is-equiv [:! A [:and B C]]
              [:and [:! A B] [:! A C]]
              "RA3")
    (is-equiv [:! B [:box :a A]]
              (implies B
                       [:box :a (implies B
                                         [:! B A])])
              "RA4")
    (is-equiv [:! A [:! B C]]
              [:! [:and A B] C]
              "RA5")))

;(run-tests)
