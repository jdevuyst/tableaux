(ns tableaux.cascade
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [tableaux.util :as u]
            [tableaux.bag :as bag]
            [tableaux.syntax :as syntax]
            [tableaux.tableau :as tab]))

(defn tableau-cascade [form]
  (bag/post (bag/tuple-bag [bag/index-by-arity
                            bag/index-pairs-by-first
                            bag/index-triples-by-first-second
                            bag/index-triples-by-second
                            bag/index-triples-by-third])
            [[:start-tableau]
             [:start-tableau (tab/tableau form)]]))

(defn newest-tableau [casc t]
  (-> (bag/newest casc [:pairs-by-first t])
      (second)))

(defn saturate-tableau
  ([] [:by-arity 2])
  ([casc [t tab]]
   (->> [tab]
        (tab/saturate)
        ((fn [[changed tableaux]]
           (when changed
             (r/map (fn [new-tab] [t new-tab])
                    tableaux))))
        (r/foldcat)
        (vector))))

(defn meta-process [tab rules]
  [(r/fold 
     u/mmap-merge
     (second (bag/process tab :meta-post-mark rules)))])

(defn meta-post [casc tables]
  (->> (apply u/mmap-merge tables)
       (r/mapcat (fn [t coll]
                   (if t
                     [[t (-> (newest-tableau casc t)
                             (bag/mark :meta-post-mark)
                             (bag/post coll))]]
                     coll)))
       (r/foldcat)
       (bag/post casc)))

(defn prime
  ([] [:by-arity 2])
  ([casc [t tab]]
   (meta-process
     tab
     [(fn
        ([] [:pairs-by-first-of-second :!])
        ([cur-tab [n [op form1 form2] :as node-label]]
         (let [new-t (gensym "tableau")]
           (if (-> (bag/query cur-tab [:by-arity 2])
                   (contains? [n form1]))
             [{nil #{[new-t]
                     [new-t (tab/tableau n form2)]
                     [form1 t new-t]}
               t #{node-label}}]
             []))))
      (fn
        ([] [:by-arity 2])
        ([cur-tab [n form :as node-label]]
         [{nil (->> (bag/query
                      cur-tab
                      [:pairs-by-first+second-of-second 
                       n :! form])
                    (r/mapcat 
                      (fn [[n2 [op form2 form3]]]
                        (let [new-t (gensym "tableau")]
                          [[new-t]
                           [new-t (tab/tableau n form3)]
                           [form t new-t]])))
                    (u/foldset))
           t #{node-label}}]))
      (fn
        ([] [:pairs-by-first+second-of-second :not :!])
        ([cur-tab [n [op1 [op2 form1 form2]] :as node-label]]
         (let [new-t (gensym "tableau")]
           [{nil #{[new-t]
                   [new-t (tab/tableau n [:not form2])]
                   [form1 t new-t]}
             t #{node-label}}
            {t #{[n form1]}}])))])))

(defn synchronization-range [casc t]
  (->> (bag/query casc [:triples-by-second t])
       (r/map #(nth % 2))
       (r/foldcat)
       (r/cat (->> (bag/query casc [:triples-by-third t])
                   (r/map second)
                   (r/foldcat)))
       (r/mapcat (fn [t2] 
                   (->> (newest-tableau casc t2)
                        (#(bag/query % [:by-arity 1]))
                        (r/map (fn [[n]] {n #{t2}})))))
       (r/fold u/mmap-merge)))

(defn synchronize-init
  ([] [:by-arity 3])
  ([casc [precond t1 t2]]
   [(r/fold u/mmap-merge
            [(->> (bag/query (newest-tableau casc t1)
                             [:pairs-by-second precond])
                  (r/map (fn [[n form]] {t2 #{[n]}}))
                  (r/fold u/mmap-merge))])]))

(defn synchronize
  ([] [:by-arity 2])
  ([casc [t tab]]
   (let [sync-range (synchronization-range casc t)]
     (meta-process
       tab
       [(fn ; copy node to left-hand tableaux
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (bag/query casc [:triples-by-third t])
                (r/map (fn [[precond t1 t2]]
                         {t1 #{[n]
                               [n precond]}})))))
        (fn ; copy node to right-hand tableaux
          ([] [:by-arity 2])
          ([cur-tab [n form]]
           (->> (bag/query casc
                           [:triples-by-first-second form t])
                (r/map (fn [[precond t1 t2]]
                         {t2 #{[n]}})))))
        (fn ; copy atoms to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(bag/newest casc
                                    [:pairs-by-first %]))
                (r/mapcat 
                  #(bag/query
                     (second %)
                     [:pairs-by-first-of-second nil]))
                (r/filter #(= (first %) n))
                (r/map (fn [x] {t #{x}})))))
        (fn ; copy atoms to neighboring tableaux
          ([] [:pairs-by-first-of-second nil])
          ([cur-tab [n atom]]
           (r/map (fn [t2] {t2 #{[n atom]}})
                  (get sync-range n))))
        (fn ; copy incoming edges to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(bag/newest casc
                                    [:pairs-by-first %]))
                (r/mapcat #(bag/query (second %)
                                      [:triples-by-third n]))
                (r/filter
                  #(contains? (bag/query cur-tab
                                         [:by-arity 1])
                              [(second %)]))
                (r/map (fn [x] {t #{x}})))))
        (fn ; copy outgoing edges to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(bag/newest casc
                                    [:pairs-by-first %]))
                (r/mapcat
                  #(bag/query (second %)
                              [:triples-by-second n]))
                (r/filter
                  #(contains? (bag/query cur-tab
                                         [:by-arity 1])
                              [(nth % 2)]))
                (r/map (fn [x] {t #{x}})))))
        (fn ; copy edges to neighboring tableaux
          ([] [:by-arity 3])
          ([cur-tab [idx n m]]
           (r/map (fn [t2] {t2 #{[idx n m]}})
                  (set/intersection
                    (get sync-range n)
                    (get sync-range m)))))]))))

(defn saturate [cascades]
  (let [next-casc
        (->> cascades
             (r/map #(bag/process
                       %
                       :casc-sat-mark
                       [saturate-tableau]))
             (r/mapcat (partial apply tab/disjunctive-post))
             (r/map #(bag/process
                       %
                       :casc-sat-mark2
                       [prime
                        synchronize
                        synchronize-init]))
             (r/map (partial apply meta-post))
             (r/fold 8 (r/monoid r/cat vector) conj))]
    (if-not (->> next-casc
                 (r/mapcat #(bag/since % :casc-sat-mark))
                 (u/fold-empty?))
      (recur next-casc)
      next-casc)))

(defn consistent? [casc]
  (->> (bag/query casc [:by-arity 1])
       (r/map #(bag/newest casc [:pairs-by-first (first %)]))
       (r/filter #(not (tab/consistent? (second %))))
       (u/fold-empty?)))

(defn satisfiable? [form]
  (->> (saturate [(tableau-cascade form)])
       (r/filter consistent?)
       (u/fold-empty?)
       (not)))

(defn valid? [form]
  (not (satisfiable? [:not form])))

