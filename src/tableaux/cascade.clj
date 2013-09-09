(ns tableaux.cascade
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :as log]
            [tableaux.syntax :as syntax]
            [tableaux.util :as u]
            [tableaux.tableau :as tab]))

(defn tableau-cascade [form]
  (rw/post (rw/logging-system [log/log-by-arity
                                 log/log-labels-by-node
                                 log/log-edges-by-idx-src
                                 log/log-edges-by-src
                                 log/log-edges-by-dest])
           [[:start-tableau]
            [:start-tableau (tab/tableau :start-node form)]]))

(defn newest-tableau [casc t]
  (second (rw/newest casc [:labels-by-node t])))

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

(defn prime
  ([] [:by-arity 2])
  ([casc [t tab]]
   (rw/process
     tab
     (fn [new-tab updates]
       [(r/fold u/mmap-merge updates)])
     :prime-sync-mark
     [(fn
        ([] [:labels-by-prefix :!])
        ([cur-tab [n [op form1 form2] :as node-label]]
         (let [new-t (gensym "tableau")]
           (if (-> (rw/query cur-tab [:by-arity 2])
                   (contains? [n form1]))
             [{nil #{[new-t]
                     [new-t (tab/tableau n form2)]
                     [form1 t new-t]}
               t #{node-label}}]
             []))))
      (fn
        ([] [:by-arity 2])
        ([cur-tab [n form :as node-label]]
         [{nil (->> (rw/query cur-tab [:labels-by-node-prefix-infix n :! form])
                    (r/mapcat (fn [[n2 [op form2 form3]]]
                                (let [new-t (gensym "tableau")]
                                  [[new-t]
                                   [new-t (tab/tableau n form3)]
                                   [form t new-t]])))
                    (u/foldset))
           t #{node-label}}]))
      (fn
        ([] [:labels-by-prefix2 :not :!])
        ([cur-tab [n [op1 [op2 form1 form2]] :as node-label]]
         (let [new-t (gensym "tableau")]
           [{nil #{[new-t]
                   [new-t (tab/tableau n [:not form2])]
                   [form1 t new-t]}
             t #{node-label}}
            {t #{[n form1]}}])))])))

(defn synchronization-range [casc t]
  (->> (rw/query casc [:edges-by-src t])
       (r/map #(nth % 2))
       (r/foldcat)
       (r/cat (->> (rw/query casc [:edges-by-dest t])
                   (r/map second)
                   (r/foldcat)))
       (r/mapcat (fn [t] (->> (newest-tableau casc t)
                              (#(rw/query % [:by-arity 1]))
                              (r/map (fn [[n]] {n #{t}})))))
       (r/fold u/mmap-merge)))

(defn synchronize-init
  ([] [:by-arity 3])
  ([casc [precond t1 t2]]
   [(r/fold u/mmap-merge
            ; copy nodes from t2 to t1
            [(->> (rw/query (newest-tableau casc t2)
                            [:by-arity 1])
                  (r/map (fn [x] {t1 #{x}}))
                  (r/fold u/mmap-merge))
             ; copy nodes from t1 to t2
             (->> (rw/query (newest-tableau casc t1)
                            [:labels-by-label precond])
                  (r/map (fn [[n form]] {t2 #{[n]}}))
                  (r/fold u/mmap-merge))])]))

(defn synchronize
  ([] [:by-arity 2])
  ([casc [t tab]]
   (let [sync-range (synchronization-range casc t)]
     (rw/process
       tab
       (fn [rs updates]
         [(r/fold u/mmap-merge updates)])
       :prime-sync-mark
       [(fn
          ; copy atoms to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(rw/newest casc [:labels-by-node %]))
                (r/mapcat #(rw/query (second %) [:labels-by-prefix nil]))
                (r/filter #(= (first %) n))
                (r/map (fn [x] {t #{x}})))))
        (fn
          ; copy incoming edges to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(rw/newest casc [:labels-by-node %]))
                (r/mapcat #(rw/query (second %) [:edges-by-dest n]))
                (r/filter #(contains? (rw/query cur-tab [:by-arity 1])
                                      [(second %)]))
                (r/map (fn [x] {t #{x}})))))
        (fn
          ; copy outgoing edges to this tableau
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (get sync-range n)
                (r/map #(rw/newest casc [:labels-by-node %]))
                (r/mapcat #(rw/query (second %) [:edges-by-src n]))
                (r/filter #(contains? (rw/query cur-tab [:by-arity 1])
                                      [(nth % 2)]))
                (r/map (fn [x] {t #{x}})))))
        (fn
          ; copy node to left-hand tableaux
          ([] [:by-arity 1])
          ([cur-tab [n]]
           (->> (rw/query casc [:edges-by-dest t])
                (r/map (fn [[precond t1 t2]] {t1 #{[n]
                                                   [n precond]}})))))
        (fn
          ; copy node to right-hand tableaux
          ; upon adding of precondition
          ([] [:by-arity 2])
          ([cur-tab [n form]]
           (->> (rw/query casc [:edges-by-idx-src form t])
                (r/map (fn [[precond t1 t2]] {t2 #{[n]}})))))
        (fn
          ; copy atoms to counterpart worlds
          ; in neighboring tableaux
          ([] [:labels-by-prefix nil])
          ([cur-tab [n atom]]
           (r/map (fn [t2] {t2 #{[n atom]}})
                  (get sync-range n))))
        (fn
          ; copy edges to neighboring tableaux
          ; (if counterpart worlds exist)
          ([] [:by-arity 3])
          ([cur-tab [idx n m]]
           (r/map (fn [t2] {t2 #{[idx n m]}})
                  (set/intersection (get sync-range n)
                                    (get sync-range m)))))]))))

(defn meta-dispatcher [meta-tab marker]
  (fn [rs table]
    (->> (apply u/mmap-merge table)
         (r/mapcat (fn [t coll]
                     (if t
                       [[t (-> (newest-tableau meta-tab t)
                               (rw/mark marker)
                               (rw/post coll))]]
                       coll)))
         (r/foldcat)
         (rw/post rs))))

(defn saturate [cascades]
  (let [next-rss
        (->> cascades
             (r/mapcat #(rw/process
                          %
                          (rw/disjunctify rw/post)
                          :casc-sat-mark
                          [saturate-tableau]))
             (r/map #(rw/process
                       %
                       (meta-dispatcher % :prime-sync-mark)
                       :casc-sat-mark2
                       [prime
                        synchronize
                        synchronize-init]))
             (r/foldcat))]
    (if-not (->> next-rss
                 (r/mapcat #(rw/since % :casc-sat-mark))
                 (u/fold-empty?))
      (recur next-rss)
      next-rss)))

(defn consistent? [casc]
  (->> (rw/query casc [:by-arity 1])
       (r/map #(rw/newest casc [:labels-by-node (first %)]))
       (r/filter #(not (tab/consistent? (second %))))
       (u/fold-empty?)))

(defn satisfiable? [form]
  (->> (saturate [(tableau-cascade form)])
       (r/filter consistent?)
       (u/fold-empty?)
       (not)))

(defn valid? [form]
  (not (satisfiable? [:not form])))