(ns tableaux.rewrite
  (:require [clojure.core.reducers :as r]
            [tableaux.util :as u]))

(defprotocol IRewritingSystem
  (post [this coll])
  (query [this k])
  (mark [this marker])
  (newest [this k])
  (since [this marker])
  (process [this f marker rules]))

(defrecord RewritingSystem [loggers everything newest history])

(defn rewriting-system
[loggers] (RewritingSystem. loggers {} {} '()))

(extend-type RewritingSystem
  IRewritingSystem
  (post [this coll]
        (let [aggr-result (->> (.loggers this)
                               (u/rjuxt (constantly coll))
                               (r/map (fn [[f v]] (f v)))
                               (r/reduce u/mmap-merge))]
          (RewritingSystem. (.loggers this)
                            (u/mmap-merge (.everything this) aggr-result)
                            (u/map-merge-overwrite (.newest this) aggr-result)
                            (cons (u/mmap-diff aggr-result (.everything this))
                                  (.history this)))))
  (query [this k] (u/mmap-get (.everything this) k))
  (mark [this marker]
        (assert (not (coll? marker)))
        (RewritingSystem. (.loggers this)
                          (.everything this)
                          (.newest this)
                          (cons marker (.history this))))
  (newest [this k] (u/mmap-get-unique (.newest this) k))
  (since [this marker]
         (->> (.history this)
              (r/take-while #(not= % marker))
              (r/filter coll?)
              (r/fold u/mmap-merge)))
  (process [this f marker rules]
           (let [new-logs (since this marker)]
             (->> rules
                  (u/rjuxt #(get new-logs (%)))
                  (r/mapcat (fn [[f v]] (f this v)))
                  ;(r/fold 5 (r/cat #(java.util.HashSet.)) r/append!)
                  (u/foldset)
                  (f (mark this marker))))))

(defn disjunctify
[f]
(fn [rs coll-of-colls]
  (r/map (partial f rs)
         (u/one-from-each coll-of-colls))))