(ns tableaux.util
  "Utility functions for working with multi-maps (maps containing
  sets of items) and fold/combine."
  (:require [clojure.core.protocols :as p]
            [clojure.core.reducers :as r]
            [clojure.set :as set]))

(defn mmap-merge
  "An mmap (or multi-map) is a map in which all values are sets. This
  function merges such structures.

  Returns an mmap with a key-set equal to the union of the key-sets of
  the mmaps in ms. In the returned mmap, the value for every key k is
  the union of all values for k in the different mmaps of ms."
  [& ms] (apply merge-with into ms))

(defn mmap-diff
  "An mmap (or multi-map) is a map in which all values are sets. This
  function subtracts the mmap m2 from the mmap m2.

  Returns an mmap. The value for every key k in the resulting mmap is
  the set-difference of the value for k in m1 minus the value for k in
  m2."
  [m1 m2]
  (->> m1
       (r/map (fn [[k v]] [k (set/difference v (get m2 k #{}))]))
       (r/filter (fn [[k v]] (not (= v #{}))))
       (r/reduce conj {})))

(defn rjuxt
  "Returns a foldable collection of all vectors [x y] where x is an
  element of coll and y is in f(x).

  coll must be a foldable collection and f must map the elements of
  coll onto reducible collections."
  [f coll]
  (r/folder coll
            (fn [f1]
              (fn
                ([] (f1))
                ([ret v] (p/coll-reduce (r/map (fn [x] [v x]) (f v))
                                        f1
                                        ret))))))

(defn one-from-each
  "Given a foldable collection coll-of-colls of foldable collections,
  this function returns a foldable collection of every set that contains
  exactly one element from each collection in coll-of-colls"
  [coll-of-colls]
  (letfn [[f [acc rem]
           (if (empty? rem)
             [acc]
             (->> (first rem)
                  (r/map #(f (conj acc %) (next rem)))
                  (r/fold (r/monoid into hash-set))))]]
    (f #{} coll-of-colls)))
