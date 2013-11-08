(ns dyntab.util
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]))

(defn mmap-get [m k]
  (get m k #{}))

(defn mmap-conj [m [k v]]
  (assoc m k (conj (mmap-get m k)
                   v)))

(defn mmap-merge [& ms]
  (apply merge-with into ms))
  
(defn map-merge-overwrite [& ms]
  (apply merge-with (fn [x y] y) ms))

(defn mmap-get-unique [m k]
  (let [v (mmap-get m k)]
    (if-not (= (count v) 1)
      (throw (str "No unique value for " k " in " m)))
    (first v)))

(defn mmap-diff [m1 m2]
  (->> m1
       (r/map (fn [[k v]]
                [k (set/difference v (get m2 k #{}))]))
       (r/filter (fn [[k v]]
                   (not (= v #{}))))
       (r/reduce conj {})))

(defn fold-empty? [coll]
  (r/fold (fn
            ([] true)
            ([left right] (and left right)))
          (constantly false)
          coll))

(defn foldset [coll]
  (r/fold set/union conj coll))

(defn rjuxt [f coll]
  (r/mapcat (fn [x] (r/map (fn [y] [x y])
                           (f x)))
            coll))

(defn one-from-each [coll-of-colls]
  (letfn [[f [acc rem]
           (cond (empty? rem) [acc]
                 (empty? (first rem)) (f acc (next rem))
                 :else (->> (first rem)
                            (r/mapcat #(f (conj acc %) 
                                          (next rem)))))]]
    (f #{} coll-of-colls)))

