(ns tableaux.util
  (:require [clojure.core.protocols :as p]
            [clojure.core.reducers :as r]
            [clojure.set :as set]))

(defn map-merge-overwrite [& ms]
  (apply merge-with (fn [x y] y) ms))

(defn mmap-merge [& ms]
  (apply merge-with into ms))

(defn mmap-get [m k]
  (get m k #{}))

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
            ([& xs] false))
          coll))

(defn foldset [coll]
  (r/fold set/union conj coll))

(defn rjuxt [f coll]
  (r/folder coll
            (fn [f1]
              (fn
                ([] (f1))
                ([ret v]
                 (p/coll-reduce (r/map (fn [x] [v x]) (f v))
                                f1
                                ret))))))

(defn one-from-each [coll-of-colls]
  (letfn [[f [acc rem]
           (cond (empty? rem) [acc]
                 (empty? (first rem)) (f acc (next rem))
                 :else (->> (first rem)
                            (r/mapcat #(f (conj acc %) (next rem)))))]]
    (f #{} coll-of-colls)))
