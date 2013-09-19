(ns tableaux.bag
  (:require [clojure.core.reducers :as r]
            [tableaux.util :as u]))

(defprotocol ITupleBag
  (post [this coll])
  (query [this k])
  (newest [this k])
  (mark [this marker])
  (since [this marker])
  (process [this marker rules]))

(defrecord TupleBag [indexers everything newest history])

(defn tuple-bag [indexers]
  (TupleBag. indexers {} {} '()))

(extend-type TupleBag
  ITupleBag
  (post [this coll]
        (let [aggr-result (->> (.indexers this)
                               (u/rjuxt (constantly coll))
                               (r/mapcat
                                 (fn [[f v]]
                                   (r/map (fn [k] [k v])
                                          (f v))))
                               (r/fold u/mmap-merge
                                       u/mmap-conj))]
          (TupleBag. (.indexers this)
                     (u/mmap-merge (.everything this)
                                   aggr-result)
                     (u/map-merge-overwrite (.newest this)
                                            aggr-result)
                     (cons (u/mmap-diff aggr-result
                                        (.everything this))
                           (.history this)))))
  (query [this k] (u/mmap-get (.everything this) k))
  (newest [this k] (u/mmap-get-unique (.newest this) k))
  (mark [this marker]
        (assert (keyword? marker))
        (TupleBag. (.indexers this)
                   (.everything this)
                   (.newest this)
                   (cons marker (.history this))))
  (since [this marker]
         (->> (.history this)
              (r/take-while #(not= % marker))
              (r/filter coll?)
              (r/fold u/mmap-merge)))
  (process [this marker rules]
           (let [new-bags (since this marker)]
             [(mark this marker)
              (->> rules
                   (u/rjuxt #(get new-bags (%)))
                   (r/mapcat (fn [[f v]] (f this v)))
                   (u/foldset))])))

(defn nth-or-nil [obj n]
  (try
    (nth obj n)
    (catch Exception x nil)))

(defn index-by-arity [x]
  [[:by-arity (count x)]])

(defn index-pairs-by-first [x]
  (if (= 2 (count x))
    [[:pairs-by-first (first x)]]
    nil))

(defn index-pairs-by-second [x]
  (if (= 2 (count x))
    [[:pairs-by-second (second x)]]
    nil))

(defn index-pairs-by-first-of-second [x]
  (if (= 2 (count x))
    [[:pairs-by-first-of-second (nth-or-nil (second x) 0)]]
    nil))

(defn index-pairs-by-first+second-of-second [x]
  (if (= 2 (count x))
    (let [form (second x)]
      [[:pairs-by-first+second-of-second
        (first x)
        (nth-or-nil form 0)
        (nth-or-nil form 1)]])
    nil))

(defn index-pairs-by-first+firstsecond-of-second [x]
  (if (= 2 (count x))
    (let [outer-form (second x)
          inner-form (nth-or-nil outer-form 1)]
      [[:pairs-by-first+second-of-second
        (nth-or-nil outer-form 0)
        (nth-or-nil inner-form 0)]])
    nil))

(defn index-triples-by-first-second [x]
  (if (= 3 (count x))
    [[:triples-by-first-second (first x) (second x)]]
    nil))

(defn index-triples-by-first-third [x]
  (if (= 3 (count x))
    [[:triples-by-first-third (first x) (nth x 2)]]
    nil))

(defn index-triples-by-second [x]
  (if (= 3 (count x))
    [[:triples-by-second (second x)]]
    nil))

(defn index-triples-by-third [x]
  (if (= 3 (count x))
    [[:triples-by-third (nth x 2)]]
    nil))

