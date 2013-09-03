(ns tableaux.loggers
  (:require [clojure.core.reducers :as r]
            [tableaux.rewrite :as rw]))

(defn- tuple-count [x]
  (if (sequential? x)
    (count x)
    0))

(defn- nth-or-nil [obj n]
  (try
    (nth obj n)
    (catch Exception x nil)))

(defn log-by-arity [x]
  {[:by-arity (tuple-count x)] #{x}})

(defn log-labels-by-node [x]
  (if (= 2 (tuple-count x))
    {[:labels-by-node (first x)] #{x}}
    {}))

(defn log-labels-by-label [x]
  (if (= 2 (tuple-count x))
    {[:labels-by-label (second x)] #{x}}
    {}))

(defn log-labels-by-prefix [x]
  (if (= 2 (tuple-count x))
    {[:labels-by-prefix (nth-or-nil (second x) 0)] #{x}}
    {}))

(defn log-labels-by-prefix2 [x]
  (if (= 2 (tuple-count x))
    (let [outer-form (second x)
          inner-form (nth-or-nil outer-form 1)]
      {[:labels-by-prefix2
        (nth-or-nil outer-form 0)
        (nth-or-nil inner-form 0)]
       #{x}})
    {}))

(defn log-labels-by-node-prefix-infix [x]
  (if (= 2 (tuple-count x))
    (let [form (second x)]
      {[:labels-by-node-prefix-infix
        (first x)
        (nth-or-nil form 0)
        (nth-or-nil form 1)]
       #{x}})
    {}))

(defn log-edges-by-idx-src [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-idx-src (first x) (second x)] #{x}}
    {}))

(defn log-edges-by-idx-dest [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-idx-dest (first x) (nth x 2)] #{x}}
    {}))

(defn log-edges-by-src [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-src (second x)] #{x}}
    {}))

(defn log-edges-by-dest [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-dest (nth x 2)] #{x}}
    {}))
