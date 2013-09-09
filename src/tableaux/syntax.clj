(ns tableaux.syntax
  (:require [clojure.set :as set]))

(def Prop #{:p :q :r :s :t})

(def Ind #{:a :b :c :d :e})

(defn wff? [form]
  (try
    (case [(count form) (nth form 0)]
      [2 :not] (wff? (nth form 1))
      [3 :and] (and (wff? (nth form 1))
                    (wff? (nth form 2)))
      [3 :box] (and (contains? Ind (nth form 1))
                    (wff? (nth form 2)))
      [3 :!] (and (wff? (nth form 1))
                  (wff? (nth form 2)))
      false)
    (catch Exception x ; not a coll or empty coll
      (contains? Prop form))))

(defn implies [x y]
  [:not [:and x [:not y]]])

(defn equiv [x y]
  [:and (implies x y) (implies y x)])

(defn diamond [idx form]
  [:not [:box idx [:not form]]])