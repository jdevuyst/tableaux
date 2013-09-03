(ns tableaux.syntax
  "Sets and functions for dealing with the formal language of
  public announcement logic."
  (:require [clojure.set :as set]))

(def Prop "Set of atomic propositions." #{:p :q :r :s :t})

(def Ind "Set of indices (or agent names)." #{:a :b :c :d :e})

(defn wff?
  "Returns true if passed a well-formed formula (wff); false otherwise.

  Wff are defined as follows:

  - Any element of Prop is a wff
  - [:not wff]
  - [:and wff wff]
  - [:box x wff] (where x is an element of Ind)"
  [form]
  (or (contains? Prop form)
      (try
        (case [(count form) (first form)]
          [2 :not] (wff? (second form))
          [3 :and] (and (wff? (nth form 1))
                        (wff? (nth form 2)))
          [3 :box] (and (contains? Ind (nth form 1))
                        (wff? (nth form 2)))
          [3 :!] (and (wff? (nth form 1))
                      (wff? (nth form 2))))
        (catch Exception x false))))

(defn implies [x y]
  [:not [:and x [:not y]]])

(defn iff [x y]
  [:and (implies x y) (implies y x)])