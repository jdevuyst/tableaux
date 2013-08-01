(ns tableaux.tableau-rules
  "Tableau rules. For information on how these work see my PhD thesis."
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [tableaux.rewrite :as rw]
            [tableaux.util :as u]
            [tableaux.syntax :as syntax]))

(defn rule-not-not
  "Rule for elimination of double negation."
  ([] [:labels-by-prefix2 :not :not])
  ([rs [n
        [not1 [not2 form]]]]
   (assert (= not1 not2 :not))
   #{[n form]}))

(defn rule-and
  "Conjunction elimination rule."
  ([] [:labels-by-prefix :and])
  ([rs [n
        [and1 form1 form2]]]
   #{[n form1]
     [n form2]}))

(defn fork-rule-not-and
  "Disjunction rule. Returns a 2-tuple, of which the elements represent
  different branches."
  ([] [:labels-by-prefix2 :not :and])
  ([rs [n
        [not1 [and1 form1 form2]]]]
   #{[[n [:not form1]]
      [n [:not form2]]]}))

(defn rule-box1
  "Box rule, part 1. This function is triggered when a new box is
  added."
  ([] [:labels-by-prefix :box])
  ([rs [n
        [box1 idx form]]]
   (map (fn [m] [(nth m 2) form])
        (rw/query rs [:edges-by-idx-src idx n]))))

(defn rule-box2
  "Box rule, part 2. This function is triggered when a new edge is
  added."
  ([] [:by-arity 3])
  ([rs [idx src dest]]
   (map (fn [m] [dest (nth (second m) 2)])
        (rw/query rs [:labels-by-node-prefix-infix src :box idx]))))

(defn rule-not-box
  "Diamond rule."
  ([] [:labels-by-prefix2 :not :box])
  ([rs [n
        [not1 [box1 idx form]]]]
   (if (->> (rw/query rs [:edges-by-idx-src idx n])
            (r/filter (fn [[idx2 src2 dest2]]
                        (get (rw/query rs [:by-arity 2])
                             [dest2 [:not form]])))
            (r/fold (r/monoid (constantly true) (constantly false))))
     #{}
     (let [m (gensym)]
       #{[m]
         [idx n m]
         [m [:not form]]}))))

(defn- fork-precond [forms nodes]
  (->> nodes
       (u/rjuxt (constantly forms))
       (r/map (fn [[[n] form]] [[n form]
                                [n [:not form]]]))
       (r/fold (r/monoid conj hash-set))))

(defn fork-rule-precond1
  "Precondition rule, part 1. This function is triggered when a new,
  positive, announcement formula is added."
([] [:labels-by-prefix :!])
([rs [n
      [bang1 ann-form post-form]]]
 (->> (rw/query rs [:by-arity 1])
      (r/map (fn [[n]] [[n ann-form] [n [:not ann-form]]]))
      (r/fold (r/monoid conj hash-set)))))

(defn fork-rule-precond2
  "Precondition rule, part 2. This function is triggered when a new,
  negated, announcement formula is added."
  ([] [:labels-by-prefix2 :not :!])
  ([rs [n
        [not1 [bang1 ann-form post-form]]]]
 (->> (rw/query rs [:by-arity 1])
      (r/map (fn [[n]] [[n ann-form] [n [:not ann-form]]]))
      (r/fold (r/monoid conj hash-set)))))

(defn fork-rule-precond3
  "Precondition rule, part 3. This function is triggered when a new
  node is added"
([] [:by-arity 1])
([rs [n]]
 (->> (rw/query rs [:labels-by-prefix :!])
      (r/map (fn [[x [ann1 ann-form post-form]]]
               [[n ann-form] [n [:not ann-form]]]))
      (r/fold (r/monoid conj hash-set)))))

(defn fork-rule-precond4
  "Precondition rule, part 4. This function is triggered when a new
  node is added"
([] [:by-arity 1])
([rs [n]]
 (->> (rw/query rs [:labels-by-prefix2 :not :!])
      (r/map (fn [[x [not1 [ann1 ann-form post-form]]]]
               [[n ann-form] [n [:not ann-form]]]))
      (r/fold (r/monoid conj hash-set)))))

(defn rule-T
  "Rule for reflexivity."
  ([] [:by-arity 1])
  ([rs [n]] (map (fn [x] [x n n]) syntax/Ind)))

(defn rule-B
  "Rule for symmetry."
  ([] [:by-arity 3])
  ([rs [idx src dest]] #{[idx dest src]}))

(defn rule-4
  "Rule for transitivity."
  ([] [:by-arity 3])
  ([rs [idx src dest]]
   (r/fold (r/monoid into hash-set)
           [(r/map (fn [[idx2 src2 dest2]] [idx src dest2])
                   (rw/query rs [:edges-by-idx-src idx dest]))
            (r/map (fn [[idx2 src2 dest2]] [idx src2 dest])
                   (rw/query rs [:edges-by-idx-dest idx src]))])))
