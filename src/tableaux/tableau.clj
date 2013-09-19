(ns tableaux.tableau
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [tableaux.bag :as bag]
            [tableaux.util :as u]
            [tableaux.syntax :as syntax]))

(defn rule-not-not
  ([] [:pairs-by-first+second-of-second :not :not])
  ([tb [n
        [not1 [not2 form]]]]
   [[n form]]))

(defn rule-and
  ([] [:pairs-by-first-of-second :and])
  ([tb [n
        [and1 form1 form2]]]
   [[n form1]
    [n form2]]))

(defn rule-box1
  ([] [:pairs-by-first-of-second :box])
  ([tb [n
        [box1 idx form]]]
   (->> (bag/query tb [:triples-by-first-second idx n])
        (r/map (fn [m] [(nth m 2) form])))))

(defn rule-box2
  ([] [:by-arity 3])
  ([tb [idx src dest]]
   (->> (bag/query 
          tb 
          [:pairs-by-first+second-of-second src :box idx])
        (r/map (fn [m] [dest (nth (second m) 2)])))))

(defn rule-not-box
  ([] [:pairs-by-first+second-of-second :not :box])
  ([tb [n
        [not1 [box1 idx form]]]]
   (if (->> (bag/query tb [:triples-by-first-second idx n])
            (r/filter (fn [[idx2 src2 dest2]]
                        (get (bag/query tb [:by-arity 2])
                             [dest2 [:not form]])))
            (u/fold-empty?))
     (let [m (gensym "node")]
       [[m]
        [idx n m]
        [m [:not form]]])
     [])))

(defn rule-T
  ([] [:by-arity 1])
  ([tb [n]] (->> syntax/Ind
                 (r/map (fn [x] [x n n])))))

(defn rule-B
  ([] [:by-arity 3])
  ([tb [idx src dest]] [[idx dest src]]))

(defn rule-4
  ([] [:by-arity 3])
  ([tb [idx src dest]]
   (->> [(r/map (fn [[idx2 src2 dest2]] [idx src dest2])
                (bag/query 
                  tb 
                  [:triples-by-first-second idx dest]))
         (r/map (fn [[idx2 src2 dest2]] [idx src2 dest])
                (bag/query 
                  tb 
                  [:triples-by-first-third idx src]))]
        (r/mapcat identity))))

(defn rule-not-and*
  ([] [:pairs-by-first+second-of-second :not :and])
  ([tb [n
        [not1 [and1 form1 form2]]]]
   [[[n [:not form1]]
     [n [:not form2]]]]))

(defn rule-precond1*
  ([] [:pairs-by-first-of-second :!])
  ([tb [n
        [bang1 ann-form post-form]]]
   (->> (bag/query tb [:by-arity 1])
        (r/map (fn [[n]]
                 [[n ann-form] [n [:not ann-form]]])))))

(defn rule-precond2*
  ([] [:pairs-by-first+second-of-second :not :!])
  ([tb [n
        [not1 [bang1 ann-form post-form]]]]
   (->> (bag/query tb [:by-arity 1])
        (r/map (fn [[n]]
                 [[n ann-form] [n [:not ann-form]]])))))

(defn rule-precond3*
  ([] [:by-arity 1])
  ([tb [n]]
   (->> (bag/query tb [:pairs-by-first-of-second :!])
        (r/map (fn [[x [ann1 ann-form post-form]]]
                 [[n ann-form] [n [:not ann-form]]])))))

(defn rule-precond4*
  ([] [:by-arity 1])
  ([tb [n]]
   (->> (bag/query
          tb
          [:pairs-by-first+second-of-second :not :!])
        (r/map (fn [[x [not1 [ann1 ann-form post-form]]]]
                 [[n ann-form] [n [:not ann-form]]])))))

(defn disjunctive-post [tb coll-of-colls]
  (r/map (partial bag/post tb)
         (u/one-from-each coll-of-colls)))

(defn tableau
  ([form] (tableau :start-node form))
  ([node form]
   (if (not (syntax/wff? form))
     (throw (IllegalArgumentException. 
              (str "Not a wff: " form))))
   (-> (bag/tuple-bag
         [bag/index-by-arity
          bag/index-pairs-by-second
          bag/index-pairs-by-first-of-second
          bag/index-pairs-by-first+firstsecond-of-second
          bag/index-pairs-by-first+second-of-second
          bag/index-triples-by-first-second
          bag/index-triples-by-first-third
          bag/index-triples-by-second
          bag/index-triples-by-third])
       (bag/post [[node] [node form]]))))

(defn saturate [tableaux]
  (letfn [(phase1 [inner-tabs]
            (->> inner-tabs
                 (r/map #(bag/process %
                                      :tab-sat-mark
                                      [rule-not-not
                                       rule-and
                                       rule-box1
                                       rule-box2
                                       rule-T
                                       rule-B
                                       rule-4]))
                 (r/map (partial apply bag/post))
                 (r/foldcat)))
          (phase2 [inner-tabs]
            (->> inner-tabs
                 (r/map #(bag/process %
                                      :tab-sat-mark2
                                      [rule-not-box]))
                 (r/map (partial apply bag/post))
                 (r/map #(bag/process %
                                      :tab-sat-mark3
                                      [rule-not-and*
                                       rule-precond1*
                                       rule-precond2*
                                       rule-precond3*
                                       rule-precond4*]))
                 (r/mapcat (partial apply disjunctive-post))
                 (r/foldcat)))]
    (loop [tableaux tableaux
           cur-phase phase1
           changed false]
      (let [new-tableaux (cur-phase tableaux)]
        (if-not (->> new-tableaux
                     (r/mapcat #(bag/since % :tab-sat-mark))
                     (u/fold-empty?))
          (recur new-tableaux phase1 true)
          (if (= cur-phase phase1)
            (recur new-tableaux phase2 changed)
            [changed new-tableaux]))))))

(defn consistent? [tab]
  (let [not-forms (bag/query
                     tab
                     [:pairs-by-first-of-second :not])]
    (->> (bag/query tab [:pairs-by-first-of-second nil])
         (r/filter (fn [[n p]] (get not-forms [n [:not p]])))
         (u/fold-empty?))))

