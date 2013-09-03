(ns tableaux.rules
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [tableaux.rewrite :as rw]
            [tableaux.util :as u]
            [tableaux.syntax :as syntax]))

(defn rule-not-not
  ([] [:labels-by-prefix2 :not :not])
  ([rs [n
        [not1 [not2 form]]]]
   [[n form]]))

(defn rule-and
  ([] [:labels-by-prefix :and])
  ([rs [n
        [and1 form1 form2]]]
   [[n form1]
    [n form2]]))

(defn fork-rule-not-and
([] [:labels-by-prefix2 :not :and])
([rs [n
      [not1 [and1 form1 form2]]]]
 [[[n [:not form1]]
   [n [:not form2]]]]))

(defn rule-box1
([] [:labels-by-prefix :box])
([rs [n
      [box1 idx form]]]
 (->> (rw/query rs [:edges-by-idx-src idx n])
      (r/map (fn [m] [(nth m 2) form])))))

(defn rule-box2
([] [:by-arity 3])
([rs [idx src dest]]
 (->> (rw/query rs [:labels-by-node-prefix-infix src :box idx])
      (r/map (fn [m] [dest (nth (second m) 2)])))))

(defn rule-not-box
  ([] [:labels-by-prefix2 :not :box])
  ([rs [n
        [not1 [box1 idx form]]]]
   (if (->> (rw/query rs [:edges-by-idx-src idx n])
                (r/filter (fn [[idx2 src2 dest2]]
                            (get (rw/query rs [:by-arity 2])
                                 [dest2 [:not form]])))
                (u/fold-empty?))
     (let [m (gensym "node")]
       [[m]
        [idx n m]
        [m [:not form]]])
     [])))

(defn- fork-declare [rs n form]
  (let [all-labels (rw/query rs [:by-arity 2])]
    (if (empty? (set/intersection all-labels
                                  #{[n form] [n [:not form]]}))
      [[n form]
       [n [:not form]]])))

(defn fork-rule-precond1
([] [:labels-by-prefix :!])
([rs [n
      [bang1 ann-form post-form]]]
 (->> (rw/query rs [:by-arity 1])
      (r/map (fn [[n]] (fork-declare rs n ann-form))))))

(defn fork-rule-precond2
([] [:labels-by-prefix2 :not :!])
([rs [n
      [not1 [bang1 ann-form post-form]]]]
 (->> (rw/query rs [:by-arity 1])
      (r/map (fn [[n]] (fork-declare rs n ann-form))))))

(defn fork-rule-precond3
([] [:by-arity 1])
([rs [n]]
 (->> (rw/query rs [:labels-by-prefix :!])
      (r/map (fn [[x [ann1 ann-form post-form]]]
               (fork-declare rs n ann-form))))))

(defn fork-rule-precond4
([] [:by-arity 1])
([rs [n]]
 (->> (rw/query rs [:labels-by-prefix2 :not :!])
      (r/map (fn [[x [not1 [ann1 ann-form post-form]]]]
               (fork-declare rs n ann-form))))))

(defn rule-T
  ([] [:by-arity 1])
  ([rs [n]] (->> syntax/Ind
                 (r/map (fn [x] [x n n])))))

(defn rule-B
  ([] [:by-arity 3])
  ([rs [idx src dest]] [[idx dest src]]))

(defn rule-4
  ([] [:by-arity 3])
  ([rs [idx src dest]]
   (->> [(r/map (fn [[idx2 src2 dest2]] [idx src dest2])
                (rw/query rs [:edges-by-idx-src idx dest]))
         (r/map (fn [[idx2 src2 dest2]] [idx src2 dest])
                (rw/query rs [:edges-by-idx-dest idx src]))]
        (r/mapcat identity))))
