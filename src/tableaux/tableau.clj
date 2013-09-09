(ns tableaux.tableau
  (:require [clojure.core.reducers :as r]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :as log]
            [tableaux.util :as u]
            [tableaux.rules :as tr]
            [tableaux.syntax :as syntax]))

(defn tableau
  ([form] (tableau :start-node form))
  ([node form]
   (if (not (syntax/wff? form))
     (throw (IllegalArgumentException. (str "Not a wff: " form))))
   (-> (rw/logging-system [log/log-by-arity
                             log/log-labels-by-label
                             log/log-labels-by-prefix
                             log/log-labels-by-prefix2
                             log/log-labels-by-node-prefix-infix
                             log/log-edges-by-idx-src
                             log/log-edges-by-idx-dest
                             log/log-edges-by-src
                             log/log-edges-by-dest])
       (rw/post [[node] [node form]]))))

(defn saturate [tableaux]
  (loop [tableaux tableaux
         changed false]
    (let [next-rss
          (->> tableaux
               (r/map #(rw/process %
                                   rw/post
                                   :tab-sat-mark
                                   [tr/rule-not-not
                                    tr/rule-and
                                    tr/rule-box1
                                    tr/rule-box2
                                    tr/rule-T
                                    tr/rule-B
                                    tr/rule-4]))
               (r/map #(rw/process %
                                   rw/post
                                   :tab-sat-mark2
                                   [tr/rule-not-box]))
               (r/mapcat #(rw/process %
                                      (rw/disjunctify rw/post)
                                      :tab-sat-mark3
                                      [tr/fork-rule-not-and
                                       tr/fork-rule-precond1
                                       tr/fork-rule-precond2
                                       tr/fork-rule-precond3
                                       tr/fork-rule-precond4]))
               (r/foldcat))]
      (if-not (->> next-rss
                   (r/mapcat #(rw/since % :tab-sat-mark))
                   (u/fold-empty?))
        (recur next-rss true)
        [changed next-rss]))))

(defn consistent? [rs]
  (let [not-stmnts (rw/query rs [:labels-by-prefix :not])]
    (->> (rw/query rs [:labels-by-prefix nil])
         (r/filter #(get not-stmnts [(first %) [:not (second %)]]))
         (u/fold-empty?))))