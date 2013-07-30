(ns tableaux.core
  "Functions for using the tableau system. Of particular intrest
  are explain, valid?, and satisfiable?."
  (:require [clojure.core.reducers :as r]
            [tableaux.rewrite :as rw]
            [tableaux.loggers :as log]
            [tableaux.util :as u]
            [tableaux.tableau-rules :as tr]
            [tableaux.syntax :as syntax]))

(defn construct-tableaux
  "Constructs a (complete) set of saturated tableaux for the given
  formula. Returns the tableau in the form of a rewriting system.
  The elements can be accessed through the following logs: by-arity,
  labels-by-prefix, labels-by-prefix2, labels-by-node-prefix-infix,
  edges-by-idx-src, edges-by-idx-dest.

  Throws an IllegalArgumentException if the argument is not a
  well-formed formula."
  [form]
  (if (not (syntax/wff? form))
    (throw (IllegalArgumentException. (str "Not a wff: " form))))
  (loop [rss #{(-> (rw/rewriting-system [log/log-by-arity
                                         log/log-labels-by-prefix
                                         log/log-labels-by-prefix2
                                         log/log-labels-by-node-prefix-infix
                                         log/log-edges-by-idx-src
                                         log/log-edges-by-idx-dest])
                   (rw/post [[:start-node] [:start-node form]]))}]
    (let [next-rss (->> rss
                        (r/map #(rw/process %
                                            rw/post
                                            :mark
                                            [tr/rule-not-not
                                             tr/rule-and
                                             tr/rule-box1
                                             tr/rule-box2
                                             tr/rule-T
                                             tr/rule-B
                                             tr/rule-4]))
                        (r/map #(rw/process %
                                            rw/post
                                            :mark2
                                            [tr/rule-not-box]))
                        (r/map #(rw/process %
                                            (rw/disjunctify rw/post)
                                            :mark3
                                            [tr/fork-rule-not-and]))
                        (r/fold (r/monoid into hash-set))
                        (r/fold (r/monoid conj hash-set)))]
      (if-not (empty? (->> next-rss
                           (r/map #(rw/since % :mark))
                           (r/reduce (r/monoid into hash-set))))
        (recur next-rss)
        next-rss))))

(defn rs-consistent?
  "Tests if the argument is a consistent tableau. A tableau is
  considered consistent iff it has no literal contradictions."
  [rs]
  (let [not-stmnts (rw/query rs [:labels-by-prefix :not])]
    (->> (rw/query rs [:labels-by-prefix nil])
         (r/filter #(get not-stmnts [(first %) [:not (second %)]]))
         (r/fold (r/monoid (constantly false) (constantly true))))))

(defn satisfiable?
  "Returns true iff there's a model for the given formula."
  [form]
  (->> (construct-tableaux form)
       (r/filter rs-consistent?)
       (r/fold (r/monoid (constantly true) (constantly false)))))

(defn valid?
  "Returns true iff the given formula is true in all models."
  [form]
  (not (satisfiable? [:not form])))

(defn print-graph
  "Human readable print-out of a graph, tableau, or tableau cascade."
  [rs]
  (println "Nodes:")
  (doseq [x (rw/query rs [:by-arity 1])] (println "-" x))
  (println "Edges:")
  (doseq [x (rw/query rs [:by-arity 3])] (println "-" x))
  (println "Labels:")
  (doseq [x (rw/query rs [:by-arity 2])] (println "-" x)))

(defn explain
  "Tests if the given formula is satisfiable and valid, and prints a
  report for human consumption."
  [form]
  (print (str "Constructing tableaux for " form "..."))
  (let [open-tableaux (->> (construct-tableaux form)
                      (r/filter rs-consistent?)
                      (r/fold (r/monoid conj hash-set)))
        sat (not (empty? open-tableaux))
        valid (valid? form)]
    (println " done.")
    (if sat
      (do
        (println "The formula is"
                 (if valid
                   "valid."
                   "satisfiable but not valid."))
        (when (not valid)
          (if (= 1 (count open-tableaux))
            (println "One open tableau was constructed.")
            (println (count open-tableaux)
                     "open tableaux were constructed. Here's one."))
          (print-graph (first open-tableaux))))
      (println "The formula is not satisfiable."
               "Therefore, its negation is valid."))))
