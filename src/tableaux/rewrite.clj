(ns tableaux.rewrite
  "General abstractions for monotone rewriting systems."
  (:require [clojure.core.reducers :as r]
            [tableaux.util :as u]))

(defprotocol IRewritingSystem
  "Abstract rewriting system. Items (or elements or messages) are
  posted to this system, which are immediately logged by a configurable
  set of 'loggers'. These items are later processed by sets of rules,
  which might lead to new elements being posted.

  The rewriting system is monotone in the sense that, once recorded,
  posted items or logs cannot be deleted or modified.

  See rewriting-system for more information on loggers and logs."
  (post [this coll] "Posts new elements to the rewriting system.
                    This causes the loggers to create new logs.

                    Returns the rewriting system, ammended with the new
                    logs")
  (query [this k] "Returns the log (a set of items) filed under key k.")
  (mark [this marker] "Inserts the given marker. This allows for those
                      logts to be fetched that were inserted after the
                      marker was recorded.

                      Returns the rewriting system after inserting the
                      marker.")
  (since [this marker] "Returns the mmap containing those log items
                       inserted after the given marker was recorded.")
  (process [this f marker rules] "Applies the rules to the logs with key
                                 k that were inserted after the given
                                 marker.

                                 rules is a seq of functions that have
                                 the following properties.

                                 1. If called with no arguments a rule
                                 returns a log key.
                                 2. If called with two arguments (the
                                 rewriting system and the log for the
                                 above key) then the rule returns a
                                 meaningful foldable collection.

                                 (For example, it may return a set of
                                 new items to be posted to the
                                 rewriting system.)

                                 Finally, process does the following:
                                 1. Marks the rewriting system with the
                                 given marker.
                                 2. Computes the union of the
                                 collections returned by the rules.
                                 3. Applies f to 1 and 2.
                                 4. Returns the result of 3."))

(defrecord RewritingSystem [loggers logs history])

(defn rewriting-system
  "Creates a new rewriting system. The argument loggers is a collection
  of functions.

  Each logger takes one argument--namely an element that was posted to
  the rewriting system. The logger should return a log, which is an
  mmap from log keys to sets of items."
[loggers] (RewritingSystem. loggers {} '()))

(extend-type RewritingSystem
  IRewritingSystem
  (post [this coll]
        (let [aggr-result (->> (.loggers this)
                               (u/rjuxt (constantly coll))
                               (r/map (fn [[f v]] (f v)))
                               (r/fold u/mmap-merge))]
          (RewritingSystem. (.loggers this)
                            (u/mmap-merge (.logs this) aggr-result)
                            (cons (u/mmap-diff aggr-result (.logs this))
                                  (.history this)))))
  (query [this k] (get (.logs this) k #{}))
  (mark [this marker]
        (RewritingSystem. (.loggers this)
                          (.logs this)
                          (cons marker (.history this))))
  (since [this marker]
         (->> (.history this)
              (r/take-while #(not= % marker))
              (r/filter coll?)
              (r/fold u/mmap-merge)))
  (process [this f marker rules]
           (let [new-logs (since this marker)]
             (->> rules
                  (u/rjuxt #(get new-logs (%)))
                  (r/map (fn [[f v]] (f this v)))
                  (r/fold (r/monoid into hash-set))
                  (f (mark this marker))))))

(defn disjunctify
  "Given a function f, returns a new function f'.

  For every set S that contains exactly one element from every
  collection in coll-of-colls, f' applies f to rs and S. f' returns
  the results of these function calls as a foldable collection."
  [f]
  (fn [rs coll-of-colls]
    (r/map (partial post rs)
           (u/one-from-each coll-of-colls))))
