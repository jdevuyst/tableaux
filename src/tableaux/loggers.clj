(ns tableaux.loggers
  "Loggers for logging (or indexing) items posted to an abstract
  rewriting system, with a focus on tableau systems.

  Definitions. A node-label pair is a vector [n form], with n a node
  and form a well-formed formula. An edge is a vector [i src dest],
  where i is in Ind (see tableaux.syntax) and the node src connects to
  dest."
  (:require [clojure.core.reducers :as r]
            [tableaux.rewrite :as rw]))

(defn- tuple-count
  "If x is a sequential collection, returns the number of items in x;
  returns 0 otherwise."
  [x]
  (if (sequential? x)
    (count x)
    0))

(defn- nth-or-nil
  "If x is a sequential collection, returns the nth iten; returns nil
  otherwise. (Like get but also works for linked lists.)"
  [obj n]
  (try
    (nth obj n)
    (catch Exception x nil)))

(defn log-by-arity
  "Files tuples under the key [:by-arity n], where n is the number of
  items in the tuple. Non-tuples are filed under [:by-arity 0]."
  [x]
  {[:by-arity (tuple-count x)] #{x}})

(defn log-labels-by-prefix
  "Files tuples [x [y ...] ...] under the key [:labels-by-prefix y].
  Other items are filed under [:labels-by-prefix nil]."
  [x]
  (if (= 2 (tuple-count x))
    {[:labels-by-prefix (nth-or-nil (second x) 0)] #{x}}
    {}))

(defn log-labels-by-prefix2
  "Files tuples [x [y [z ...]...] ...] under the key
  [:labels-by-prefix2 y z]. For items that do not match this pattern,
  nil is substituted for z. For items that also do not match
  [x [y ...]...], nil is substituted for y."
  [x]
  (if (= 2 (tuple-count x))
    (let [outer-form (second x)
          inner-form (nth-or-nil outer-form 1)]
      {[:labels-by-prefix2
        (nth-or-nil outer-form 0)
        (nth-or-nil inner-form 0)]
       #{x}})
    {}))

(defn log-labels-by-node-prefix-infix
  "Files tuples [x [y z...]...] under the key
  [:labels-by-prefix-infix y z]. For items that do not match this
  pattern, nil is substituted for z. Items that do not match [x [y]...]
  are filed under [:labels-by-prefix-infix nil nil]."
  [x]
  (if (= 2 (tuple-count x))
    (let [form (second x)]
      {[:labels-by-node-prefix-infix
        (first x)
        (nth-or-nil form 0)
        (nth-or-nil form 1)]
       #{x}})
    {}))

(defn log-edges-by-idx-src
  "Files tuples [x? y?...] under [:edges-by-idx-src x y]. For non-tuples
  nil is substituted for x and y."
  [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-idx-src (first x) (second x)] #{x}}
    {}))

(defn log-edges-by-idx-dest
  "Files tuples [x? y? z?...] under [:edges-by-idx-dest x z]. For other
  items, nil is substituted for both x and z."
  [x]
  (if (= 3 (tuple-count x))
    {[:edges-by-idx-dest (first x) (nth x 2)] #{x}}
    {}))
