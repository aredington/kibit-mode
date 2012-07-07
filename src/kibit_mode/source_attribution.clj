(ns kibit-mode.source-attribution
  (:require [clojure.core.logic :as l])
  (:import (java.lang String Long Double Boolean Character)
           (java.math BigDecimal)
           (clojure.lang Keyword
                         Symbol
                         Ratio
                         PersistentVector
                         PersistentList
                         PersistentHashSet
                         PersistentArrayMap)))

;; Matched-by maps a type to a fn returning a regexp matching an instance of that type.

(l/defrel matched-by type matcher-fn)
(l/facts matched-by
         [[Object (fn [object] (re-pattern (str "\\Q" (pr-str object) "\\E")))]])

(l/defrel collection-bounds type start-string end-string)
(l/facts collection-bounds
         [[PersistentVector "[" "]"]
          [PersistentList "(" ")"]
          [PersistentHashSet "#{" "}"]
          [PersistentArrayMap "{" "}"]])


;; Scalars are values that the reader may return, and represent a
;; single scalar value, in direct opposition to collections.

(defn scalar-value?
  [instance]
  (l/project [instance]
             (l/== false (coll? instance))))

;; Vectors are values that the reader may return, and represent zero
;; or more scalar values grouped together, potentially with some
;; ordering, e.g. collections.

(defn vector-value?
  [instance]
  (l/project [instance]
             (l/== true (coll? instance))))

;; Primitives are values that cannot have metadata associated with
;; them. All primitives should be scalars. Not all scalars are
;; primitive.

(defn primitive-value?
  [instance]
  (l/project [instance]
             (l/== false (isa? (class instance) clojure.lang.IObj))))

;; Due to projections this goal will likely only work with instance
;; grounded. Still valuable as there may be many different ways to
;; express the same value, e.g. the Long 48 can be expressed as the
;; string "48" or as the string "14r36"

(defn matching-regexpo
  [instance regexp]
  (l/fresh [re-gen]
           (l/conde ((scalar-value? instance)
                     (matched-by Object re-gen)
                     (l/project [instance re-gen]
                                (l/== regexp (re-gen instance)))))))

;; Goal to relate a expression (an object read from Clojure's reader)
;; and a source block with precise indices in the source block the
;; describe where and only where that expression was read from.

(defn scalar-sourceo
  [expr source source-match start-index end-index]
  (l/fresh [regexp]
           (scalar-value? expr)
           (matching-regexpo expr regexp)
           (l/project [source regexp]
                      (l/== source-match (re-find regexp source)))
           (l/project [source regexp]
                      (l/== [start-index end-index]
                            (let [matcher (re-matcher regexp source)
                                  _ (.find matcher)
                                  start (.start matcher)
                                  end (.end matcher)]
                              [start end])))))

(defn vector-sourceo
  [expr source source-match start-index end-index]
  (l/fresh [collection-type collection-start collection-end]
   (vector-value? expr)
   (l/project [expr]
              (l/== collection-type (class expr)))
   (collection-bounds collection-type collection-start collection-end)
   (l/project [source collection-start]
              (l/== start-index (.indexOf source collection-start)))
   (l/project [source collection-end]
              (l/== end-index (inc (.indexOf source collection-end))))
   (l/project [source start-index end-index]
              (l/== source-match (.substring source start-index end-index)))))

(defn sourceo
  [expr source source-match start-index end-index]
  (l/conde ((scalar-sourceo expr source source-match start-index end-index))
           ((vector-sourceo expr source source-match start-index end-index))))

(defprotocol SpottyMeta
  (spotty-meta [value meta] "Returns `value` with `meta` associated if
  it implements IObj, `value` unchanged otherwise"))

(extend-protocol SpottyMeta
  clojure.lang.IObj
  (spotty-meta
    [this meta]
    (with-meta this meta))
  java.lang.Object
  (spotty-meta
    [this meta]
    this))

(defn source-of
  [form expr]
  (if (= form expr)
    expr
    (let [{source :source
           line :line
           start-char :start-character}  (meta form)
          match-hit (l/run 1 [q] (l/fresh [source-match start-index end-index]
                                (l/== q [source-match start-index end-index])
                                (sourceo expr source source-match start-index end-index)))
          meta {:start-character (+ start-char match-start)
                :end-character (+ start-char match-end)
                :line (+ line (count (re-seq #"\n" (.substring source 0 match-start))))
                :end-line (+ line (count (re-seq #"\n" source-match)))
                :source source-match}]
      (spotty-meta expr meta))))

(defn detailed-exprs-with-meta
  "Given `form` with source attribution metadata, yield a recursive
  descent, fifo seq of all the exprs in `form` with appropriately
  scoped source attribution metadata attached"
  [form]
  (let [exprs (tree-seq sequential? seq form)]
    (map (partial source-of form) exprs)))
