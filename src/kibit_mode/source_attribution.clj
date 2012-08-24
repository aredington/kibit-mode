(ns kibit-mode.source-attribution
  (:require [clojure.core.logic :as l])
  (:import (java.lang String Long Double Boolean Character)
           (java.math BigDecimal)
           (clojure.lang Keyword
                         Symbol
                         Ratio
                         IPersistentVector
                         IPersistentList
                         IPersistentSet
                         IPersistentMap)))

;;; NOTES:

;; Seems the best way to go is to map the input expr and source file
;; into the same coordinate space, validate what we can, and infer
;; what we can't.

;;e.g. The list (:a :b [1 2]) would generate the following vector of maps:

;; [{:coords [0] :form (:a :b [1 2]) :auth true}
;;  {:coords [0 0] :form :a :auth true}
;;  {:coords [0 1] :form :b :auth true}
;;  {:coords [0 2] :form [1 2] :auth true}
;;  {:coords [0 2 0] :form 1 :auth true}
;;  {:coords [0 2 1] :form 2 :auth true}]

;; meanwhile the string " (:a :b\n\n[1 2])" would map into the same
;; coordinate space with source attribution data

;; [{:coords [0] :source "(:a :b\n\n[1 2])" :auth true :start 1 :end 15 :start-line 0 :end-line 2}
;;  {:coords [0 0] :source ":a" :auth true :start 2 :end 4 :start-line 0 :end-line 0 }
;;  {:coords [0 1] :source ":b" :auth true :start 5 :end 7 :start-line 0 :end-line 0}
;;  {:coords [0 2] :source "[1 2]" :auth true :start 9 :end 14 :start-line 2 :end-line 2}
;;  {:coords [0 2 0] :source "1" :auth true :start 10 :end 11 :start-line 2 :end-line 2}
;;  {:coords [0 2 1] :source "2" :auth true :start 12 :end 13 :start-line 2 :end-line 2}]

;; Unify, go home, drink beer.

;; When assembling the maps of coordinate spaces, the assemblers will
;; flag their entries as authoritative or speculative. For instance,
;; assembling the map for the set #{:a :b} would generate the
;; following entries:

;;[{:coords [0] :form #{:a :b} :auth true}
;; {:coords [0 0] :form :a :auth false}
;; {:coords [0 1] :form :b :auth false}]

;; The string map that generated it CAN be authoritative regarding ordering.

;; Reader literals right now can only be a tag and
;; one value, which means there's no ambiguity about how a list of
;; string tokens should collapse to values, only which values they
;; collapse to

;; After generating the two maps, we can then validate the matches.

;; PROBLEM: A set containing precisely two reader literals that are unknown has
;; no ordering cues to hint at which value in the set came from which
;; reader literal. We could adaptively learn reader literal -> type
;; mappings, perhaps.

;; String map needs metadata. Maybe the values of both maps are
;; themselves maps and can indicate if they are authoritative or not.

(l/defrel collection-bounds type start-string end-string)
(l/facts collection-bounds
         [[IPersistentVector "[" "]"]
          [IPersistentList "(" ")"]
          [IPersistentSet "#{" "}"]
          [IPersistentMap "{" "}"]])

(defn whitespace?
  "Predicate returns true if `ch` is considered whitespace by the
  Clojure reader."
  [ch]
  (or (Character/isWhitespace (char ch)) (= \, ch)))

(defn contains-whitespace?
  [string]
  (some whitespace? (seq string)))

(defn vector-type-sourceo
  [source type truthiness]
  (println source type truthiness)
  (l/fresh [collection-start collection-end]
           (collection-bounds type collection-start collection-end)
           (l/project [source collection-start]
                      (l/== (.startsWith source collection-start) truthiness))
           (l/project [source collection-end]
                      (l/== (.endsWith source collection-end) truthiness))))

(l/defne vector-sourceo
  [source truthiness]
  ([_ true] (l/fresh [type]
                     (vector-type-sourceo source type truthiness)))
  ([_ false] #_(Check all collection-bounds to make sure that source doesn't satisfy any of them)))

(defn child-source-blockso
  [parent-source child-source child-source-start child-source-end]
  (l/conda ((l/all (vector-sourceo parent-source true)
                   (l/project [parent-source child-source child-source-start child-source-end]
                              (l/== (.substring parent-source child-source-start child-source-end) child-source))
                   #_(Ensure that the child block is the smallest matching block)))))

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

(defn form-map
  "Returns the form-map of structural metadata for `form`, which is the
  `index`ed child of a form described by the structural metadata map
  `parent-metadata`"
  [{:keys [coords] :as parent-metadata} form index]
  {:coords (conj coords index)
   :form form
   :auth (not (set? form))})

(defn value-map
  "Returns a vector of maps of structural metadata about `form`, each
  map containing the keys :coords, :form, and :auth specifying the
  hierarchical coordinates of the entry, the clojure form it is
  pertaining to, and if the returned information can be considered
  authoritative or not."
  ([form] (let [form-map {:coords [0] :form form :auth true}]
            (value-map form form-map)))
  ([current-form current-form-map]
     (if (coll? current-form)
       (conj (apply concat (map-indexed #(value-map %2 (form-map current-form-map %2 %1)) current-form))
             current-form-map)
       (list current-form-map))))

(defn coll-source?
  "Predicate. Tests if `source` describes a Clojure collection or not."
  [source]
  (not (empty? (l/run 1 [q] (vector-sourceo source true)))))

(defn source-block-map
  "Returns the source-map of structural metadata for `source-block`,
  which is the `index`ed child of a span of Clojure source described
  by the structural metadata map `parent-metadata`"
  [{:keys [coords start start-line] :as parent-metadata}
   [source block-start block-end block-start-line block-end-line :as source-block]
   index]
  {:coords (conj coords index)
   :source source
   :start (+ start block-start)
   :end (+ start block-end)
   :start-line (+ start-line block-start-line)
   :end-line (+ start-line block-end-line)})

(defn source-blocks
  "Returns the source blocks describing the immediate children of the
  form described by `source`. A source block is a vector of data
  containing, in order: source, start index, end index, start line,
  end line. All positional elements of the vector are relative to the
  parent."
  [source]
  )

(defn source-map
  "Returns a vector of maps of structural metadata about `source`,
  each map containing the keys :coords, :source, :auth, :start,
  :end, :start-line and :end-line specifying the hierarchical
  coordinates of the entry, the relevant portion of source pertaining
  to the entry, if the returned information can be considered
  authoritative, and positional information about the source in the
  file from which it came."
  ;; Assumptions:
  ;;
  ;; 1) The block of source provided is coming from metadata on a form
  ;; that was read; this means it's valid.
  ;;
  ;; 2) The block of source provided starts and ends on syntactically
  ;; significant characters.
  ([source] (let [start-map {:coords [0]
                             :source source
                             :auth false
                             :start 0
                             :end (count source)
                             :start-line 0
                             :end-line (count (re-seq #"\n" source))}]
              (source-map source start-map)))
  ([current-source current-source-map]
     (if (coll-source? current-source)
       (conj (apply concat (map-indexed #(source-map %2 (source-block-map current-source-map %2 %1)) (source-blocks current-source))
                    current-source-map))
       (list current-source-map))))

(defn match-meta
  "Return a map of sub-expression source attribution metadata, given
  the parent's source attribution metadata `parent-meta`, and a
  [`source-match` `starting-match-index` `ending-match-index`] vector
  `match-info` as returned from match-info. Returns an empty map if
  there is not sufficient match info."
  [{:keys [source line start-character] :as parent-meta} match-info]
  (if (empty? match-info)
    {}
    (let [[source-match starting-match-index ending-match-index] match-info
          start-line (+ line (count (re-seq #"\n" (.substring source 0 starting-match-index))))
          end-line (+ start-line (count (re-seq #"\n" source-match)))]
      {:start-character (+ start-character starting-match-index)
       :end-character (+ start-character ending-match-index)
       :line start-line
       :end-line end-line
       :source source-match})))

(defn source-of
  "Return `expr` with appropriately scoped source attribution metadata
  from `form` attached."
  [form expr]
  (if (= form expr)
    expr
    (let [{source :source :as parent-meta} (meta form)
           match-info (match-info source expr)
           meta (match-meta parent-meta match-info)]
      (spotty-meta expr meta))))

(defn detailed-exprs-with-meta
  "Given `form` with source attribution metadata, yield a recursive
  descent, fifo seq of all the exprs in `form` with appropriately
  scoped source attribution metadata attached"
  [form]
  (let [exprs (tree-seq sequential? seq form)]
    (map (partial source-of form) exprs)))
