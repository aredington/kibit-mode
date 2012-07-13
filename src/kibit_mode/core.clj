(ns kibit-mode.core
  (:require [kibit.check :as c]
            [kibit.core :as kc]
            [kibit-mode.detailed-reader :as reader]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.util.regex Pattern)))

(defprotocol DetailedFormSource
  (detailed-forms [reader] "Return a seq of all the top-level forms
  from detailed-form-reader, including detailed metadata
  for :line, :end-line, :character, and :end-character"))

(extend-protocol DetailedFormSource
  kibit-mode.detailed-reader.DetailedFormReader
  (detailed-forms
    [reader]
    (let [read-result (reader/read reader false :eof)]
      (lazy-seq
        (when-not (= read-result :eof)
          (cons read-result
                (detailed-forms reader))))))
  Object
  (detailed-forms
    [reader]
    (detailed-forms (reader/make-detailed-form-reader reader))))

(defn detailed-exprs
  "Return a seq of all the exprs in form. If form has detailed
  metadata (as is provided by detailed-read), each expr will also have
  the correct detailed metadata attached."
  [form]
  (tree-seq sequential? seq form))

(defn assoc-reporter
  "Given a kibit simplification map, print an emacs-lispy version of it."
  [file [expr simp :as simplify-pair]]
  (let [{:keys [line character end-character end-line source] :as expr-meta} (meta expr)]
    (println (str "'((file . \""
                  file
                  "\") (line . "
                  line
                  ") (start-char . "
                  character
                  ") (end-line . "
                  end-line
                  ") (end-char . "
                  end-character
                  ") (source . "
                  (pr-str source)
                  ") (expr . \""
                  (pr-str expr)
                  "\") (replacement-exp . \""
                  (pr-str simp)
                  "\"))"))))

(defn report-error
  "Given a kibit-mode simplification, print the line number and
  normalized form of the expr and the replacement"
  [file [expr simp :as simplify-pair]]
  (let [{:keys [line character end-character end-line source] :as expr-meta} (meta expr)]
    (println (str file
                  ":"
                  line
                  ":\n  Replace\n    "
                  (pr-str source)
                  "\n  with\n    "
                  (pr-str simp)
                  ))))

(defn simplifications
  "Given a seq of exprs, check all of them against
  kibit.check/all-rules, returning a seq of pairs of exprs and their
  simplifications"
  [exprs]
  (letfn [(simp [expr] (kc/simplify-one expr c/all-rules))
          (pair [expr] (let [simp (simp expr)]
                         (when-not (= simp expr)
                           [expr simp])))]
    (filter (comp not nil?)
            (map pair exprs))))

(defn check-file
  [file reporter]
  (with-open [reader (io/reader file)]
    (let [forms (detailed-forms reader)
          exprs (mapcat detailed-exprs forms)
          simplifications (simplifications exprs)]
      (doseq [simplify-pair simplifications]
        (reporter file simplify-pair))
      simplifications)))

(defn -main
  [file]
  (when-not (empty? (check-file file assoc-reporter))
    (System/exit 1)))
