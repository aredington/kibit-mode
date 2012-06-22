(ns kibit-mode.test-core
  (:use midje.sweet
        kibit-mode.core)
  (:require [clojure.java.io :as io]
            [clojure.set :as s]))

;;; detailed reader tests
(fact (#'kibit-mode.core/read-update-fn
       {:start-character 20
        :total-read 20
        :only-whitespace true
        :source-chars '()} 10) => {:start-character 21
                                   :total-read 21
                                   :only-whitespace true
                                   :source-chars '()})

(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))]
  ;; Read the first form from bogus.clj
  (dotimes [_ 20] (.read df-reader))
  (.resetStats df-reader)
  (let [read-result (.read df-reader)
        stats (.peekStats df-reader)]
    (fact (char read-result) => \newline)
    (fact (:start-character stats) => 21)))

;;; type checking
(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))]
  (fact (s/intersection
         (ancestors (class df-reader))
         #{clojure.lang.LineNumberingPushbackReader
           kibit-mode.core.DetailedFormReader}) => #{clojure.lang.LineNumberingPushbackReader
                                                     kibit-mode.core.DetailedFormReader}))

;;; metadata tests
(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))
      first-form (detailed-read df-reader)
      second-form (detailed-read df-reader)]
  (fact (:line (meta first-form)) => 1)
  (fact (:start-character (meta first-form)) => 0)
  (fact (:end-line (meta first-form)) => 1)
  (fact (:end-character (meta first-form)) => 20)
  (fact (:source (meta first-form)) => "(ns bogus-src.bogus)")
  (fact (:line (meta second-form)) => 3)
  (fact (:start-character (meta second-form)) => 22)
  (fact (:end-line (meta second-form)) => 6)
  (fact (:end-character (meta second-form)) => 95)
  (fact (:source (meta second-form)) =>
        "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))"))

;;; detailed-forms
(let [forms (detailed-forms (make-detailed-form-reader (io/reader "bogus_src/bogus.clj")))]
  (fact (map meta forms) =>
        [{:line 1
          :start-character 0
          :end-line 1
          :end-character 20
          :source "(ns bogus-src.bogus)"}
         {:line 3
          :start-character 22
          :end-line 6
          :end-character 95
          :source "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))" }]))

;;; detailed-exprs
(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))
      first-form (detailed-read df-reader)
      first-form-exprs (detailed-exprs first-form)
      second-form (detailed-read df-reader)
      second-form-exprs (detailed-exprs second-form)]
  (fact first-form-exprs => '((ns bogus-src.bogus) ns bogus-src.bogus))
  (fact (meta (nth first-form-exprs 0)) => {:line 1 :start-character 0 :end-line 1 :end-character 20 :source "(ns bogus-src.bogus)"})
  (fact (meta (nth first-form-exprs 1)) => {:line 1 :start-character 1 :end-line 1 :end-character 3 :source "ns"})
  (fact (meta (nth first-form-exprs 2)) => {:line 1 :start-character 4 :end-line 1 :end-character 19 :source "bogus-src.bogus"})
  (fact second-form-exprs => '((defn naive-adder "Who'd implement this this way?" [foo] (+ foo 1))
                               defn
                               naive-adder
                               "Who'd implement this this way?"
                               [foo]
                               foo
                               (+ foo 1)
                               +
                               foo
                               1))
  (fact (meta (nth second-form-exprs 0)) => {:line 3 :start-character 22 :end-line 6 :end-character 95 :source "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))"})
  (fact (meta (nth second-form-exprs 1)) => {:line 3 :start-character 23 :end-line 3 :end-character 27 :source "defn"})
  (fact (meta (nth second-form-exprs 2)) => {:line 3 :start-character 28 :end-line 3 :end-character 39 :source "naive-adder"})
  (fact (meta (nth second-form-exprs 3)) => nil) ;; Cannot tack metadata onto primitives
  (fact (meta (nth second-form-exprs 4)) => {:line 5 :start-character 77 :end-line 5 :end-character 82 :source "[foo]"})
  (fact (meta (nth second-form-exprs 5)) => {:line 5 :start-character 78 :end-line 5 :end-character 81 :source "foo"})
  (fact (meta (nth second-form-exprs 6)) => {:line 6 :start-character 85 :end-line 6 :end-character 94 :source "(+ foo 1)"})
  (fact (meta (nth second-form-exprs 7)) => {:line 6 :start-character 86 :end-line 6 :end-character 87 :source "+"})
  (fact (meta (nth second-form-exprs 8)) => {:line 6 :start-character 88 :end-line 6 :end-character 91 :source "foo"})
  (fact (meta (nth second-form-exprs 9)) => nil))

