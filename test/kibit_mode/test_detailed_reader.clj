(ns kibit-mode.test-detailed-reader
  (:use midje.sweet
        kibit-mode.detailed-reader)
  (:require [clojure.java.io :as io]
            [clojure.set :as s])
  (:refer-clojure :exclude [read]))

;;; detailed reader tests
(fact (#'kibit-mode.detailed-reader/read-update
       '({:start-character 20
          :total-read 20
          :only-whitespace true
          :source-chars ()}) 10) => '({:start-character 21
                                       :total-read 21
                                       :only-whitespace true
                                       :source-chars ()}))

(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))]
  ;; Read the first form from bogus.clj
  (.pushStatsFrame df-reader)
  (dotimes [_ 20] (.read df-reader))
  (.pushStatsFrame df-reader)
  (let [read-result (.read df-reader)
        stats (.peekStatsFrame df-reader)]
    (fact (char read-result) => \newline)
    (fact (:character stats) => 21)))

;;; type checking
(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))]
  (fact (s/intersection
         (ancestors (class df-reader))
         #{clojure.lang.LineNumberingPushbackReader
           kibit-mode.detailed-reader.DetailedFormReader}) => #{clojure.lang.LineNumberingPushbackReader
                                                     kibit-mode.detailed-reader.DetailedFormReader}))

;;; metadata tests
(let [df-reader (make-detailed-form-reader (io/reader "bogus_src/bogus.clj"))
      first-form (read df-reader)
      second-form (read df-reader)]
  (fact (:line (meta first-form)) => 1)
  (fact (:character (meta first-form)) => 0)
  (fact (:end-line (meta first-form)) => 1)
  (fact (:end-character (meta first-form)) => 20)
  (fact (:source (meta first-form)) => "(ns bogus-src.bogus)")
  (fact (:line (meta second-form)) => 3)
  (fact (:character (meta second-form)) => 22)
  (fact (:end-line (meta second-form)) => 6)
  (fact (:end-character (meta second-form)) => 95)
  (fact (:source (meta second-form)) =>
        "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))"))


