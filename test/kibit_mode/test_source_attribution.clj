(ns kibit-mode.test-source-attribution
  (:use midje.sweet
        kibit-mode.source-attribution)
  (:require [clojure.core.logic :as l]))

(fact (match-info "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))" '(+ foo 1)) => ["(+ foo 1)" 65 74])
