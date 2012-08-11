(ns kibit-mode.test-source-attribution
  (:use midje.sweet
        kibit-mode.source-attribution)
  (:require [clojure.core.logic :as l]))

(fact (match-info "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))" '(+ foo 1)) => ["(+ foo 1)" 65 74])

(fact (value-map '(:a :b [1 2]))
      =>
      [{:coords [0] :form '(:a :b [1 2]) :auth true}
       {:coords [0 0] :form :a :auth true}
       {:coords [0 1] :form :b :auth true}
       {:coords [0 2] :form [1 2] :auth true}
       {:coords [0 2 0] :form 1 :auth true}
       {:coords [0 2 1] :form 2 :auth true}])
