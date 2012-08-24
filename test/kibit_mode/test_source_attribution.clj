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

(fact (source-map "(\n\n:a\n\n:b\n\n  [\n    1\n    2\n  ]\n)")
      =>
      [{:coords [0] :source "(\n\n:a\n\n:b\n\n  [\n    1\n    2\n  ]\n)" :auth false :start 0 :end 32 :start-line 0 :end-line 10}
       {:coords [0 0] :source ":a" :auth false :start 3 :end 5 :start-line 2 :end-line 2}
       {:coords [0 1] :source ":b" :auth false :start 7 :end 9 :start-line 4 :end-line 4}
       {:coords [0 2] :source "[\n    1\n    2\n  ]" :start 13 :end 30 :start-line 6 :end-line 9}
       {:coords [0 2 0] :source "1" :start 19 :end 20 :start-line 7 :end-line 7}
       {:coords [0 2 1] :source "2" :start 25 :end 26 :start-line 8 :end-line 8}])
