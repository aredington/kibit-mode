(ns kibit-mode.test-core
  (:use midje.sweet
        kibit-mode.core)
  (:require [clojure.java.io :as io]
            [clojure.set :as s]))

;;; detailed-forms
(let [forms (detailed-forms (io/reader "bogus_src/bogus.clj"))]
  (fact (map meta forms) =>
        [{:line 1
          :character 0
          :end-line 1
          :end-character 20
          :source "(ns bogus-src.bogus)"}
         {:line 3
          :character 22
          :end-line 6
          :end-character 95
          :source "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))" }]))

;;; detailed-exprs
(let [reader (io/reader "bogus_src/bogus.clj")
      [first-form second-form] (detailed-forms reader)
      first-form-exprs (detailed-exprs first-form)
      second-form-exprs (detailed-exprs second-form)]
  (fact first-form-exprs => '((ns bogus-src.bogus) ns bogus-src.bogus))
  (fact (meta (nth first-form-exprs 0)) => {:line 1 :character 0 :end-line 1 :end-character 20 :source "(ns bogus-src.bogus)"})
  (fact (meta (nth first-form-exprs 1)) => {:line 1 :character 1 :end-line 1 :end-character 3 :source "ns"})
  (fact (meta (nth first-form-exprs 2)) => {:line 1 :character 4 :end-line 1 :end-character 19 :source "bogus-src.bogus"})
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
  (fact (meta (nth second-form-exprs 0)) => {:line 3 :character 22 :end-line 6 :end-character 95 :source "(defn naive-adder
  \"Who'd implement this this way?\"
  [foo]
  (+ foo 1))"})
  (fact (meta (nth second-form-exprs 1)) => {:line 3 :character 23 :end-line 3 :end-character 27 :source "defn"})
  (fact (meta (nth second-form-exprs 2)) => {:line 3 :character 28 :end-line 3 :end-character 39 :source "naive-adder"})
  (fact (meta (nth second-form-exprs 3)) => nil) ;; Cannot tack metadata onto primitives
  (fact (meta (nth second-form-exprs 4)) => {:line 5 :character 77 :end-line 5 :end-character 82 :source "[foo]"})
  (fact (meta (nth second-form-exprs 5)) => {:line 5 :character 78 :end-line 5 :end-character 81 :source "foo"})
  (fact (meta (nth second-form-exprs 6)) => {:line 6 :character 85 :end-line 6 :end-character 94 :source "(+ foo 1)"})
  (fact (meta (nth second-form-exprs 7)) => {:line 6 :character 86 :end-line 6 :end-character 87 :source "+"})
  (fact (meta (nth second-form-exprs 8)) => {:line 6 :character 88 :end-line 6 :end-character 91 :source "foo"})
  (fact (meta (nth second-form-exprs 9)) => nil))

