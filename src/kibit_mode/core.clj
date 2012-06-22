(ns kibit-mode.core
  (:require [kibit.check :as c]
            [kibit.core :as kc]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.util.regex Pattern)))

(defn detailed-read
  "Read a form from DetailedFormReader reader,
  attaching the following metadata to the form:

  :line - the line of the stream the form was found on
  :end-line - the last line of the stream containing the form
  :start-character - the first byte which describes the form
  :end-character - the last byte which describes the form

  Accepts all of the same arguments and arities as clojure.core/read."
  ([reader] (detailed-read reader true nil))
  ([reader eof-error?] (detailed-read reader eof-error? nil))
  ([reader eof-error? eof-value] (detailed-read reader eof-error? eof-value false))
  ([reader eof-error? eof-value recursive?]
     (let [form (clojure.core/read reader eof-error? eof-value recursive?)
           {start-line :line} (meta form)
           end-line (.getLineNumber reader)
           details (.resetStats reader)
           start-character (:start-character details)
           end-character (:current-character details)
           source (:source details)]
       (if (and (not eof-error?)
                (= form eof-value))
         eof-value
         (with-meta form {:line start-line
                          :end-line end-line
                          :start-character start-character
                          :end-character end-character
                          :source source})))))
(gen-interface
 :name kibit-mode.core.DetailedFormReader
 :methods [
           (resetStats [] clojure.lang.APersistentMap)
           (peekStats [] clojure.lang.APersistentMap)])

(defn whitespace?
  "Returns true if character is Clojure whitespace, false otherwise"
  [character]
  (try (let [character (char character)]
         (or (Character/isWhitespace character)
             (= character \,)))
       (catch Exception e
         false)))

(defn- read-update-fn
  "Update stats after reading the character result"
  [stats c]
  (let [new-stats (-> stats
                      (update-in [:total-read] inc)
                      (update-in [:start-character] (if (and (whitespace? c)
                                                             (:only-whitespace stats))
                                                      inc
                                                      identity))
                      (assoc-in [:only-whitespace] (and (:only-whitespace stats)
                                                        (whitespace? c))))
        character (try (char c) (catch Exception e nil))]
    (if (and character (not (:only-whitespace new-stats)))
      (update-in new-stats [:source-chars] conj character)
      new-stats)))

(defn- unread-update-fn
  "Update stats after unreading the character c"
  [stats c]
  (-> stats
      (update-in [:total-read] dec)
      (update-in [:start-character] (if (and (whitespace? c)
                                             (:only-whitespace stats))
                                      inc
                                      identity))
      (update-in [:source-chars] (if (:only-whitespace stats) identity rest))))

(defn- read-line-update-fn
  "Update stats after reading the line line"
  [stats line]
  (if line
    (-> stats
        (update-in [:total-read]
                   +
                   (inc (count line)))
        (update-in [:source-chars] concat (conj (reverse (seq line)) \newline))
        stats)))

(defn- reset-stats-update-fn
  "Reset stats based on the current state of reader"
  [stats reader]
  (-> stats
      (assoc-in [:start-character]
                (:total-read stats))
      (assoc-in [:only-whitespace]
                true)
      (assoc-in [:source-chars]
                '())))

(defn make-detailed-form-reader
  "Wraps LineNumberingPushbackReader with tracing data in order to
  enable detailed-read"
  [reader]
  (let [stats (atom {:only-whitespace true
                     :start-character 0
                     :total-read 0
                     :source-chars '()})]
    (proxy
        [clojure.lang.LineNumberingPushbackReader kibit-mode.core.DetailedFormReader]
        [reader]
      (getLineNumber [] (proxy-super getLineNumber))
      (read []
        (let [read-result (proxy-super read)]
          (swap! stats read-update-fn read-result)
          read-result))
      (unread [c]
        (proxy-super unread (int c))
        (swap! stats unread-update-fn (int c)))
      (readLine []
        (let [read-result (proxy-super readLine)]
          (when read-result
            (swap! stats read-line-update-fn read-result))
          read-result))
      (atLineStart []
        (proxy-super atLineStart))
      (resetStats []
        (let [peek (.peekStats this)]
          (swap! stats reset-stats-update-fn this)
          peek))
      (peekStats []
        {:current-line (.getLineNumber this)
         :start-character (:start-character @stats)
         :current-character (:total-read @stats)
         :source (apply str (reverse (:source-chars @stats)))}))))

(defn detailed-forms
  "Return a seq of all the top-level forms from detailed-form-reader,
  including detailed metadata for :line, :end-line, :start-character,
  and :end-character"
  [detailed-form-reader]
  (let [read-result (detailed-read detailed-form-reader false :eof)]
    (lazy-seq
      (when-not (= read-result :eof)
        (cons read-result
              (detailed-forms detailed-form-reader))))))

(defprotocol Detailable
  "Objects which can have detailed source information about them described."
  (source-match [this] "Returns a string specification of a Java
  regular expression which, when applied to strings, matches the
  portion of the string which would generate this when the string is
  read.")
  (detailed-meta [this parent] "Returns detailed metadata for this,
  which is contained in parent. parent must have detailed metadata as
  returned by detailed-read."))

(defn- -diagnose-mismatch
  [this parent]
  (let [source-match (source-match this)
        parent-source (-> parent meta :source)
        sub-pats (map #(.substring source-match 0 %) (range (count source-match)))
        pat-filt (fn [pattern] (try (re-find (Pattern/compile pattern (bit-and Pattern/MULTILINE Pattern/DOTALL)) parent-source)
                                    (catch Exception e false)))
        best-pat (last (filter pat-filt sub-pats))]
    (throw (ex-info (str "Failed to find a match for \"" this "\" in \"" (-> parent meta :source) "\"")
                   {:this this
                    :parent parent
                    :parent-source (-> parent meta :source)
                    :best-pat best-pat}))))

(defn- -detailed-meta
  [this parent]
  (let [{parent-start :start-character
         parent-end :end-character
         parent-source :source
         parent-line :line
         :as parent-meta} (meta parent)
         pattern-source (try (source-match this)
                             (catch clojure.lang.ExceptionInfo e
                               (throw (ex-info (str "Could not generate source-match for " (:class (ex-data e)))
                                               (merge (ex-data e)
                                                      {:parent-source parent-source})))))
         matcher (re-matcher (Pattern/compile pattern-source (bit-and Pattern/MULTILINE Pattern/DOTALL)) parent-source)
         matches (.find matcher)
         _ (when-not matches (-diagnose-mismatch this parent))
         this-start (.start matcher)
         this-end (.end matcher)
         source (.group matcher)
         preceeding-lines (count (re-seq #"\n" (.substring parent-source 0 this-start)))
         spanned-lines (count (re-seq #"\n" source))]
    (with-meta this {:line (+ parent-line preceeding-lines)
                     :start-character (+ parent-start this-start)
                     :end-character (+ parent-start this-end)
                     :end-line (+ parent-line preceeding-lines spanned-lines)
                     :source source})))

(let [wsc "[ \\t\\n\\x0B\\f\\r,]"
      any-ws (str wsc \*)
      some-ws (str wsc \+)]
  (extend-protocol Detailable
    java.lang.Object
    (source-match [this] (throw (ex-info (str "Tried to source-match unknown type: " (class this)) {:instance this :class (class this)})))
    (detailed-meta [this parent] (-detailed-meta this parent))
    clojure.lang.PersistentVector
    (source-match [this] (str "\\[" any-ws (apply str (interpose any-ws (map source-match this))) any-ws "\\]"))
    (detailed-meta [this parent] (-detailed-meta this parent))
    clojure.lang.PersistentList
    (source-match [this] (str "\\(" any-ws (apply str (interpose any-ws (map source-match this))) any-ws "\\)"))
    (detailed-meta [this parent] (-detailed-meta this parent))
    clojure.lang.MapEntry
    (source-match [this] (str (source-match (key this)) some-ws (source-match (val this))))
    clojure.lang.PersistentArrayMap
    (source-match [this] (str "\\{" any-ws (apply str (interpose any-ws (map source-match this))) any-ws "\\}"))
    (detailed-meta [this parent] (-detailed-meta this parent))
    clojure.lang.Cons
    (source-match [this] "FROGS")
                               ;; (throw (ex-info (str "Tried to source-match cons with unknown first element: " (first this))
                               ;;                 {:instance this
                               ;;                  :class (class this)
                               ;;                  :first (first this)
                               ;;                  :second (second this)}))))
    (detailed-meta [this parent] (-detailed-meta this parent))
    clojure.lang.Symbol
    (source-match [this] (if-let [space (namespace this)]
                           (str \\ \Q space \/ (name this) \\ \E)
                           (str \\ \Q (name this) \\ \E)))
    (detailed-meta [this parent] (-detailed-meta this parent))
    java.lang.Boolean
    (source-match [this] (str \\ \Q this \\ \E))
    (detailed-meta [this parent] this)
    clojure.lang.Keyword
    (source-match [this] (if-let [space (namespace this)]
                           (str \\ \Q \: space \/ (name this) \\ \E)
                           (str \\ \Q \: (name this) \\ \E)))
    (detailed-meta [this parent] this)
    java.lang.Character
    (source-match [this] (case this
                           \newline "\\Q\\newline\\E"
                           \tab "\\Q\\tab\\E"
                           \space "\\Q\\space\\E"
                           (str \\ \Q \\ this \\ \E)))
    (detailed-meta [this parent] this)
    java.lang.Long
    (source-match [this] (str \\ \Q this \\ \E))
    (detailed-meta [this parent] this)
    java.lang.String
    (source-match [this] (str \" \\ \Q this \\ \E \"))
    (detailed-meta [this parent] this)
    nil
    (source-match [this] "\\Qnil\\E")
    (detailed-meta [this parent] nil)))

(defn- detailed-exprs-with-meta
  [form]
  (tree-seq sequential? #(map (fn [child] (detailed-meta child %)) (seq %)) form))

(defn detailed-exprs
  "Return a seq of all the exprs in form. If form has detailed
  metadata (as is provided by detailed-read), each expr will also have
  the correct detailed metadata attached."
  [form]
  (if (= (set (keys (meta form))) #{:line :start-character :end-line :end-character :source})
    (detailed-exprs-with-meta form)
    (tree-seq sequential? seq form)))

(defn assoc-reporter
  "Given a kibit simplification map, print an emacs-lispy version of it."
  [file [expr simp :as simplify-pair]]
  (let [{:keys [line start-character end-character end-line source] :as expr-meta} (meta expr)]
    (println (str "'((file . \""
                  file
                  "\") (line . "
                  line
                  ") (start-char . "
                  start-character
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
  (let [{:keys [line start-character end-character end-line source] :as expr-meta} (meta expr)]
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
    (let [forms (detailed-forms (make-detailed-form-reader reader))
          exprs (mapcat detailed-exprs forms)
          simplifications (simplifications exprs)]
      (doseq [simplify-pair simplifications]
        (reporter file simplify-pair))
      simplifications)))

(defn -main
  [file]
  (when-not (empty? (check-file file assoc-reporter))
    (System/exit 1)))
