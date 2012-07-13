(ns kibit-mode.detailed-reader
  "Detailed reader attaches precise metadata to every form read."
  (:refer-clojure :exclude [read]))

(gen-interface
 :name kibit-mode.detailed-reader.DetailedFormReader
 :methods [(pushStatsFrame [] void)
           (peekStatsFrame [] clojure.lang.APersistentMap)
           (popStatsFrame [] clojure.lang.APersistentMap)])

(defn whitespace?
  "Returns true if character is Clojure whitespace, false otherwise"
  [character]
  (try (let [character (char character)]
         (or (Character/isWhitespace character)
             (= character \,)))
       (catch Exception e
         false)))

(defn- read-update
  "Update `stat-frames` after reading the character `c`"
  [stat-frames c]
  (let [frame (peek stat-frames)
        new-frame (-> frame
                      (update-in [:total-read] inc)
                      (update-in [:start-character] (if (and (whitespace? c)
                                                       (:only-whitespace frame))
                                                inc
                                                identity))
                      (assoc-in [:only-whitespace] (and (:only-whitespace frame)
                                                        (whitespace? c))))
        character (try (char c) (catch Exception e nil))
        new-frame (if (and character (not (:only-whitespace new-frame)))
                    (update-in new-frame [:source-chars] conj character)
                    new-frame)]
    (conj (pop stat-frames) new-frame)))

(defn- unread-update
  "Update `stat-frames` after unreading the character `c`"
  [stat-frames c]
  (let [frame (peek stat-frames)
        new-frame (-> frame
                      (update-in [:total-read] dec)
                      (update-in [:start-character] (if (and (whitespace? c)
                                                       (:only-whitespace frame))
                                                inc
                                                identity))
                      (update-in [:source-chars] (if (:only-whitespace frame) identity rest)))]
    (conj (pop stat-frames) new-frame)))

(defn- read-line-update
  "Update `stat-frames` after reading `line`"
  [stat-frames line]
  (if line
    (let [frame (peek stat-frames)
          new-frame (-> frame
                        (update-in [:total-read] + (inc (count line)))
                        (update-in [:source-chars] concat (conj (reverse (seq line)) \newline)))]
      (conj (pop stat-frames) new-frame))
    stat-frames))

(defn- push-stats-frame
  "Push a new stat frame onto the read stats frame stack `stat-frames`"
  [stat-frames]
  (let [top-frame (peek stat-frames)
        new-frame {:only-whitespace true
                   :start-character (:total-read top-frame)
                   :total-read (:total-read top-frame)
                   :source-chars '()}]
    (conj stat-frames new-frame)))

(defn- peek-stats-frame
  "Get the externally visible representation of a stat frame from `stat-frames` and `reader`"
  [stat-frames reader]
  (let [frame (peek stat-frames)]
    {:current-line (.getLineNumber reader)
     :character (:start-character frame)
     :current-character (:total-read frame)
     :source (apply str (reverse (:source-chars frame)))}))

(defn- pop-stats-frame
  "If `stats-frames` has more than one stat frame, pop the top-most
  stat frame, rolling its data into the next highest stat
  frame. Return a new stack of stat frames."
  [stat-frames]
  (if (> (count stat-frames) 2)
    (let [top-frame (peek stat-frames)
          next-frame (peek (pop stat-frames))
          new-top-frame {:only-whitespace (and (:only-whitespace next-frame)
                                               (:only-whitespace top-frame))
                         :start-character (:start-character next-frame)
                         :total-read (:total-read top-frame)
                         :source-chars (concat (:source-chars top-frame) (:source-chars next-frame))}]
      (conj (pop (pop stat-frames)) new-top-frame))
    stat-frames))

(defn make-detailed-form-reader
  "Wraps LineNumberingPushbackReader with tracing data in order to
  enable detailed-read"
  [reader]
  (let [stat-frames (atom '({:only-whitespace true
                             :start-character 0
                             :total-read 0
                             :source-chars ()}))]
    (proxy
        [clojure.lang.LineNumberingPushbackReader kibit-mode.detailed-reader.DetailedFormReader]
        [reader]
      (getLineNumber [] (proxy-super getLineNumber))
      (read []
        (let [read-result (proxy-super read)]
          (swap! stat-frames read-update read-result)
          read-result))
      (unread [c]
        (proxy-super unread (int c))
        (swap! stat-frames unread-update (int c)))
      (readLine []
        (let [read-result (proxy-super readLine)]
          (when read-result
            (swap! stat-frames read-line-update read-result))
          read-result))
      (atLineStart []
        (proxy-super atLineStart))
      (pushStatsFrame []
        (swap! stat-frames push-stats-frame))
      (peekStatsFrame []
        (peek-stats-frame @stat-frames this))
      (popStatsFrame []
        (let [return-frame (peek-stats-frame @stat-frames this)]
          (swap! stat-frames pop-stats-frame)
          return-frame)))))

(defn read
  "Read a form from PushbackReader reader,
  attaching the following metadata to the form:

  :line - the line of the stream the form was found on
  :end-line - the last line of the stream containing the form
  :start-character - the first byte which describes the form
  :end-character - the last byte which describes the form

  Accepts all of the same arguments and arities as clojure.core/read."
  ([reader] (read reader true nil))
  ([reader eof-error? eof-value] (read reader eof-error? eof-value false))
  ([reader eof-error? eof-value recursive?]
     (.pushStatsFrame reader)
     (let [form (clojure.core/read reader (boolean eof-error?) eof-value recursive?)
           {start-line :line} (meta form)
           end-line (.getLineNumber reader)
           details (.popStatsFrame reader)
           start-character (:character details)
           end-character (:current-character details)
           source (:source details)]
       (if (and (not eof-error?)
                (= form eof-value))
         eof-value
         (with-meta form {:line start-line
                          :end-line end-line
                          :character start-character
                          :end-character end-character
                          :source source})))))
