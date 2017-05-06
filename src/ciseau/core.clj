(ns ciseau.core
  (require  [clojure.string     :as string]
            [clojure.java.shell :as shell]))

(defn sh [command]
  ; FIXME
  (with-out-str (util/cmd "/bin/sh" "-c" aommdn)))

(defn dimensions
  []
  (let [dims (sh "/bin/stty size </dev/tty")
        [h w] (map parse (str/split (clojure.string/trim-newline dims) #" "))]
    {:rows h :columns w}))

(defn seq_with_keys [keys vals]
  (into {} (map vector keys vals)))

(defn get-now [] (System/currentTimeMillis))

(defn calc-fps [last-timestamp]
  (let [now (get-now)]
    [now (/ 1000.0 (- now last-timestamp))]))

(defn space_split [s] (string/split s #" "))

(defn tty-dims []
  (->> (shell/sh "/bin/sh" "-c" "stty size < /dev/tty")
       :out
       string/trim-newline
       space_split
       (map read-string)
       (seq_with_keys [:raws :columns])))

(defn make-terminal []
  (let [factory (new com.googlecode.lanterna.terminal.DefaultTerminalFactory)
        term    (.createTerminal factory)
        screen  (new com.googlecode.lanterna.screen.TerminalScreen term)
        text    (.newTextGraphics screen)]
    {:term    term
     :screen  screen
     :text    text}))

(defn with-terminal [f]
  (let [ctx (make-terminal)
        screen (:screen ctx)]
		(.startScreen screen)
		(.clear screen)
    (f ctx)
		(.startScreen screen)))

(defn put-strings [ls ctx]
  (let [{text :text, screen :screen} ctx]
    (doseq [[r s] (map-indexed vector ls)]
      (.putString text 0 r s))
    (.refresh screen)
    (.readInput screen)))

(defn read-file [path]
  (with-open [reader  (clojure.java.io/reader path)]
    (apply vector (line-seq reader))))

(defn to-buffer [path]
    ; TODO: compute max length to correctly offset on the left
    (let [numbers (map (partial format "%4d ") (iterate inc 0))
          lines   (read-file path)]
      (map str numbers lines)))

(defn -main
  [f & other_args]
    (with-terminal (partial put-strings (to-buffer f))))
