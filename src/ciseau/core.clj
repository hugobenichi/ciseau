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
      (.putString text 0 r (str r))
      (.putString text 3 r s))
    (.refresh screen)
    (.readInput screen)))

(defn get-now [] (System/currentTimeMillis))

(defn calc-fps [last-timestamp]
  (let [now (get-now)]
    [now (/ 1000.0 (- now last-timestamp))]))

(defn bench-put-strings [ls ctx]
  (let [start (get-now)
        {text :text, screen :screen} ctx]
    (loop [[last-ts fps] [1 1]]
      (.putString text 0 0 (str fps))
      (doseq [[r s] (map-indexed vector ls)]
        (.putString text 0 (inc r) (str r))
        (.putString text 3 (inc r) s))
      (.refresh screen)
      (if (> (- (get-now) start) 3000)
        (.readInput screen)
        (recur (calc-fps last-ts))))))

(defn -main
  [f & other_args]
  (with-open [reader (clojure.java.io/reader f)
              viewer (partial bench-put-strings (apply vector (line-seq reader)))]
              ;viewer (partial put-strings (line-seq reader))]
    (with-terminal viewer)))
