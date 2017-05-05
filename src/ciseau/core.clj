(ns ciseau.core)

(require  'clojure.string
          'clojure.java.shell)

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

(defn space_split [s] (clojure.string/split s #" "))

(defn tty-dims []
  (->> (clojure.java.shell/sh "/bin/sh" "-c" "stty size < /dev/tty")
       :out
       clojure.string/trim-newline
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
  (doseq [[r s] (map-indexed vector ls)]
    (.putString (:text ctx) 0 r (str r))
    (.putString (:text ctx) 3 r s))
	(.refresh (:screen ctx))
	(.readInput (:screen ctx)))

(defn -main
  [f & other_args]
  (with-open [reader (clojure.java.io/reader f)
              viewer (partial put-strings (line-seq reader))]
    (with-terminal viewer)))
