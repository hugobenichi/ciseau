(ns ciseau.core
  (require  [clojure.string     :as string]
            [clojure.java.shell :as shell]))

(defn parse [x] (Integer/parseInt x))

(defn dimensions
  []
  (let [dims (shell/sh "/bin/sh" "-c" "stty size < /dev/tty")
        [h w] (map parse (string/split (string/trim-newline dims) #" "))]
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

(defn renderer [ctx]
  (let [{text :text, screen :screen} ctx]
    (.startScreen screen)
    (fn [ls]
      (.clear screen)
      (doseq [[r s] (map-indexed vector ls)]
        (.putString text 0 r s))
      (.refresh screen))))

(defn get-input [ctx]
  (let [{screen :screen} ctx]
    (fn []
      ; TODO: return a map with next key and screen size
      (.readInput screen))))

(defn read-file [path]
  (with-open [reader  (clojure.java.io/reader path)]
    (apply vector (line-seq reader))))

(defn to-buffer [path]
    ; TODO: compute max length to correctly offset on the left
    (let [numbers (map (partial format "%4d ") (iterate inc 0))
          lines   (read-file path)]
      (map str numbers lines)))

(defn editor-loop [editor model_zero]
  (try
    (loop [model model_zero]
      ((:render editor) model)
      (let [next_input ((:input editor))
            next_model ((:update editor) next_input model)]
        (if next_model
          (recur next_model) nil)))
    (finally ((:close editor)))))

(defn default_update [input model]
  nil)

(defn make-editor [ctx]
  {:render  (renderer ctx),
   :input   (get-input ctx),
   :update  default_update,
   :close   (fn [] (->> ctx :screen .stopScreen))})

(defn -main [f & other_args]
  (let [ctx (make-terminal)
        model (to-buffer f)]
    (try
      (editor-loop (make-editor ctx) model)
      (catch Exception e
        (println (.getMessage e))))))
