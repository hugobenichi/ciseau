(ns ciseau.core
  (require  [clojure.string     :as string]
            [clojure.java.shell :as shell]
            [clojure.stacktrace :as stacktrace]))

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

(defn lanterna-pos [vec2d]
  (let [[x y] vec2d]
    (new com.googlecode.lanterna.TerminalPosition x y)))

(defn render-cursor [screen model]
  ; TODO: also render cursor color
  (->> model :cursor lanterna-pos (.setCursorPosition screen)))

(defn renderer [ctx]
  (let [{text_obj :text, screen :screen} ctx]
    (.startScreen screen)
    ; TODO: introduce and render multiple layers of text
    (fn [model]
      (.clear screen)
      (doseq [[r s] (map-indexed vector (:text model))]
        (.putString text_obj 0 r s))
      (render-cursor screen model)
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
    (let [lines     (read-file path)
          line_cnt  (count lines)
          fm_str    (str '% (->> line_cnt Math/log10 int inc) "d ")
          numbers   (map (partial format fm_str) (iterate inc 0))]
      (map str numbers lines)))

(defn editor-main-loop [editor model previous]
  (when (= :exit model) :exit)
  (when (not (= model previous))
    ((:render editor) model))
  (recur
    editor
    ((:update editor) ((:input editor)) model)
    model))

(defn make-editor-loop [editor model_zero]
  (try
    (editor-main-loop editor model_zero nil)
    (finally ((:close editor)))))

(defn update_default [input model]
  model)

(defn cursor_update [f]
  (fn [model]
    ; TODO add cursor box bounding
    (conj model [:cursor (->> model :cursor (apply f))])))

(def cursor_right (cursor_update (fn [x y] [(inc x) y])))
(def cursor_left  (cursor_update (fn [x y] [(dec x) y])))
(def cursor_up    (cursor_update (fn [x y] [x (dec y)])))
(def cursor_down  (cursor_update (fn [x y] [x (inc y)])))

(defn lanterna-key [typ]
  (new com.googlecode.lanterna.input.KeyStroke typ))

(def input-handler {
  (lanterna-key com.googlecode.lanterna.input.KeyType/ArrowRight)  cursor_right
  (lanterna-key com.googlecode.lanterna.input.KeyType/ArrowLeft)   cursor_left
  (lanterna-key com.googlecode.lanterna.input.KeyType/ArrowUp)     cursor_up
  (lanterna-key com.googlecode.lanterna.input.KeyType/ArrowDown)   cursor_down
  })

(defn update_print_input [input model]
  (let [initial_text (:text model)
        updated_text (conj initial_text (str input))
        [x y]        (:cursor model)
        up_fn        (or (input-handler input) identity)]
    (conj (up_fn model) [:text updated_text])))

(defn make-editor [ctx]
  {:render  (renderer ctx),
   :input   (get-input ctx),
   :update  update_print_input, ;update_default,
   :close   (fn [] (->> ctx :screen .stopScreen))})

(defn -main [f & other_args]
  (let [model {:cursor  [0 0]
               :text    (to-buffer f)}]
    (try
      ((->> (make-terminal)
            make-editor
            (partial make-editor-loop)) model)
      (catch Exception e
        (stacktrace/print-cause-trace e)))))
