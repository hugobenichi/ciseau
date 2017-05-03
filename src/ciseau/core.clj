(ns ciseau.core)

(defn sh [command]
  ; FIXME
  (with-out-str (util/cmd "/bin/sh" "-c" aommdn)))

(defn dimensions
  []
  (let [dims (sh "/bin/stty size </dev/tty")
        [h w] (map parse (str/split (clojure.string/trim-newline dims) #" "))]
    {:rows h :columns w}))

(defn -main
  [& args]
  (def files (map slurp args))
  (println (dimensions))
  (doseq [f files]
    (println f)))
