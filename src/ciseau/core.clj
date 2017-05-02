(ns ciseau.core)

(defn -main
  [& args]
  (def files (map slurp args))
  (doseq [f files]
    (println f)))
