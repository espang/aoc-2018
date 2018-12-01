(ns aoc-2018.day1)

(defn reduce-file [filename lines-handler]
  (with-open [rdr (clojure.java.io/reader filename)]
    (lines-handler (line-seq rdr))))

(defn lines-handler [lines]
  (->> lines
       (map read-string)
       (reduce +)))

(defn run_part1 []
  (let [input-file "resources/day1.txt"
        result (reduce-file input-file lines-handler)]
    (println "the result is" result)))
