(ns aoc-2018.day1)

(defn reduce-file [filename lines-handler]
  (with-open [rdr (clojure.java.io/reader filename)]
    (lines-handler (line-seq rdr))))

(defn lines-handler [lines]
  (->> lines
       (map read-string)
       (reduce +)))

(defn first-duplicate-in [values]
  (reduce (fn [acc value]
            (if (contains? acc value)
              (reduced value)
              (conj acc value)))
          #{}
          values))

(defn frequencies [lines]
  (->> lines
       cycle
       (map read-string)
       (reductions +)))

(defn run_part1 []
  (let [input-file "resources/day1.txt"
        result (reduce-file input-file lines-handler)]
    (println "the result is" result)))

(defn run_part2 []
  (let [input-file "resources/day1.txt"
        reduce-fn (fn [lines]
                    (let [frequencies (frequencies lines)
                          result (first-duplicate-in frequencies)]
                      result))
        first-duplicate (reduce-file input-file reduce-fn)]
    (println "the result is" first-duplicate)))
