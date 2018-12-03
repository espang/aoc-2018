(ns aoc-2018.day3)

(defn line->nbrs [pattern line]
  (map read-string (drop 1 (re-matches pattern line))))

(defn squares [start-x start-y width height]
  (for [x (range (inc start-x) (inc (+ start-x width)))
        y (range (inc start-y) (inc (+ start-y height)))]
    [x y]))

(defn handle-line [acc pattern line]
  (let [numbers (line->nbrs pattern line)
        [id x y width height] numbers
        squares (squares x y width height)]
    (reduce (fn [acc value]
              (update-in acc [value] conj id))
            acc
            squares)))

(defn lines->square-map [lines]
  (let [pattern (re-pattern #"#(\d*) @ (\d*),(\d*): (\d*)x(\d*)")]
    (reduce (fn [acc line]
              (handle-line acc pattern line))
            {}
            lines)))

(defn run_part1 []
  (let [input-file "resources/day3.txt"
        lines      (clojure.string/split-lines (slurp input-file))
        result     (lines->square-map lines)
        values     (vals result)
        n-squares  (count (filter #(> (count %) 1) values))]
    (println "the result is" n-squares)))


(defn run_part2 []
  (let [input-file "resources/day3.txt"
        lines      (clojure.string/split-lines (slurp input-file))
        result     (lines->square-map lines)
        values     (vals result)
        non-intact (reduce (fn [acc value]
                             (clojure.set/union acc (into #{} value)))
                           #{}
                           (filter #(> (count %) 1) values))
        intact     (clojure.set/difference (into #{} (range 1 1230))
                                           non-intact)]
   (println "the result is" intact)))
