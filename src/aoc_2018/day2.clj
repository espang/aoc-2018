(ns aoc-2018.day2)

(defn inc-when [pred counter kw]
  (if pred
    (update counter kw (fnil inc 0))
    counter))

(defn lines-handler [lines]
  (->> lines
       (reduce (fn [acc word]
                 (let [freqs  (frequencies word)
                       some2s (some #(= 2 %1) (vals freqs))
                       some3s (some #(= 3 %1) (vals freqs))
                       acc    (inc-when some2s acc :2s)
                       acc    (inc-when some3s acc :3s)]
                   acc))
               {})))

(defn run_part1 []
  (let [input-file "resources/day2.txt"
        lines      (clojure.string/split-lines (slurp input-file))
        result     (lines-handler lines)]
    (println "the result is" result (* (:2s result) (:3s result)))))

; PART 2
(defn similar [word1 word2]
  (let [tuples (partition 2 (interleave word1 word2))]
    (loop [chars-different 0
           tuples tuples]
      (if (empty? tuples)
        (<= chars-different 1)
        (if (> chars-different 1)
          false
          (recur (if (apply = (first tuples))
                   chars-different
                   (inc chars-different))
                 (rest tuples)))))))

(defn has-similar-word [word words]
  (loop [words words]
    (if (empty? words)
      false
      (let [word1        word
            word2        (first words)
            are-similar? (similar word1 word2)]
        (if are-similar?
          [word1 word2]
          (recur (rest words)))))))

(defn find-similar-in [words]
  (loop [word  (first words)
         words (rest words)]
    (if-let [similar-words (has-similar-word word words)]
      similar-words
      (recur (first words) (rest words)))))

(defn run_part2 []
  (let [input-file "resources/day2.txt"
        lines      (clojure.string/split-lines (slurp input-file))
        tuple      (find-similar-in lines)]
    (println "the result is" tuple)))
