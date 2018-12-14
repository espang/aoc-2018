(ns aoc-2018.day12)

(def example-input "#..#.#..##......###...###")
(def example-notes
 #{"...##" "..#.." ".#..." ".#.#." ".#.##"
   ".##.." ".####" "#.#.#" "#.###" "##.#."
   "##.##" "###.." "###.#" "####."})

(def input "##..#..##....#..#..#..##.#.###.######..#..###.#.#..##.###.#.##..###..#.#..#.##.##..###.#.#...#.##..")
(def notes #{"#####" "##.##" "..###" "#..##" ".#.#." "#.#.#" "#..#." "##.#." ".##.#" ".#..#" ".#..." ".##.." "...##" "###.#" ".#.##"})

(defn pattern->state [pattern]
  (loop [v (mapv #(if (true? (second %)) \# \.) pattern)]
    (if (= \. (peek v))
      (recur (pop v))
      v)))

(defn next-step [notes state starting-idx]
  (let [parts (partition 5 1 (concat "...." state "...."))
        pattern (drop-while #(-> % second false?)
                            (map-indexed #(list %1 (contains? notes %2)) parts))
        [idx _] (first pattern)]
    [(pattern->state pattern)
     (+ starting-idx (- idx 2))]))

(defn state->result [state idx]
  (reduce +
          0
          (keep-indexed #(when (= \# %2) (+ idx %1)) state)))

(defn evolve [input notes steps]
  (let [notes (into #{} (map #(apply list %) notes))
        next-fn (partial next-step notes)]
    (loop [idx 0
           step 0
           last-points 0
           last-delta 0
           state input]
      (when (< step steps)
        (let [points              (state->result state idx)
              delta               (- points last-points)
              [new-state new-idx] (next-fn state idx)]
          (when (not= delta last-delta)
            (println step points delta))
          (recur new-idx (inc step) points delta new-state))))))


(evolve example-input example-notes 20)

(evolve input notes 20)

; run until "constant"
(evolve input notes 50000000000)

; after step 159 it grows by 86 per step
; after step 159 the sum is 16088
(+ 16088 (* 86 (- 50000000000 159)))
