(ns aoc-2018.day11)

(defn power-level
  [x y serial]
  (let [rack-id (+ x 10)
        nbr     (* (+ (* y rack-id) serial) rack-id)]
    (- (mod (quot nbr 100) 10) 5)))

(defn initialize-grid [serial]
  (into []
    (for [x (range 1 301)
          y (range 1 301)]
      (power-level x y serial))))

(defn index->xy
  ([idx] (index->xy idx 300))
  ([idx size] [(inc (mod idx size)) (inc (quot idx size))]))

(defn xy->index
  ([x y] (xy->index x y 300))
  ([x y size] (+ (dec x) (* size (dec y)))))

(defn indexes [idx square]
  (for [x (range 0 square)
        y (range 0 square)]
    (+ idx x (* 300 y))))

(defn power-grid [grid square]
  (into []
    (for [x (range 1 (- 302 square))
          y (range 1 (- 302 square))]
      (let [idx  (xy->index x y)
            idxs (indexes idx square)]
        (reduce (fn [acc idx]
                  (+ acc (nth grid idx)))
                0
                idxs)))))

(defn best-val-index [grid]
  (apply max-key first (map-indexed (fn [idx itm] [itm idx]) grid)))

(defn best-coordinate-val [grid square]
  (let [grid       (power-grid grid square)
        [val best] (best-val-index grid)
        size       (int (Math/sqrt (count grid)))
        [x y]      (index->xy best size)]
    [x y val]))

(defn best-coordinate [serial]
  (let [[x y _] (best-coordinate-val (initialize-grid serial) 3)]
    [x y]))

; check power levels
(assert (= (power-level 3 5 8) 4))
(assert (= (power-level 122 79 57) -5))
(assert (= (power-level 217 196 39) 0))
(assert (= (power-level 101 153 71) 4))

; check examples
(assert (= [21 61] (best-coordinate 42)))
(assert (= [33 45] (best-coordinate 18)))

(assert (= [33 45 29] (best-coordinate-val (initialize-grid 18) 3)))
(assert (= [21 61 30] (best-coordinate-val (initialize-grid 42) 3)))

; answer 1:
(println "Answer1: " (best-coordinate-val (initialize-grid 7689) 3))

; for answer 2:
(assert (= [90 269 113] (best-coordinate-val (initialize-grid 18) 16)))
(assert (= [232 251 119] (best-coordinate-val (initialize-grid 42) 12)))

(defn answer2 [serial]
  (let [grid (initialize-grid serial)]
    (loop [square       1
           current-best 0]
      (let [[x y best] (best-coordinate-val grid square)
            new-best   (if (> best current-best) best current-best)]
        (when (> best current-best)
          (println "Current-best" square x y best))
        (if (> square 300)
          new-best
          (recur (inc square) new-best))))))
