(ns aoc-2018.day13)

;; ---- functions to print the map:
(defn find-corners [m]
  (let [points (keys m)
        min-x  (first (apply min-key #(first %) points))
        max-x  (first (apply max-key #(first %) points))
        min-y  (second (apply min-key #(second %) points))
        max-y  (second (apply max-key #(second %) points))]
    [min-x max-x min-y max-y]))

(defn print-row [row m min-x max-x]
  (for [x (range min-x (inc max-x))]
    (if-let [c (m [row x])]
      c
      \space)))

(defn print-header [min-x max-x]
  (for [x (range min-x (inc max-x))]
    (char (+ 48 (mod x 10)))))

(defn print-map [m]
 (let [[min-x max-x min-y max-y] (find-corners m)]
   (println "hdr" (apply str (print-header min-y max-y)))
   (loop [x min-x]
     (if (> x max-x)
       nil
       (do
         (println (format "%3d" x) (apply str (print-row x m min-y max-y)))
         (recur (inc x)))))))

(defn extract-direction [cars] (reduce-kv (fn [m k v] (assoc m k (first v))) {} cars))
(defn print-map-with-cars [m cars]
  (print-map (merge m (extract-direction cars))))

(defn print-map-with-cars-and-crashes [m cars crashes]
  (print-map (merge m
                    (extract-direction cars)
                    (reduce (fn [m k] (assoc m k \X)) {} crashes))))

; --- end

; car [0 0 \> :left]




(defmulti intersection :choice)
(defmethod intersection :left [{:keys [direction choice]}]
  (cond
    (= direction \^) [\< :straight]
    (= direction \<) [\v :straight]
    (= direction \v) [\> :straight]
    :else            [\^ :straight]))
(defmethod intersection :straight [{:keys [direction choice]}]
  [direction :right])
(defmethod intersection :right [{:keys [direction choice]}]
  (cond
    (= direction \^) [\> :left]
    (= direction \<) [\^ :left]
    (= direction \v) [\< :left]
    :else            [\v :left]))

(def direction->change
  {\^ [-1  0]
   \v [ 1  0]
   \> [ 0  1]
   \< [ 0 -1]})

(defn next-cell [direction cell]
  (mapv + cell (direction->change direction)))

(defn direction-transition [car cell]
  (let [[direction next-choice] car]
    (cond
      (= cell \-) car
      (= cell \|) car
      (= cell \/) (cond
                    (= \^ direction) [\> next-choice]
                    (= \v direction) [\< next-choice]
                    (= \< direction) [\v next-choice]
                    :else            [\^ next-choice])
      (= cell \\) (cond
                    (= \^ direction) [\< next-choice]
                    (= \v direction) [\> next-choice]
                    (= \< direction) [\^ next-choice]
                    :else            [\v next-choice])
      (= cell \+) (intersection {:choice next-choice
                                 :direction direction})
      :else :undefined)))

(defn move-car [[from car] m]
  (let [[direction _ ] car
        next-cell (next-cell direction from)]
    [next-cell (direction-transition car (m next-cell))]))

(def example-input (slurp "resources/day13_example.txt"))
(def input (slurp "resources/day13.txt"))

(defn line->map [[row line]]
  (into {} (keep-indexed #(when (not= \space %2) [[row %1] %2]) line)))

(defn lines->map [content]
  (into {} (map line->map (map-indexed vector (clojure.string/split-lines content)))))

(defn get-cars [m]
  (let [cars (filter #(contains? #{\v \^ \< \>} (val %)) m)]
    (reduce (fn [acc v] (assoc acc (key v) [(val v) :left]))
            (sorted-map)
            cars)))

(def replace-car
  {\^ \|
   \v \|
   \> \-
   \< \-})

(defn replace-cars [m]
  (merge m
         (->> m
              (filter #(contains? #{\v \^ \< \>} (val %)))
              (map (fn [kv] [(key kv) (replace-car (val kv))]))
              (into {}))))

(defn move-cars [m cars]
  (loop [moved-cars   {}
         crashes      []
         cars-to-move cars]
    (if (empty? cars-to-move)
      [(into (sorted-map) moved-cars)
       crashes]
      (let [car     (first cars-to-move)
            new-car (move-car car m)
            [pos _] new-car]
        (if (contains? moved-cars pos)
          (recur (dissoc moved-cars pos) (conj crashes pos) (rest cars-to-move))
          (recur (conj moved-cars new-car) crashes (rest cars-to-move)))))))

(def m (lines->map input))
(def cars (get-cars m))
(def m (replace-cars m))

(defn next-step [cars]
  (let [[cars crashes] (move-cars m cars)]
    (if (empty? crashes)
      (do
        (print-map-with-cars m cars)
        cars)
      (do
        (print-map-with-cars-and-crashes m cars crashes)
        (println crashes)))))

(defn until-accident [cars]
  (loop [step 0
         cars cars]
    (if (nil? cars)
      nil
      (do
        (println "step" step)
        (recur (inc step) (next-step cars))))))

(until-accident cars)
