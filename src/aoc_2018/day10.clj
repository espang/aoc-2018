(ns aoc-2018.day10)

; position=< 9,  1> velocity=< 0,  2>"
(def pattern
  (re-pattern #"position=<\s*(-{0,1}\d*),\s*(-{0,1}\d*)> velocity=<\s*(-{0,1}\d*),\s*(-{0,1}\d*)>"))

(def example-data (slurp "resources/day10_example.txt"))
(def answer1-data (slurp "resources/day10.txt"))

(defn matches->numbers [matches]
  (map #(Integer. (clojure.string/trim %)) (drop 1 matches)))

(defn data->lights [data]
  (let [lines   (clojure.string/split-lines data)
        matches (map #(re-matches pattern %) lines)
        lights  (map matches->numbers matches)]
    lights))

(defn lights->coordinates
 [lights]
 (map #(take 2 %) lights))

(defn lights->velocity
 [lights]
 (map #(drop 2 %) lights))

(defn inc-coordinates
 ([c velocity]
  (map #(apply list (map + %1 %2)) c velocity))
 ([c velocity by]
  (map #(map (reduce + %1 (repeat by %2))) c velocity)))

(defn bounds [c]
  [(apply min (map first c))
   (apply max (map first c))
   (apply min (map second c))
   (apply max (map second c))])

(defn printable-coordinates
  [c]
  (let [[min-x max-x min-y max-y] (bounds c)
        lookup (set c)]
    (if (> (Math/abs (- max-y min-y)) 18)
     ;(println "(" min-x "," min-y ") to (" max-x "," max-y ")")
     nil
     (loop [x min-x
            y min-y
            res []]
      (let [filler (if (contains? lookup (list x y))
                     \*
                     \_)]
       (if (> x max-x)
         (if (> y max-y)
           (apply str res)
           (recur min-x (inc y) (conj res \newline)))
         (recur (inc x) y (conj res filler))))))))

(defn print-lights [data]
 (let [l (data->lights data)
       c (lights-coordinates l)
       v (lights-velocity l)]
  (loop [step 0
         c    (inc-coordinates c v)]
   (if-let [res (printable-coordinates c)]
     (do
      (println "on step" (inc step))
      (println res))
     (recur (inc step) (inc-coordinates c v))))))

(print-lights answer1-data)
