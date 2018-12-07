(ns aoc-2018.day6)

(def input (slurp "resources/day6.txt"))

(defn line->tuple [l]
  (clojure.string/split l #","))

(defn txt->int [s]
  (Integer. (clojure.string/trim s)))

(defn handle-line [line]
  (->> (line->tuple line)
       (map txt->int)))

(def centers
  (let [lines  (clojure.string/split-lines input)
        tuples (map handle-line lines)
        min-x  (apply min-key #(first %) tuples)
        max-x  (apply max-key #(first %) tuples)
        min-y  (apply min-key #(second %) tuples)
        max-y  (apply max-key #(second %) tuples)]
    (println min-x min-y max-x max-y)
    tuples))

(defn manhattan-distance [t1 t2]
  (reduce #(+ %1 (Math/abs %2)) 0 (map - t1 t2)))
;
; ; pointed search: search d1, d2, d3 ...
; ; vs: calculate all distances
; (defn distances [tuples]
;     (reduce (fn [acc val]
;               (let [[x y t] val
;                     distance (manhattan-distance [x y] t)]
;                 (update-in acc
;                            [[x y]]
;                            (fnil #(conj %1 {:distance distance
;                                             :tuple t})
;                                  []))))
;             {}
;             (for [x (range 40 354)
;                   y (range 41 355)
;                   t tuples]
;               [x y t])))
;
; (defn best [coll]
;   (let [sorted (sort-by :distance coll)
;         f (first sorted)
;         s (second sorted)]
;     (if (= (:distance f) (:distance s))
;       :undefined
;       (:tuple f))))
;
; (defn distances->best [distances]
;   (reduce (fn [acc val]
;             (conj acc [(first val) (best (second val))]))
;           {}
;           distances))

(defn possible-points [x-min x-max y-min y-max]
  (into #{}
        (for [x (range x-min (inc x-max))
              y (range y-min (inc y-max))]
          [x y])))

(defn start-points []
  [[0 0] [2 4] [4 1] [1 1]])

(defn neighbours [point]
 (into #{} (map #(map + point %) [[-1 0] [0 -1] [1 0] [0 1]])))

(defn reduce-points [result left distance last-distance-points]
  (let [n-result (reduce (fn [acc point]
                           (let [n (filter #(contains? left %) (neighbours point))]
                             (reduce (fn [sub-acc n-point]
                                       (if (contains? sub-acc n-point)
                                         (assoc sub-acc n-point :undefined)
                                         (assoc sub-acc n-point {:distance distance :point (get-in result [point :point])})))
                                     acc
                                     n)))
                         result
                         last-distance-points)
        handled-points (into #{} (keys n-result))
        n-points       (clojure.set/intersection left handled-points)
        left           (clojure.set/difference left handled-points)]
    (if (empty? left)
      n-result
      (recur n-result left (inc distance) n-points))))

(defn find-distances [points x-min x-max y-min y-max]
  ""
  (let [result      ()
        area-points (possible-points x-min x-max y-min y-max)]

    (into {} (for [p (filter #(contains? area-points %) points)] [p 0]))))



(defn answer1 [points]
  ""
  (find-distances points 40 353 41 354))

(defn next-step [possible-points distance points]
  "Returns a map of possible-point to [distance point]
   {[0 0] [1 [1 0]]
    [2 0] [1 [1 0]]
    [1 1] :undefined}
   by
   find all neighbours of all points, if only neighbour to one
   update the distance map, if neighbour to more update undefined"
  (let [neighbours]))


; matrix
; 40/41     353/41
;
; 40/354    353/354
; aaaaa.cccc
; aAaaa.cccc
; aaaddecccc
; aadddeccCc
; ..dDdeeccc
; bb.deEeecc
; bBb.eeee..
; bbb.eeefff
; bbb.eeffff
; bbb.ffffFf
;
