(ns aoc-2018.day6)

(defn manhattan-distance [t1 t2]
  (reduce #(+ %1 (Math/abs %2)) 0 (map - t1 t2)))

(def input (slurp "resources/day6.txt"))

(defn line->tuple [l]
  (clojure.string/split l #","))

(defn txt->int [s]
  (Integer. (clojure.string/trim s)))

(defn handle-line [line]
  (->> (line->tuple line)
       (map txt->int)))

(defn starting-points []
  (let [lines  (clojure.string/split-lines input)
        points (into #{} (map handle-line lines))]
    points))

(defn find-corners [points]
  (let [min-x  (first (apply min-key #(first %) points))
        max-x  (first (apply max-key #(first %) points))
        min-y  (second (apply min-key #(second %) points))
        max-y  (second (apply max-key #(second %) points))]
    [min-x max-x min-y max-y]))

(defn neighbours [point]
 (into #{} (map #(map + point %) [[-1 0] [0 -1] [1 0] [0 1]])))

(defn reduce-points [result left distance last-distance-points]
  (let [n-result (reduce (fn [acc point]
                           (let [root-p (get-in result [point :point])
                                 n      (filter #(contains? left %) (neighbours point))]
                             (reduce (fn [sub-acc n-point]
                                       (if (contains? sub-acc n-point)
                                         ; the point already exists - check if we come from the same root
                                         (if (= root-p (get-in sub-acc [n-point :point]))
                                           (assoc sub-acc n-point {:distance distance :point root-p})
                                           (assoc sub-acc n-point :undefined))
                                         (assoc sub-acc n-point {:distance distance :point root-p})))
                                     n
                                     acc)))
                         result
                         last-distance-points)
        handled-points (into #{} (keys n-result))
        n-points       (clojure.set/intersection left handled-points)
        left           (clojure.set/difference left handled-points)]
    (if (empty? left)
      n-result
      (recur n-result left (inc distance) n-points))))

(defn create-initial-result [points]
  (reduce (fn [acc val]
            (assoc acc val {:distance 0 :point val}))
          {}
          points))

(defn possible-points [x-min x-max y-min y-max]
  (into #{}
        (for [x (range x-min (inc x-max))
              y (range y-min (inc y-max))]
          [x y])))

(defn bounds [x-min x-max y-min y-max]
  (clojure.set/union
    (into #{}
          (for [x [x-min x-max]
                y (range y-min (inc y-max))]
            [x y]))
    (into #{}
          (for [x (range x-min (inc x-max))
                y [y-min y-max]]
            [x y]))))

(defn answer1 []
  (let [starting-points           (starting-points)
        [min-x max-x min-y max-y] (find-corners starting-points)
        result                    (create-initial-result starting-points)
        possible-points           (possible-points min-x max-x min-y max-y)
        possible-points           (clojure.set/difference possible-points starting-points)
        result                    (reduce-points result possible-points 1 starting-points)
        points-on-the-border      (bounds min-x max-x min-y max-y)
        infinite-points           (reduce conj #{} (map #(get-in result [% :point]) points-on-the-border))
        groups                    (reduce #(update %1 (:point (second %2)) (fnil inc 0)) {} result)
        finite-points             (filter #(not (contains? infinite-points (key %))) groups)
        answer                    (last (sort-by second finite-points))]
    answer))

(defn sum-of-distance [point points distance-fn]
  (reduce #(+ %1 (distance-fn point %2)) 0 points))

(defn into-map [all-points coordinates]
  (into {}
        (map (fn [i] [i (sum-of-distance i coordinates manhattan-distance)])
             all-points)))

(defn answer2 []
  (let [starting-points           (starting-points)
        [min-x max-x min-y max-y] (find-corners starting-points)
        possible-points           (possible-points min-x max-x min-y max-y)
        valid-locations           (filter #(< (val %) 10000)
                                          (into-map possible-points starting-points))]
    (count valid-locations)))
