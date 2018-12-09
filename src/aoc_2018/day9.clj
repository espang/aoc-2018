(ns aoc-2018.day9)

; current
; current --> 1 --> here --> 2
; (= 0 (mod current 23)) (not played and added)
;  7 <-- current (removed and added)
;  |-> new current

;446 players; last marble is worth 71522 points

(defn start [players]
  "start is after 9 has been inserted"
  {:data {:left [0 8 4]
          :current 9
          :right '(2 5 1 6 3 7)}
   :points (reduce (fn [a v] (assoc a v 0)) {} (range 0 players))})

(defn vec->list [v]
  (apply list v))

(defn remove-n-from-back [vec n])


(defn handle-mod-23 [value {:keys [left current right] :as data}]
  (if (< (count left) 7)
    ; this is a rare event -- don't bother (!!)
    (let [[right to-left] (split-at (- (count right) 7) right)]
      (handle-mod-23 value
                     {:left    (vec (concat to-left left))
                      :current current
                      :right   (apply list right)}))
    (let [to-right           (subvec left (- (count left) 5))
          left               (subvec left 0 (- (count left) 5))
          left               (vec left)
          n-current          (peek left)
          left               (pop left)
          points             (peek left)
          left               (pop left)
          right              (conj right current)
          right              (concat (vec->list to-right)
                                     right)]
      [(+ value points)
       {:left left
        :current n-current
        :right right}])))

(defn insert [value {:keys [left current right] :as data}]
  (cond
    (= 0 (mod value 23)) (handle-mod-23 value data)
    (empty? right) [0
                    {:left [(first left)]
                     :current value
                     :right (vec->list (rest (conj left current)))}]
    :else [0
           (-> data
             (update :left #(conj % current))
             (assoc :current value)
             (update :left #(conj % (first right)))
             (update :right #(drop 1 %)))]))

(defn play [players end]
  (reduce (fn [acc val]
            (let [player (mod val players)
                  [points data] (insert val (:data acc))]
              {:points (update (:points acc) player #(+ % points))
               :data data}))
          (start players)
          (range 10 (inc end))))

(defn get-result [players end]
  (let [result (play players end)]
    (val (first (sort-by val > (:points result))))))

(assert (= (get-result 9 25) 32))
(assert (= (get-result 10 1618) 8317))
(assert (= (get-result 13 7999) 146373))
(assert (= (get-result 17 1104) 2764))
(assert (= (get-result 21 6111) 54718))
(assert (= (get-result 30 5807) 37305))

(time (get-result 446 71522))
(time (get-result 446 7152200))
