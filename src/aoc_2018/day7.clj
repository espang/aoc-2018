(ns aoc-2018.day7)

(defn tuples []
  "read the input into tuples of
   [s e] where s must be finished
   before e"
  (let [content (slurp "resources/day7.txt")
        lines   (clojure.string/split-lines content)
        start   (map #(subs % 5 6) lines)
        end     (map #(subs % 36 37) lines)]
    (partition 2 (interleave start end))))

(def steps "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn next-step [preconditions]
  (first (first (sort (filter #(empty? (val %)) preconditions)))))

(defn remove-work [preconditions work]
  (reduce (fn [acc v]
            (conj acc
                  [(first v) (disj (second v) work)]))
          {}
          preconditions))

(defn order [conditions]
  (loop [conditions conditions
         result     []]
    (if (empty? conditions)
      (apply str result)
      (let [step       (next-step conditions)
            conditions (dissoc conditions step)
            conditions (remove-work conditions step)]
        (recur conditions (conj result step))))))

(defn answer1 []
  (let [t          (tuples)
        conditions (into {} (map (fn [item] [(str item) #{}]) steps))
        conditions (reduce (fn [acc val]
                             (let [[s e] val]
                               (update acc e #(conj % s))))
                           conditions
                           t)]
    (order conditions)))

(def work-times (into {} (map-indexed (fn [idx item] [(str item) (+ 61 idx)]) steps)))

(defn possible-steps [preconditions]
  (count (filter #(empty? (val %)) preconditions)))

(defn schedule [preconditions]
  (loop [time        0
         work          {}
         preconditions preconditions
         result        []]
    (if (and (empty? work) (empty? preconditions))
      {:result (apply str result)
       :time   time}
      (if (or (= 5 (count work)) (= 0 (possible-steps preconditions)))
        ; all worker busy or no task available - finsih a task
        (let [[work-done new-time] (first (sort-by val work))]
          (recur new-time
                 (dissoc work work-done)
                 (remove-work preconditions work-done)
                 (conj result work-done)))
        (let [next-step (next-step preconditions)]
          ; schedule some work
          (recur time
                 (assoc work next-step (+ time (work-times next-step)))
                 (dissoc preconditions next-step)
                 result))))))

(defn answer2 []
  (let [inputs (tuples)
        preconditions (into {} (map (fn [item] [(str item) #{}]) steps))
        preconditions (reduce (fn [acc tuple]
                                (let [[before after] tuple]
                                  (update acc after #(conj % before))))
                              preconditions
                              inputs)]
    (schedule preconditions)))
