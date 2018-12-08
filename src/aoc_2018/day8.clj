(ns aoc-2018.day8)

; 2 3
;     0 3 10 11 12
;                  1 1
;                      0 1 99 2 1 1 2
  ; {:metadata [1 1 2]
  ;  :children [{:metadata [10 11 12]
  ;              :children nil}
  ;             {:metadata [2]
  ;              :children [{:metadata [99]
  ;                          :children nil}]}]})

(def example-data (list 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(def data
  (let [content   (clojure.string/trim (slurp "resources/day8.txt"))
        s-numbers (clojure.string/split content #" ")
        numbers   (into '() (reverse (map #(Integer. %) s-numbers)))]
    numbers))

(declare to-tree)

(defn retrieve-children [n values]
  (loop [retrieved 0
         children  []
         values    values]
    (if (= retrieved n)
      [children values]
      (let [[child not-used-values] (to-tree values)]
        (recur (inc retrieved) (conj children child) not-used-values)))))

(defn retrieve-metadata [n values]
  (loop [retrieved 0
         metadata  []
         values    values]
    (if (= retrieved n)
      [metadata values]
      (recur (inc retrieved)
             (conj metadata (peek values))
             (pop values)))))

(defn to-tree [values]
  (let [n-children (peek values)
        values     (pop values)
        n-metadata (peek values)
        values     (pop values)
        [children values] (retrieve-children n-children values)
        [metadata values] (retrieve-metadata n-metadata values)
        element           {:children children
                           :metadata metadata}]
    [element values]))

(defn sum-of-metadata [tree]
  (+ (reduce + (:metadata tree))
     (reduce + (map sum-of-metadata (:children tree)))))

(defn answer1 []
  (let [[tree _] (to-tree data)
        checksum (sum-of-metadata tree)]
    checksum))

;(map #(nth [1 2 3 4] %) (filter #(< % 4) (map dec [2 3 4 5])))

(defn value-of [tree]
  (let [children (:children tree)
        choosen  (map #(nth children %)
                      (filter #(< % (count children))
                              (map dec (:metadata tree))))]
    (if (empty? children)
      (reduce + (:metadata tree))
      (reduce (fn [acc child]
                (+ acc (value-of child)))
              0
              choosen))))

(defn answer2 []
  (let [[tree _] (to-tree data)
        value    (value-of tree)]
    value))
