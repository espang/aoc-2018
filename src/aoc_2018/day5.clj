(ns aoc-2018.day5)

(def input (slurp "resources/day5.txt"))

(defn react? [c1 c2]
  (and
    (not (= c1 c2))
    (= (clojure.string/lower-case c1)
       (clojure.string/lower-case c2))))

(defn reduce-queue [q]
  (cond
    (= 0 (count q)) q
    (= 1 (count q)) q
    (= 2 (count q)) (if (react? (first q) (second q))
                      (clojure.lang.PersistentQueue/EMPTY)
                      q)
    (= 3 (count q)) (if (react? (second q) (nth q 2))
                      (conj (clojure.lang.PersistentQueue/EMPTY) (peek q))
                      q)
    :else q))

(defn reduce-input [input]
  (reduce
    (fn [acc value]
      (let [q (:queue acc)
            q (conj q value)
            q (reduce-queue q)
            r (:result acc)]
        (if (and
              (= 1 (count q))
              (> (count r) 0))
          {:queue (conj (clojure.lang.PersistentQueue/EMPTY) (peek r) (first q))
           :result (pop r)}
          (if (= 3 (count q))
            {:result (conj r (peek q))
             :queue (pop q)}
            {:result r
             :queue q}))))
    {:result []
     :queue (clojure.lang.PersistentQueue/EMPTY)}
    input))

(defn answer1 [input]
  (let [step1 (reduce-input input)
        q     (:queue step1)
        q     (reduce-queue q)
        res   (filter #(not= \newline %) (concat (:result step1) q))]
    (println "unit remain after redaction:" (count res))))

(defn answer2 [input]
  (for [l "abcdefghijklmnopqrstuvwxyz"
        u "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        :when (react? l u)]
    (let [f-input (filter #(not (contains? #{l u} %)) input)
          step1 (reduce-input f-input)
          q     (:queue step1)
          q     (reduce-queue q)
          res   (filter #(not= \newline %) (concat (:result step1) q))]
        (println "Removed " l ". units remain after redaction:" (count res)))))
