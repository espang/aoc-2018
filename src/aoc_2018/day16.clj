(ns aoc-2018.day16)

(def input 
  (->> "./resources/day16.txt"
       slurp
       (clojure.string/split-lines)
       (remove clojure.string/blank?)
       (partition 3)))

(defn parse-triple [[before instruction after]]
  [(read-string (subs before 7))
   (read-string (str "[" instruction "]"))
   (read-string (subs after 7))])


(defn addr [register [_ A B C]]
  (assoc register C (+ (register A) (register B))))

(defn addi [register [_ A B C]]
  (assoc register C (+ (register A) B)))

(defn mulr [register [_ A B C]]
  (assoc register C (* (register A) (register B))))

(defn muli [register [_ A B C]]
  (assoc register C (* (register A) B)))

(defn banr [register [_ A B C]]
  (assoc register C (bit-and (register A) (register B))))

(defn bani [register [_ A B C]]
  (assoc register C (bit-and (register A) B)))

(defn borr [register [_ A B C]]
  (assoc register C (bit-or (register A) (register B))))

(defn bori [register [_ A B C]]
  (assoc register C (bit-or (register A) B)))

(defn setr [register [_ A B C]]
  (assoc register C (register A)))

(defn seti [register [_ A B C]]
  (assoc register C A))

(defn gtir [register [_ A B C]]
  (assoc register C (if (> A (register B)) 1 0)))

(defn gtri [register [_ A B C]]
  (assoc register C (if (> (register A) B) 1 0)))

(defn gtrr [register [_ A B C]]
  (assoc register C (if (> (register A) (register B)) 1 0)))

(defn eqir [register [_ A B C]]
  (assoc register C (if (= A (register B)) 1 0)))

(defn eqri [register [_ A B C]]
  (assoc register C (if (= (register A) B) 1 0)))

(defn eqrr [register [_ A B C]]
  (assoc register C (if (= (register A) (register B)) 1 0)))

(def operations [addr addi mulr muli banr bani borr bori 
                 setr seti gtir gtri gtrr eqir eqri eqrr])

(defn test-operations [[before instructions after] operations]
  (reduce (fn [acc op]
            (if (= after (op before instructions))
              (conj acc op)
              acc))
          []
          operations))

; part 1
(->> input
     (map parse-triple)
     (map #(test-operations % operations))
     (map count)
     (filter #(> % 2))
     (count))

; part 2
(def op-map {:addr addr
             :addi addi
             :mulr mulr
             :muli muli
             :banr banr
             :bani bani
             :borr borr
             :bori bori
             :setr setr
             :seti seti
             :gtir gtir
             :gtri gtri
             :gtrr gtrr
             :eqir eqir
             :eqri eqri
             :eqrr eqrr})

(defn test-map [[before instructions after] op-map]
  [(first instructions)
   (into #{} (map first (filter (fn [[k op]] (= after (op before instructions))) op-map)))])

(def potential-op-mapping
  (->> input
       (map parse-triple)
       (map #(test-map % op-map))
       (reduce (fn [acc [nbr operations]] (update acc nbr clojure.set/intersection operations))
               (into {} (for [i (range 16)] [i (into #{} (keys op-map))])))))

(defn find-mapping [op-map]
  (->> op-map
       (filter #(= 1 (count (val %))))
       first))

(defn op-mapping [potential-mapping]
  (loop [in  potential-mapping
         ret {}]
    (if (empty? in)
      ret
      (let [[nbr ops] (find-mapping in)
            op        (first ops)]
        (recur (into {}
                     (map (fn [[k v]] [k (clojure.set/difference v ops)]) (dissoc in nbr)))
               (assoc ret nbr op))))))

(def nbr->fn (into {} (for [[nbr op-label] (op-mapping potential-op-mapping)]
                        [nbr (op-map op-label)])))

(def input2
  (->> "./resources/day16_2.txt"
       (slurp)
       (clojure.string/split-lines)
       (remove clojure.string/blank?)
       (map #(read-string (str "[" % "]")))))

(defn execute [instructions nbr->fn]
  (loop [register     [0 0 0 0]
         instructions instructions]
    (if (empty? instructions)
      register
      (let [[nbr _ _ _] (first instructions)]
        (recur ((nbr->fn nbr) register (first instructions))
               (rest instructions))))))

(execute input2 nbr->fn)
