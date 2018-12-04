(ns aoc-2018.day4)

(def input (slurp "resources/day4.txt"))

(defn txt->value [txt]
  (cond
    (clojure.string/ends-with? txt "up") {:action   :wakes-up}
    (clojure.string/ends-with? txt "asleep") {:action :falls-asleep}
    :else {:action   :begins-shift
           :guard-id (Integer. (second (re-find #"#(\d*)" txt)))}))

(defn combine [input]
  (->> input
       clojure.string/split-lines
       sort
       (map #(clojure.string/split % #" " 3))
       (reduce (fn [acc value]
                 (let [[date time txt] value
                       key (str date time)
                       minute (Integer. (subs time 3 5))
                       last-minute (:last-minute acc)
                       value (merge {:last-minute last-minute :minute minute :guard-id (:guard-id acc)}
                                    (txt->value txt))]
                   (if (= :wakes-up (:action value))
                     {:events   (conj (:events acc) [key value])
                      :guard-id (:guard-id value)
                      :last-minute minute}
                     {:events   (:events acc)
                      :guard-id (:guard-id value)
                      :last-minute minute})))
               {:events      (sorted-map)
                :guard-id    0
                :last-minute 0})
       :events
       vals
       (reduce (fn [acc value]
                 (let [guard-id (:guard-id value)
                       start    (:last-minute value)
                       end      (:minute value)]
                   (reduce #(update-in %1 [guard-id %2] (fnil inc 0)) acc (range start end))))
               {})
       (reduce (fn [acc kv]
                 (let [[k v] kv]
                   (conj acc [k {:sum    (reduce + (vals v))
                                 :minute (apply max-key val v)}])))
               {})))


(defn answer1 [input]
  (->> input
       combine
       (sort-by #(get-in (val %) [:sum]) >)
       first))


(defn answer2 [input]
  (->> input
       combine
       (sort-by #(first (get-in (val %) [:minute])) >)
       first))
