(ns aoc-2018.day14)

(defn sum->new-recipes [n]
  (if (> n 9)
    [1 (mod n 10)]
    [n]))

(defn next-step
  ([]  {:recipes [3 7]
        :first   0
        :second  1})
  ([in]
   (let [f   (-> in :first)
         s   (-> in :second)
         rs  (-> in :recipes)
         fsc (nth rs f)
         ssc (nth rs s)
         n   (+ fsc ssc)
         rs' (sum->new-recipes n)
         l   (+ (count rs) (count rs'))
         f'  (mod (+ (inc f) fsc) l)
         s'  (mod (+ (inc s) ssc) l)]
     {:recipes (apply conj rs rs')
      :first   f'
      :second  s'})))

(defn after [x]
  (loop [in (next-step)]
    (let [rs (-> in :recipes)]
      (if (> (count rs) (+ x 10))
        (apply str (subvec rs x (+ x 10)))
        (recur (next-step in))))))

(assert (= (after 9) "5158916779"))
(assert (= (after 5) "0124515891"))
(assert (= (after 18) "9251071085"))
(assert (= (after 2018) "5941429882"))

(defn search-for [x]
  (loop [in (next-step)]
    (let [rs       (-> in :recipes)
          as-str   (apply str (subvec rs (max 0 (- (count rs) 7))))
          index-of (clojure.string/index-of as-str x)]
      (if (nil? index-of)
        (recur (next-step in))
        (+ (- (count rs) 7) index-of)))))

(assert (= (search-for "51589") 9))
(assert (= (search-for "01245") 5))
(assert (= (search-for "92510") 18))
(assert (= (search-for "59414") 2018))
