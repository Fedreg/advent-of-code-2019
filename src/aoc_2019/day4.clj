(ns aoc-2019.day4
  (:require
   [clojure.string :as s]))

(defn digits [n]
  (->> n
       str
       (map str)
       (map #(Integer/parseInt %))))

(defn ascending? [[a b c d e f]]
  (<= a b c d e f))

(defn two-consecutive-equal? [[a b c d e f]]
  (cond
    (and (= a b)
         (and (not= a b c) 
              (not= a b d) 
              (not= a b e)
              (not= a b f))) true

    (and (= b c)
         (and (not= a b c) 
              (not= b c d) 
              (not= b c e) 
              (not= b c f))) true 

    (and (= c d)
         (and (not= a c d) 
              (not= b c d) 
              (not= c d e)
              (not= c d f))) true

    (and (= d e)
         (and (not= a d e) 
              (not= b d e) 
              (not= c d e)
              (not= d e f))) true

    (and (= e f)
         (and (not= a e f) 
              (not= b e f) 
              (not= c e f)
              (not= d e f))) true

    :else false))

(defn day4 [min max]
  (let [nums (range min (inc max))]
    (reduce (fn [cnt n]
              (let [digits (digits n)]
                (if (and (ascending?             digits)
                         (two-consecutive-equal? digits))
                  (inc cnt)
                  cnt)))
              0
              nums)))

(comment
  (day4 128392 643281)
  (two-consecutive-equal? [1 2 2 3 3 3])

  :end)
