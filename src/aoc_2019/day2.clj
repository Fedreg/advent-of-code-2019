(ns aoc-2019.day1
  (:require
   [clojure.string :as s]))

(def input1 
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,19,6,23,2,13,23,27,1,9,27,31,2,31,9,35,1,6,35,39,2,10,39,43,1,5,43,47,1,5,47,51,2,51,6,55,2,10,55,59,1,59,9,63,2,13,63,67,1,10,67,71,1,71,5,75,1,75,6,79,1,10,79,83,1,5,83,87,1,5,87,91,2,91,6,95,2,6,95,99,2,10,99,103,1,103,5,107,1,2,107,111,1,6,111,0,99,2,14,0,0])

(defn opcode [arr f [a b i]]
  (assoc arr i (f (or a 0) (or b 0))))

(defn day2
  [{:keys [in test noun verb]}]
  (let [input (if test
                in
                (assoc in 1 noun 2 verb))]
    (loop [i   0
           arr input]
      (when (< i (count in))
        (if (= 99 (nth arr i))
          arr
          (do
            (let [new-arr (case (nth arr i)
                            1 (opcode arr + (mapv #(nth arr %) [(nth arr (+ i 1))
                                                                (nth arr (+ i 2))
                                                                (+ i 3)]))
                            2 (opcode arr * (mapv #(nth arr %) [(nth arr (+ i 1))
                                                                (nth arr (+ i 2))
                                                                (+ i 3)]))
                            arr)]
              (recur (if (or (= 1 (nth arr i))
                             (= 2 (nth arr i)))
                       (+ i 4)
                       (inc i))
                     new-arr))))))))

(comment
  (use 'clojure.test)
  (day2 {:in input1 :noun 12 :verb 2})
  (first (day2 {:in input1 :noun 64 :verb 29}))
  ;; 19690720
  (+ (* 100 64) 29)

  (is (= [2,0,0,0,99]          (day2 {:in [1,0,0,0,99]          :test true})))
  (is (= [2,3,0,6,99]          (day2 {:in [2,3,0,3,99]          :test true})))
  (is (= [2,4,4,5,99,9801]     (day2 {:in [2,4,4,5,99,0]        :test true})))
  (is (= [30,1,1,4,2,5,6,0,99] (day2 {:in [1,1,1,4,99,5,6,0,99] :test true})))

  :end)
