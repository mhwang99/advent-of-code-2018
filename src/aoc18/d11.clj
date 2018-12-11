(ns aoc18.d11
  (:require [aoc18.core :refer :all]))

(def get-power
  (memoize
    (fn [x y serial size]
      (if (= size 1)
        (-> (+ x 10)
            (* y)
            (+ serial)
            (* (+ x 10))
            (quot 100)
            (mod 10)
            (- 5))
        (let [size1 (int (/ size 2))
              size2 (- size size1)]
          (+ (get-power x y serial size1)
             (get-power (+ x size2) (+ y size2) serial size1)
             (get-power (+ x size1) y serial size2)
             (get-power x (+ y size1) serial size2)
             (if (= size1 size2) 0
               (- 0 (get-power (+ x size1) (+ y size1) serial 1)))))))))

(defn q0
  [size-range serial]
  (->> (for [x (range 1 301)
             y (range 1 301)
             size size-range
             :when (and (<= (+ x size) 301)
                        (<= (+ y size) 301))]
         [x y size])
       (reduce (fn [ret [x y size]]
                 (let [p (get-power x y serial size)]
                   (if (> p (first ret))
                     [p [x y size]]
                     ret)))
               [-10])
       ((fn [[_ [x y size]]] (str x "," y "," size)))))

(def q1 (partial q0 [3]))
(def q2 (partial q0 (range 1 301)))

#_(time (q1 6392))
#_(time (q2 6392))
