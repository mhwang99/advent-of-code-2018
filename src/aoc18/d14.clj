(ns aoc18.d14
  (:require [aoc18.core :refer :all]))

(defn q1
  [target]
  (loop [sboard [3 7]
         cnt 2
         idx (range 2)]
    (if (> cnt (+ target 9))
      (apply str (take 10 (drop target sboard)))
      (let [score (mapv (partial nth sboard) idx)
            no (apply + score)
            [sboard cnt] (if (< no 10)
                           [(conj sboard no) (inc cnt)]
                           [(conj sboard (quot no 10) (mod no 10)) (+ cnt 2)])
            idx (mapv (fn [s i] (mod (+ s i 1) cnt)) score idx)]
        (recur sboard cnt idx)))))

(defn q2
  [target]
  (let [target (mapv atoi (seq (str target)))
        tcnt (count target)]
    (loop [sboard [3 7]
           cnt 2
           chck [(repeat tcnt 0)]
           idx (range 2)]
      (if-let [tidx (when (>= cnt tcnt)
                      (cond
                        (= (first chck) target) (- cnt tcnt)
                        (= (second chck) target) (- cnt tcnt 1)
                        :else nil))]
        tidx
        (let [score (mapv (partial nth sboard) idx)
              no (apply + score)
              nol (if (< no 10) [no] [(quot no 10) (mod no 10)])
              sboard (apply conj sboard nol)
              chck (let [chck (first chck)]
                     (if (> (count nol) 1)
                       [(apply conj (vec (drop 2 chck)) nol)
                        (conj (vec (drop 1 chck)) (first nol))]
                       [(conj (vec (drop 1 chck)) (first nol))]))
              cnt (+ cnt (count nol))
              idx (mapv (fn [s i] (mod (+ s i 1) cnt)) score idx)]
          (recur sboard cnt chck idx))))))




