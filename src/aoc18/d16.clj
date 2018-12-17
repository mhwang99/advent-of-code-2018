(ns aoc18.d16
  (:require [aoc18.core :as aoc :refer :all]))

(defn addr [[a b c] r] (assoc r c (+ (get r a 0) (get r b 0))))
(defn addi [[a b c] r] (assoc r c (+ (get r a 0) b)))
(defn mulr [[a b c] r] (assoc r c (* (get r a 0) (get r b 0))))
(defn muli [[a b c] r] (assoc r c (* (get r a 0) b)))
(defn banr [[a b c] r] (assoc r c (bit-and (get r a 0) (get r b 0))))
(defn bani [[a b c] r] (assoc r c (bit-and (get r a 0) b)))
(defn borr [[a b c] r] (assoc r c (bit-or (get r a 0) (get r b 0))))
(defn bori [[a b c] r] (assoc r c (bit-or (get r a 0) b)))
(defn setr [[a b c] r] (assoc r c (get r a 0)))
(defn seti [[a b c] r] (assoc r c a))
(defn gtir [[a b c] r] (assoc r c (bint (> a (get r b 0)))))
(defn gtri [[a b c] r] (assoc r c (bint (> (get r a 0) b))))
(defn gtrr [[a b c] r] (assoc r c (bint (> (get r a 0) (get r b 0)))))
(defn eqir [[a b c] r] (assoc r c (bint (= a (get r b 0)))))
(defn eqri [[a b c] r] (assoc r c (bint (= (get r a 0) b))))
(defn eqrr [[a b c] r] (assoc r c (bint (= (get r a 0) (get r b 0)))))

(def all-ops [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn check-ops
  [[r [_ & l] cr] ops]
  (filter (fn [f]
            (= (f l (vec r)) cr)) ops))

(defn q1
  [[part1]]
  (reduce (fn [cnt s]
            (if (>= (count (check-ops s all-ops)) 3)
              (inc cnt)
              cnt))
          0 part1))

(defn q2
  [[part1 part2]]
  (let [opm (->> (reduce
                   (fn [m s]
                     (let [opi (first (second s))
                           ops (check-ops s (get m opi))]
                       (if (= (count ops) 1)
                         (-> (reduce (fn [m [k v]]
                                       (assoc m k (remove #(= (first ops) %) v)))
                                     {} m)
                             (assoc opi ops))
                         (assoc m opi ops))))
                   (zipmap (range 16) (repeat all-ops)) part1)
                 (reduce-kv #(assoc %1 %2 (first %3)) {}))]
    (-> (reduce (fn [r [opi & l]]
                  ((get opm opi) l r))
                {} part2)
        (get 0))))

(def in
  (let [nl (get-line-num)
        part1 (->> nl
                   (partition 4)
                   (take-while #(seq (first %))))
        part2 (drop (+ (* 4 (count part1)) 2) nl)]
    [part1 part2]))

(q1 in)
(q2 in)
