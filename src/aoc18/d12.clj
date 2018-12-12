(ns aoc18.d12
  (:require [aoc18.core :refer :all]))

(defn get-next [p rule]
  (let [np (reduce (fn [l i]
                     (conj l (get rule (take 5 (drop (- i 2) p)) \.)))
                   [] (range 2 (- (count p) 2)))
        sidx (count (take-while #(= % \.) np))
        eidx (count (take-while #(= % \.) (reverse np)))
        np (drop sidx (drop-last eidx np))]
    [(concat "..." np "...") sidx]))

(defn get-val [p sidx]
  (let [sidx (- sidx 3)]
    (reduce +
            (map (fn [c n]
                   (if (= c \#) n 0))
                 p (range sidx (+ sidx (count p)))))))

(defn q0
  [target p rule]
  (loop [p (concat "..." p "...")
         n 1
         sidx 0]
    (if (> n target)
      (get-val p sidx)
      (let [[np nsidx] (get-next p rule)]
        (if (= p np)
          (let [prev (get-val p sidx)]
            (-> (- (get-val p (+ sidx -1 nsidx))
                   prev)
                (* (- (inc target) n))
                (+ prev)))
          (recur np (inc n) (+ sidx -1 nsidx)))))))

(def q1 (partial q0 20))
(def q2 (partial q0 50000000000))

(def initial (drop 15 (first (get-split))))
(def rule (into {}
                 (keep (fn [s]
                         (let [[k v] (get-split s " => ")]
                           (when (= v "#")
                             [(seq (.trim k)) (first v)])))
                       (drop 2 (get-split)))))
(time (q1 initial rule))
(time (q2 initial rule))
