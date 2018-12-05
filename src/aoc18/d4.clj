(ns aoc18.d4
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(defn get-cmd [[tm s]]
  (if-let [id (re-find #"\d+" s)]
    [:change (atoi id)]
    (let [[_ _ _ _ m] (get-num tm)]
      (if (re-find #"falls" s)
        [:sleep m]
        [:wake  m]))))

(defn q1 [l]
  (let [l (mapv get-cmd (sort-by first l))
        m (loop [m {}
                 l l
                 id nil
                 start -1]
            (if (empty? l)
              m
              (let [[cmd no] (first l)]
                (case cmd
                  :sleep (recur m (rest l) id no)
                  :change (recur m (rest l) no -1)
                  (recur (update m id concat (range start no))
                         (rest l) id -1)))))
        [id lst] (-> (sort-by (fn [[_ lst]] (count lst)) m) last)
        minute (first (last (sort-by second (frequencies lst))))]
    (* id minute)))

(defn q2 [l]
  (let [l (mapv get-cmd (sort-by first l))
        [mn _ id] (->> (loop [m {}
                              l l
                              id nil
                              start -1]
                         (if (empty? l)
                           m
                           (let [[cmd no] (first l)]
                             (case cmd
                               :sleep (recur m (rest l) id no)
                               :change (recur m (rest l) no -1)
                               (recur (update m id concat (range start no))
                                      (rest l) id -1)))))
                       (mapv (fn [[id lst]]
                               (-> (sort-by second (frequencies lst))
                                   last
                                   (conj id))))
                       (sort-by second) last)]
    (* id mn)))

(def in (get-line-split "]"))
(q1 in)
(q2 in)
