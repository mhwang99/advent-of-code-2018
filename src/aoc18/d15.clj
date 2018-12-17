(ns aoc18.d15
  (:require [aoc18.core :as aoc :refer :all]))

(defn parse-map
  [l elf-power]
  (loop [yl l
         y 0
         m {}]
    (if (empty? yl)
      m
      (recur
        (rest yl) (inc y)
        (loop [xl (first yl)
               x 0
               m m]
          (if (empty? xl)
            m
            (recur
              (rest xl) (inc x)
              (case (first xl)
                \# (assoc m [y x] [:W])
                \G (assoc m [y x] [:G 3 200 -1])
                \E (assoc m [y x] [:E elf-power 200 -1])
                m))))))))

(defn get-next-ps
  [p]
  (mapv (fn [[ax ay] [bx by]]
          [(+ ax bx) (+ ay by)])
        [[-1 0] [0 -1] [0 1] [1 0]]
        (repeat p)))

(defn move
  [m p target]
  (loop [paths (set (mapv vector (get-next-ps p)))
         old #{p}]
    (if-let [ret (not-empty (filter #(= target (->> % last (get m) first)) paths))]
      (let [cnt (count (first ret))]
        (if (= cnt 1) p
          (ffirst (sort-by (fn [path] [(nth path (- cnt 2)) (first path)]) ret))))
      (let [old (apply conj old (mapv last paths))
            paths (remove #(->> % last (get m)) paths)]
        (if (empty? paths)
          p
          (recur (reduce (fn [paths path]
                           (->> (get-next-ps (last path))
                                (remove old)
                                (mapv (fn [p]
                                        (if (< (count path) 3)
                                          (conj path p)
                                          (vector (first path) (last path) p))))
                                (apply conj paths)))
                         #{} paths)
                 old))))))

(defn attack
  [m p target actor-power]
  (if-let [[tp [target power hp round]]
           (reduce (fn [[tp actor mn :as ret] p]
                     (let [actor (get m p)]
                       (if (and actor
                                (= target (first actor))
                                (or (nil? mn) (> mn (nth actor 2))))
                         [p actor (nth actor 2)]
                         ret)))
                   nil (get-next-ps p))]
    (let [hp (- hp actor-power)]
      (if (<= hp 0)
        (dissoc m tp)
        (assoc m tp [target power hp round])))
    (do
      m)))

(defn do-turn
  [m round p [atype power hp]]
  (let [target (if (= :G atype) :E :G)
        np (move m p target)]
    (-> m
        (dissoc p)
        (assoc np [atype power hp round])
        (attack np target power))))

(defn do-round
  [rows cols m fn-check]
  (loop [round 0
         m m]
    ;(println " R" round)
    (let [m (->> (for [y (range 1 (dec cols))
                       x (range 1 (dec rows))]
                   [y x])
                 (reduce (fn [m p]
                           (if-let [ret (fn-check round m)]
                             (reduced ret)
                             (let [[_ _ _ rnd :as actor] (get m p)]
                               (if (and rnd (< rnd round))
                                 (do-turn m round p actor)
                                 m))))
                         m))]
      (if (map? m)
        (recur (inc round) m)
        m))))

(defn q1 [l]
  (do-round
    (count l)
    (count (first l))
    (parse-map l 3)
    (fn [round m]
      (let [[types sum] (reduce (fn [[types sum] [_ [atype _ hp]]]
                                  [(conj types atype)
                                   (if hp (+ hp sum) sum)])
                                [#{} 0] m)]
        (when (< (count types) 3)
          (* round sum))))))

(defn q2 [l]
  (let [rows (count l)
        cols (count (first l))
        elf-count (reduce + (mapv #(count (filter (partial = \E) %)) l))]
    (loop [elf-power 4]
      ;(println "P" elf-power)
      (let [ret (do-round
                  (count l)
                  (count (first l))
                  (parse-map l elf-power)
                  (fn [round m]
                    (let [[gn en sum] (reduce (fn [[gn en sum] [_ [atype _ hp]]]
                                                (case atype
                                                  :G [(inc gn) en sum]
                                                  :E [gn (inc en) (+ sum hp)]
                                                  [gn en sum]))
                                              [0 0 0] m)]
                      (cond
                        (< en elf-count) :ERR
                        (= 0 gn) (* round sum)
                        :else nil))))]
        (if (keyword? ret)
          (recur (inc elf-power))
          ret)))))

(def in (get-split))
#_(time (q1 in))
#_(time (q2 in))
