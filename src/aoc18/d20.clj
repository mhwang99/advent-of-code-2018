(ns aoc18.d20
  (:require [aoc18.core :as aoc :refer :all]))

(defn pass
  [m [x y :as ap] c]
  (let [bp (case c
             \E [(inc x) y]
             \W [(dec x) y]
             \N [x (dec y)]
             \S [x (inc y)])]
    [bp (-> m
            (assoc ap (conj (get m ap #{}) bp))
            (assoc bp (conj (get m bp #{}) ap)))]))

(defn get-doors
  [s]
  (loop [psll (list (list #{[0 0]}) (list))
         m {}
         i 1]
    (let [c (get s i)]
      (case c
        \$ m
        \( (let [[psl] psll
                 [ps] psl]
             (recur (conj psll (list ps)) m (inc i)))
        \| (let [[sibling psl & psll] psll
                 [ps] psl]
             (recur (conj psll psl (conj sibling ps)) m (inc i)))
        \) (let [[sibling psl & psll] psll
                 [ps & psl] psl
                 sibling (apply clojure.set/union sibling)]
             (recur (conj psll (conj psl sibling)) m (inc i)))
        (let [[psl & psll] psll
              [ps & psl] psl
              [ps m] (reduce (fn [[ps m] p]
                               (let [[np m] (pass m p c)]
                                 [(conj ps np) m]))
                             [#{} m] ps)]
          (recur (conj psll (conj psl ps)) m (inc i)))))))

(defn get-large
  ([m] (get-large m -1))
  ([m limit]
   (loop [pass #{}
          cur #{[0 0]}
          i 0]
     (if (or (empty? cur)
             (>= i limit 0))
       [(dec i) (+ (count pass) (count cur))]
       (let [pass (clojure.set/union pass cur)
             nxt (reduce (fn [nxt p]
                           (reduce (fn [nxt np]
                                     (if (pass np)
                                       nxt
                                       (conj nxt np)))
                                   nxt (get m p)))
                         #{} cur)]
         (recur pass nxt (inc i)))))))

(defn q1 [s]
  (-> s get-doors get-large first))

(defn q2 [s i]
  (let [m (get-doors s)]
    (- (second (get-large m))
       (second (get-large m (dec i))))))

(q1 (get-res))
(q2 (get-res) 1000)
