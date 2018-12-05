(ns aoc18.core
  (:require [clojure.string :as s]))

(defn get-res []
  (->> (s/split (str *ns*) #"\.")
       second
       (#(str "resources/" % ".txt"))
       slurp drop-last (apply str)))

(defn get-split
  ([]
   (get-split (get-res)))
  ([s]
   (s/split s #",|\n")))

(defn get-line-split
  ([delim]
   (get-line-split (get-res) delim))
  ([s delim]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (s/split l (re-pattern delim)))))))

(defn atoi
  [s]
  (Integer. (str s)))

(defn bint [s]
  (if s 1 0))

(defn get-num
  ([]
   (get-num (get-res)))
  ([s]
   (get-num s false))
  ([s minus?]
   (->> s
        (re-seq (if minus? #"[+-]?\d+" #"\d+"))
        (mapv atoi))))

(defn get-line-num
  ([]
   (get-line-num (get-res)))
  ([s]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (->> (re-seq #"[+-]?\d+" l)
                     (mapv atoi)))))))

