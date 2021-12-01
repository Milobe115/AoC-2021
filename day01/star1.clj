(ns com.milopolitan.aoc
  (:require
   [clojure.java.io :as io]
   [clojure.core.match :as m :refer [match]])
  (:import java.lang.Integer))

(defn process-file
  [file]
  (with-open [rdr (io/reader file)]
    (into [] (line-seq rdr))))

(defn compare_lists [l]
  (match [l]
    [([hd he & tl] :seq)] (if (> he hd) (+ 1 (compare_lists (cons he tl))) (compare_lists (cons he tl)))
    :else 0)
)

(println (compare_lists (map #(Integer/parseInt %) (process-file "./input.txt"))))