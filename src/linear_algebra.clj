(ns linear-algebra
  (:require [uncomplicate.commons.core :as uncomplicate]
            [uncomplicate.neanderthal.core :as thal]
            [taoensso.timbre :as log]))

(defn unit-vec
  [factory v]
  (uncomplicate/with-release [v       (thal/vctr factory v)
                              alpha-1 (thal/nrm2 v)
                              alpha   (/ alpha-1)
                              result  (thal/scal alpha v)]
    (doall (vec (seq result)))))

(defn vec-sum
  [& vectors]
  (let [vectors (keep seq vectors)]
    (when (seq vectors)
      (->> vectors
           (apply map +)))))

(defn vectors->matrix
  [factory vectors]
  (let [n       (count (first vectors))
        m       (count vectors)
        vectors (flatten vectors)]
    (thal/ge factory m n vectors {:layout :row})))

(defn mdot
  [factory s1 s2]
  (uncomplicate/with-release [s1-mat       (vectors->matrix factory s1)
                              s2-mat       (vectors->matrix factory s2)
                              s2-mat-trans (thal/trans s2-mat)]
    #_(log/info (thal/mrows s2-mat) (thal/ncols s2-mat) (thal/mrows s1-mat) (thal/ncols s1-mat))
    (uncomplicate/with-release [result (thal/mm s1-mat s2-mat-trans)]
      #_(log/info (thal/mrows result) (thal/ncols result))
      (vec (doall (map vec result))))))

(defn find-best-match-in-col
  [col]
  (->> col
       (map-indexed (fn [i score] {:score score :i i}))
       (apply max-key :score {:score 0})))

(defn find-best-col-matches
  [factory s1 s2]
  (uncomplicate/with-release [score-mat (mdot factory s1 s2)]
    #_(log/info (count score-mat) (count (first score-mat)))
    (when score-mat
      (->> score-mat
           (map-indexed vector)
           (pmap (fn [[j col]]
                   (-> col
                       (find-best-match-in-col)
                       (assoc :j j))))

           (doall)))))

(defn find-best-match
  [factory s1 s2]
  (->> (find-best-col-matches factory s1 s2)
       (apply max-key :score {:score 0})
       (doall)))