(ns linear-algebra
  (:require [uncomplicate.commons.core :as uncomplicate]
            [uncomplicate.neanderthal.core :as thal]))

(defn unit-vec
  [{:keys [factory]} v]
  (uncomplicate/with-release [v (thal/vctr factory v)
                              alpha-1 (thal/nrm2 v)
                              alpha (/ alpha-1)
                              result (thal/scal alpha v)]
    (doall (vec (seq result)))))

(defn vec-sum
  [& vectors]
  (let [vectors (keep seq vectors)]
    (when (seq vectors)
      (->> vectors
           (apply map +)))))

(defn vectors->matrix
  [{:keys [factory]} vectors]
  (let [n (count (first vectors))
        m (count vectors)
        vectors (flatten vectors)]
    (thal/ge factory m n vectors {:layout :row})))

(defn mdot
  [params s1 s2]
  (uncomplicate/with-release [s1-mat (vectors->matrix params s1)
                              s2-mat (vectors->matrix params s2)
                              s2-mat-trans (thal/trans s2-mat)]
    #_(log/info (thal/mrows s1-mat) (thal/ncols s1-mat) (thal/mrows s2-mat) (thal/ncols s2-mat))
    (uncomplicate/with-release [result (thal/mm s1-mat s2-mat-trans)]
      #_(log/info (thal/mrows result) (thal/ncols result))
      (vec (doall (map vec result))))))

(defn find-best-match-in-row
  [row]
  (->> row
       (map-indexed (fn [j score] {:score score :j j}))
       (reduce (fn [best new]
                 (if (< (:score best) (:score new))
                   new
                   best))
               {:score 0})))


(defn find-best-row-matches
  [params s1 s2]
  (uncomplicate/with-release [score-mat (mdot params s1 s2)]
    (when score-mat
      (->> score-mat
           (map-indexed vector)
           (pmap (fn [[i row]]
                   (-> row
                       (find-best-match-in-row)
                       (assoc :i i))))
           (doall)))))


(defn find-best-match
  [params s1 s2]
  (->> (find-best-row-matches params s1 s2)
       (reduce (fn [best new]
                 (if (< (:score best) (:score new))
                   new
                   best))
               {:score 0})
       (doall)))

