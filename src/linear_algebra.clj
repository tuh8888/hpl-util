(ns linear-algebra
  (:require [uncomplicate.commons.core :as uncomplicate]
            [uncomplicate.neanderthal.core :as thal]))

(defn unit-vec
  [{:keys [factory]} v]
  (uncomplicate/with-release [v (thal/vctr factory v)
                              alpha-1 (thal/nrm2 v)
                              alpha (/ alpha-1)
                              result (thal/scal alpha v)]
    (vec (seq result))))

(defn vec-sum
  [& vectors]
  (let [vectors (keep seq vectors)]
    (when (seq vectors)
      (->> vectors
           (apply map +)))))


(defn vectors->matrix
  [{:keys [factory]} vectors]
  (let [m (count (first vectors))
        n (count vectors)]
    (->> vectors
         (map seq)
         (flatten)
         (thal/ge factory m n))))

(defn mdot
  [params s1 s2]
  (uncomplicate/with-release [s1-mat (vectors->matrix params s1)
                              s1-mat-trans (thal/trans s1-mat)
                              s2-mat (vectors->matrix params s2)
                              result (thal/mm s1-mat-trans s2-mat)]
    #_(log/info #_(thal/mrows s1) #_(thal/ncols s1) #_(thal/mrows s2) #_(thal/ncols s2))
    (vec (doall (map vec result)))))

(defn find-best-match-in-row
  [row]
  (->> row
       (map-indexed (fn [j score] {:score score :i j}))
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
           (map-indexed (fn [i row]
                          (-> row
                              (find-best-match-in-row)
                              (assoc :j i))))
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

