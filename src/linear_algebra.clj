(ns linear-algebra
  (:require [uncomplicate.commons.core :as uncomplicate]
            [uncomplicate.neanderthal.core :as thal]
            [uncomplicate.neanderthal.real :as thal-real]))

(defn unit-vec
  [factory v]
  (uncomplicate/let-release [v (thal/vctr factory v)
                             alpha-1 (thal/nrm2 v)
                             alpha (/ alpha-1)
                             result (thal/scal alpha v)]
    (seq result)))

(defn cosine-sim
  [factory v1 v2]
  (uncomplicate/let-release [v1 (unit-vec factory v1)
                             v2 (unit-vec factory v2)
                             result (thal/dot v1 v2)]
    result))

(defn unit-vec-sum
  [factory & vectors]
  (let [vectors (keep seq vectors)]
    (if (seq vectors)
      (->> vectors
           (reduce (fn [v1 v2]
                     (if v1
                       (uncomplicate/let-release [v1 (thal/vctr factory v1)
                                                  v2 (thal/vctr factory v2)
                                                  sum-v (thal/xpy v1 v2)]
                         (seq sum-v))
                       v2))
                   nil)
           (unit-vec factory)))))


(defn vectors->matrix
  [{:keys [factory]} vectors]
  (let [d (some #(count %) vectors)]
    (->> vectors
         (mapcat seq)
         (thal/ge factory d (count vectors)))))

(defn mdot
  [{:keys [vector-fn] :as params} s1 s2]
  (let [v1 (map vector-fn s1)
        v2 (map vector-fn s2)]
    (uncomplicate/let-release [s1-mat (vectors->matrix params v1)
                               s1-mat-trans (thal/trans s1-mat)
                               s2-mat (vectors->matrix params v2)]
      #_(log/info #_(thal/mrows s1) #_(thal/ncols s1) #_(thal/mrows s2) #_(thal/ncols s2))
      (thal/mm s1-mat-trans s2-mat))))


(defn best-in-row-from-matrix
  [score-mat init i s]
  (->> s
       (map-indexed vector)
       (reduce
         (fn [{:keys [score] :as best} [j s]]
           (uncomplicate/let-release [new-score (thal-real/entry score-mat i j)]
             (if (< score new-score)
               {:match s :score (float new-score)}
               best)))
         init)))

(defn find-best-match
  [params s1 s2]
  (uncomplicate/let-release [score-mat (mdot params s1 s2)]
    (when score-mat
      (->> s1
           (map-indexed vector)
           (reduce
             (fn [best [i sample]]
               (-> (best-in-row-from-matrix score-mat best i s2)
                   (assoc :sample sample)))
             {:score 0})))))

(defn find-best-row-matches
  [params s1 s2]
  (uncomplicate/let-release [score-mat (mdot params s1 s2)]
    (when score-mat
      (->> s1
           (map-indexed (fn [i sample]
                          (-> score-mat
                              (best-in-row-from-matrix {:score 0} i s2)
                              (assoc :sample sample))))
           (doall)))))
