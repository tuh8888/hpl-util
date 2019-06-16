(ns cluster-tools
  (:require [linear-algebra :as linear-algebra]
            [taoensso.timbre :as log]
            [util :as util]
            [uncomplicate.commons.core :as uncomplicate]))

(defn sample-cluster-scores
  [j col cluster-thresh]
  (->> col
       (map-indexed (fn [i score] {:i i :score score :j j}))
       (filter #(< cluster-thresh (:score %)))))

(defn update-score-cache
  [{:keys [factory cluster-thresh]} sample-vectors cluster-vectors score-cache offset]
  (if (seq cluster-vectors)
    (uncomplicate/with-release [score-mat (linear-algebra/mdot factory sample-vectors cluster-vectors)]
      (->> score-mat
           (map-indexed (fn [j col] (sample-cluster-scores (+ j offset) col cluster-thresh)))
           (apply concat score-cache)))
    score-cache))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [{:keys [cluster-merge-fn cluster-thresh vector-fn] :as params} samples clusters]
  (loop [samples         (vec samples)
         sample-vectors  (mapv vector-fn samples)
         clusters        (vec clusters)
         cluster-vectors (mapv vector-fn clusters)
         score-cache     nil
         offset          (if (seq clusters) 0 -1)]
    (if (seq samples)
      (let [score-cache    (update-score-cache params sample-vectors cluster-vectors score-cache offset)
            {:keys [i j]} (apply max-key :score {:score cluster-thresh :i 0} score-cache)
            sample         (get samples i)
            cluster        (get clusters j)
            samples        (if i (util/vec-remove samples i) samples)
            sample-vectors (if i (util/vec-remove sample-vectors i) sample-vectors)
            clusters       (if j (util/vec-remove clusters j) clusters)
            score-cache    (->> score-cache
                                (remove #(= (:i %) i))
                                (remove #(= (:j %) j))
                                (map #(update % :i (fn [s] (if (< i s) (dec s) s))))
                                (map #(update % :j (fn [c] (if (and j (< j c)) (dec c) c)))))
            new-cluster    (cluster-merge-fn params cluster sample)
            offset         (if j offset (inc offset))
            clusters       (conj clusters new-cluster)]
        (recur samples sample-vectors clusters [(vector-fn new-cluster)] score-cache offset))
      clusters)))