(ns cluster-tools
  (:require [linear-algebra :as linear-algebra]
            [taoensso.timbre :as log]
            [uncomplicate.commons.core :as uncomplicate]))

(defn update-cluster
  [{:keys [cluster-merge-fn] :as params} clusters cluster sample]
  (let [clusters    (disj clusters cluster)
        new-cluster (cluster-merge-fn params cluster sample)]
    [new-cluster (conj clusters new-cluster)]))

(defn conj-cluster
  [{:keys [cluster-merge-fn] :as params} clusters sample]
  (let [new-cluster (cluster-merge-fn params nil sample)]
    [new-cluster (conj clusters new-cluster)]))

(defn sample-cluster-scores
  [cluster score-row samples]
  (pmap (fn [score sample]
          (when-not score (log/warn "No score found"))
          {:sample sample :score score :cluster cluster})
        score-row samples))

(defn update-score-cache
  [{:keys [factory vector-fn]} clusters samples score-cache]
  (if (seq clusters)
    (let [clusters (vec clusters)
          samples  (vec samples)]
      (uncomplicate/with-release [score-mat (linear-algebra/mdot factory
                                                                 (mapv vector-fn samples)
                                                                 (mapv vector-fn clusters))]
        (->> clusters
             (mapcat (fn [score-row cluster]
                       (sample-cluster-scores cluster score-row samples))
                     score-mat)
             (into score-cache))))
    score-cache))

(defn nearest-sample-cluster-pair
  [{:keys [cluster-thresh]} samples score-cache]
  (let [init {:score cluster-thresh :sample (first samples)}]
    (apply max-key :score init score-cache)))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [{:keys [cluster-merge-fn] :as params} samples clusters]
  (loop [samples      (set samples)
         clusters     (set clusters)
         score-cache  nil
         new-clusters clusters]
    (if (seq samples)
      (let [score-cache (update-score-cache params new-clusters samples score-cache)
            {:keys [cluster sample]} (nearest-sample-cluster-pair params samples score-cache)
            samples     (disj samples sample)
            score-cache (remove #(= (:cluster %) cluster) score-cache)
            new-cluster (cluster-merge-fn params cluster sample)
            clusters    (-> clusters
                            (disj clusters cluster)
                            (conj new-cluster))]
        (recur samples clusters score-cache [new-cluster]))
      clusters)))