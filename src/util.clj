(ns util
  (:require [clojure.set :as cset]
            [taoensso.timbre :as log]
            [com.climate.claypoole :as cp])
  (:import (clojure.lang IDeref IBlockingDeref IFn IPending RT)
           (java.util Collection ArrayList Random Collections)))

(defn deterministic-shuffle
  [seed ^Collection coll]
  (let [al (ArrayList. coll)
        rng (Random. seed)]
    (Collections/shuffle al rng)
    (RT/vector (.toArray al))))

(defn map-kv
  [f m]
  (zipmap (keys m)
          (map f (vals m))))

(defn pmap-kv
  [f m]
  (zipmap (keys m)
          (pmap f (vals m))))

(defn parse-int
  [x]
  (try (Integer/parseInt x)
       (catch NumberFormatException _
         x)))

(defn find-matches
  [coll1 coll2 match-fn]
  (filter
    (fn [s1]
      (some
        (fn [s2]
          (match-fn s1 s2))
        coll2))
    coll1))

(defn lowercase?
  [^Character char]
  (or (not (Character/isLetter char))
      (Character/isLowerCase char)))
(defn all-lowercase
  [^String string]
  (and (every? lowercase? string) string))

(defn recursively
  [fn arg]
  (loop [results (fn arg)
         others (set results)
         completed #{}]
    (let [completed (cset/union completed results)
          others (cset/difference others results)]
      (if (empty? others)
        completed
        (recur (fn (first others)) (rest others) completed)))))

(defn pdoseq-partitioned
  [f m partition-size]
  (let [partitioned-m (partition-all partition-size m)
        num-parts (count partitioned-m)]
    (cp/pdoseq (inc (cp/ncpus))
      [[i part] (map-indexed vector partitioned-m)]
      (log/info "Partition:" i "/" num-parts)
      (f part))))

(defn promise? [v]
  (every? #(instance? % v)
          [IPending
           IFn
           IBlockingDeref
           IDeref]))