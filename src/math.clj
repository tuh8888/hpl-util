(ns math
  (:require [taoensso.timbre :as t]
            [clojure.math.combinatorics :as combo]
            [uncomplicate.neanderthal.core :as thal]
            [uncomplicate.commons.core :as uncomplicate]
            [uncomplicate.neanderthal.real :as thal-real]
            [uncomplicate.neanderthal.native :as thal-native]))

(defn digits
  "Returns the digits of a number"
  [x]
  (map #(Character/getNumericValue ^Character %) (str x)))

(defn exp
  [x n]
  (reduce * (repeat n x)))
(declare mult-persistence)

(defn unit-vec
  [v]
  (uncomplicate/with-release [alpha (thal/nrm2 v)
                              result (thal/scal (/ alpha) v)]
    (float result)))

#_(defn cosine-sim
    [v1 v2]
    (uncomplicate/with-release [v1 (unit-vec v1)
                                v2 (unit-vec v2)
                                result (thal/dot v1 v2)]
      result))

(defn unit-vec-sum
  [& vectors]
  (if (= 1 (count vectors))
    (uncomplicate/with-release [v (first (map thal-native/dv))
                                result (unit-vec v)]
      (seq result))
    (uncomplicate/with-release [vectors (map thal-native/dv vectors)
                                v (apply thal/xpy vectors)
                                result (unit-vec v)]
      (seq result))))


(defn vectors->matrix
  [{:keys [factory]} vectors]
  (let [d (some #(count %) vectors)]
    (->> vectors
         (mapcat seq)
         (thal/ge factory d (count vectors)))))

(defn mdot
  [params s1 s2]
  (uncomplicate/with-release [s1-mat (vectors->matrix params s1)
                              s1-mat-trans (thal/trans s1-mat)
                              s2-mat (vectors->matrix params s2)]
    #_(println (thal/mrows s1) (thal/ncols s1) (thal/mrows s2) (thal/ncols s2))
    (thal/mm s1-mat-trans s2-mat)))

(defn best-in-row-from-matrix
  [score-mat init i s]
  (->> s
       (map-indexed vector)
       (reduce
         (fn [{:keys [score] :as best} [j pattern]]
           (uncomplicate/with-release [new-score (thal-real/entry score-mat i j)]
             (if (< score new-score)
               {:match pattern :score (float score)}
               best)))
         init)))

(defn find-best-match
  [{:keys [vector-fn] :as params} s1 s2]
  (uncomplicate/with-release [score-mat (mdot params (map vector-fn s1) (map vector-fn s2))]
    (->> s1
         (map-indexed vector)
         (reduce
           (fn [best [i sample]]
             (-> (best-in-row-from-matrix score-mat best i s2)
                 (assoc :sample sample)))
           {})
         (doall))))

(defn find-best-row-matches
  [params s1 s2]
  (uncomplicate/with-release [score-mat (mdot params s1 s2)]
    (->> s1
         (map-indexed (fn [i sample]
                        (-> score-mat
                            (best-in-row-from-matrix {} i s2)
                            (assoc :sample sample))))
         (doall))))

(defn pred-false
  [& [{:keys [predicted-true all]}]]
  (clojure.set/difference all predicted-true))

(defn actual-false
  [& [{:keys [actual-true all]}]]
  (clojure.set/difference all actual-true))

(defn true-pos
  [& [{:keys [predicted-true actual-true]}]]
  (clojure.set/intersection predicted-true actual-true))
(defn true-neg
  [& [params]]
  (clojure.set/intersection (pred-false params)
                            (actual-false params)))
(defn false-pos
  [& [{:keys [predicted-true] :as params}]]
  (clojure.set/intersection predicted-true
                            (actual-false params)))
(defn false-neg
  [& [{:keys [actual-true] :as params}]]
  (clojure.set/intersection (pred-false params)
                            actual-true))

(defn precision
  [& [params]]
  (float (/ (count (true-pos params)) (+ (count (true-pos params)) (count (false-pos params))))))
(defn recall
  [& [params]]
  (float (/ (count (true-pos params)) (+ (count (true-pos params)) (count (false-neg params))))))
(defn f1
  [& [params]]
  (float (/ (* 2 (precision params) (recall params))
            (+ (precision params) (recall params)))))

(defn calc-metrics
  [& [params]]
  {:tp        (count (true-pos params))
   :tn        (count (true-neg params))
   :fp        (count (false-pos params))
   :fn        (count (false-neg params))
   :precision (precision params)
   :recall    (recall params)
   :f1        (f1 params)})

(defn _mult-persistence
  ([x step]
   (let [x (if (coll? x) x (digits x))]
     (if (= 1 (count x))
       step
       (mult-persistence (digits (reduce * (BigInteger/ONE) x)) (inc step)))))
  ([x]
   (_mult-persistence x 0)))

(def mult-persistence
  (let [mem (atom {})]
    (fn
      ([x step]
       (if-let [e (find @mem x)]
         (val e)
         (_mult-persistence x step)))
      ([x]
       (let [ret (mult-persistence x 0)]
         (swap! mem assoc x ret)
         ret)))))

(defn find-highest-mult-persistence
  "Finds the shortest whole number whose digits, can be iteratively multiplied
  the most times. Current record holder is 277777788888899 with 11 iterations."
  [num-digits]

  (let [best (atom {:num     (list)
                    :steps   0
                    :longest 0})
        digit-params (combo/cartesian-product
                       [true false]
                       [true false]
                       [true false]
                       (range num-digits)
                       (range num-digits)
                       (range num-digits)
                       (range num-digits)
                       (range num-digits))]
    (t/info "Num numbers to check: " (count digit-params))

    (letfn [(check-num [x]
              (let [steps (mult-persistence x)]
                (swap! best #(cond-> %
                                     (< (:steps %) steps) (assoc :num x :steps steps)
                                     (and (= (:steps %) steps)
                                          (<= (count x) (count (:num %)))) (assoc :num x :steps steps)
                                     (< (:longest %) (count x)) (assoc :longest (count x))))))
            (make-num [use2? use3? use4? num5 num6 num7 num8 num9]
              (lazy-cat
                (repeat (if use2? 0 1) 2)
                (repeat (if (and use3? (not use2?)) 0 1) 3)
                (repeat (if (and use4? (not use2?)) 0 1) 4)
                (repeat (if (not use2?) 0 num5) 5)
                (repeat num6 6)
                (repeat num7 7)
                (repeat num8 8)
                (repeat num9 9)))]
      (doall
        (pmap (fn [[i x]]
                (when (zero? (rem i 100000)) (t/info i))
                (check-num (apply make-num x)))
              (map-indexed vector digit-params)))
      (update @best :num #(BigInteger. ^String (apply str %))))))

(comment
  (t/set-level! :info)
  (mult-persistence 277777788888899)
  (find-highest-mult-persistence 3)
  (combo/car)
  (type (* (long 2) (long 5))))
