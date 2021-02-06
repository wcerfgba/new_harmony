(ns new-harmony.core
  (:require [clojure.math.combinatorics :as combi]))

(def primes [2 3 5])

(def power-range 3)

(def five-limit-maps
  (map (partial zipmap primes) (apply combi/cartesian-product (repeat (count primes) (range (- power-range) (inc power-range))))))

(def five-limit-ratios
  (map (partial reduce
                (fn [acc [b x]]
                  (* acc (if (neg? x)
                           (/ 1 (int (Math/pow b (- x))))
                           (/ (int (Math/pow b x))
                              1))))
                1)
       five-limit-maps))

(def five-limits
  (zipmap five-limit-maps five-limit-ratios))

(def harmonic-series
  (concat [1] (take 32 (map / (drop 2 (range)) (drop 1 (range))))))

(def harmonic-limits
  (mapv #(->> (combi/combinations (range 1 (inc %)) 2)
              (map reverse)
              (map (fn [[a b]] (/ a b)))
              (set))
        (range 2 19)))

(def twelve-tets
  (into {} (map #(vector % (Math/pow 2 (/ % 12))) (range 1 13))))

(def twelve-tet->nearest-five-limit
  (into {} (map (fn [[twelve-tet-idx twelve-tet-ratio]]
                  [twelve-tet-idx (reduce (fn [{:keys [difference] :as nearest}
                                               [five-limit-map five-limit-ratio]]
                                            (let [new-difference (- twelve-tet-ratio
                                                                    five-limit-ratio)]
                                              (if (< (Math/abs new-difference) 
                                                     (Math/abs difference))
                                                {:map five-limit-map
                                                 :ratio five-limit-ratio
                                                 :difference new-difference}
                                                nearest)))
                                          {:difference Float/MAX_VALUE}
                                          five-limits)])
                twelve-tets)))

(defn ratio->cents
  [r]
  (* 1200 (/ (Math/log r) (Math/log 2))))

(def twelve-tet->nearest-harmonic-limit
  (into {} (map (fn [[twelve-tet-idx twelve-tet-ratio]]
                  [twelve-tet-idx (reduce (fn [{:keys [difference] :as nearest}
                                               [i harmonic-limit]]
                                            (let [limit-differences
                                                  (zipmap harmonic-limit
                                                          (map #(- (ratio->cents twelve-tet-ratio)
                                                                   (ratio->cents %))
                                                               harmonic-limit))
                                                  new-nearest
                                                  (reduce (fn [{:keys [difference] :as nearest} [ratio new-difference]]
                                                            (if (< (Math/abs new-difference)
                                                                   (Math/abs difference))
                                                              {:interval ratio
                                                               :difference new-difference}
                                                              nearest))
                                                          {:difference Float/MAX_VALUE}
                                                          limit-differences)]
                                              (if (and #_(<= 18 (Math/abs difference))
                                                       (< (Math/abs (:difference new-nearest))
                                                          (Math/abs difference)))
                                                (assoc new-nearest :harmonic (+ i 2))
                                                nearest)))
                                          {:difference Float/MAX_VALUE}
                                          (map-indexed #(vector %1 %2) harmonic-limits))])
                twelve-tets)))

(->> twelve-tet->nearest-harmonic-limit
     (map (fn [[s m]] (assoc m :semitones s)))
     (sort-by :harmonic)
     (group-by :harmonic))
