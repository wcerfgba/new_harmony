(ns new-harmony.core
  (:require [clojure.math.combinatorics :as combi]))

(def primes [2 3 5])

(def power-range 7)

(def prime-limit-maps
  (map (partial zipmap primes) (apply combi/cartesian-product (repeat (count primes) (range (- power-range) (inc power-range))))))

(def prime-limit-ratios
  (map (partial reduce
                (fn [acc [b x]]
                  (* acc (if (neg? x)
                           (/ 1 (int (Math/pow b (- x))))
                           (/ (int (Math/pow b x))
                              1))))
                1)
       prime-limit-maps))

(def prime-limits
  (zipmap prime-limit-maps prime-limit-ratios))

(def prime-limit-complexities
  (zipmap prime-limit-maps
          (map (fn [m]
                 (* (Math/pow (count (remove zero? (vals m))) 2)
                    (reduce
                     (fn [acc [b x]]
                       (+ acc (int (Math/pow b (Math/abs x)))))
                     1
                     m)))
               prime-limit-maps)))

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

(defn ratio->cents
  [r]
  (* 1200 (/ (Math/log r) (Math/log 2))))

(def twelve-tet->nearest-prime-limit
  (into {} (map (fn [[twelve-tet-idx twelve-tet-ratio]]
                  [twelve-tet-idx (reduce (fn [{:keys [difference] :as nearest}
                                               [prime-limit-map prime-limit-ratio]]
                                            (let [new-difference (- (ratio->cents twelve-tet-ratio)
                                                                    (ratio->cents prime-limit-ratio))]
                                              (if (and (<= 6 (Math/abs difference))
                                                       (< (Math/abs new-difference)
                                                          (Math/abs difference)))
                                                {:map prime-limit-map
                                                 :ratio prime-limit-ratio
                                                 :difference new-difference}
                                                nearest)))
                                          {:difference Float/MAX_VALUE}
                                          (sort-by (comp prime-limit-complexities first) prime-limits))])
                twelve-tets)))

(def twelve-tet->nearest-harmonic-limit
  (into {} (map (fn [[twelve-tet-idx twelve-tet-ratio]]
                  [twelve-tet-idx (reduce (fn [{:keys [difference] :as nearest}
                                               [i harmonic-limit]]
                                            (let [limit-differences
                                                  (zipmap harmonic-limit
                                                          (map #(/ twelve-tet-ratio
                                                                   %)
                                                               harmonic-limit))
                                                  new-nearest
                                                  (reduce (fn [{:keys [difference] :as nearest} [ratio new-difference]]
                                                            (if (< (Math/abs (- 1 new-difference))
                                                                   (Math/abs (- 1 difference)))
                                                              {:interval ratio
                                                               :difference new-difference}
                                                              nearest))
                                                          {:difference Float/MAX_VALUE}
                                                          limit-differences)]
                                              (if (and #_(<= 18 (Math/abs difference))
                                                       (< (Math/abs (- 1 (:difference new-nearest)))
                                                          (Math/abs (- 1 difference))))
                                                (assoc new-nearest :harmonic (+ i 2))
                                                nearest)))
                                          {:difference Float/MAX_VALUE}
                                          (map-indexed #(vector %1 %2) harmonic-limits))])
                twelve-tets)))

#_(->> twelve-tet->nearest-harmonic-limit
     (map (fn [[s m]] (assoc m :semitones s)))
     (sort-by :harmonic)
     (group-by :harmonic))

(->> twelve-tet->nearest-prime-limit
     (map (fn [[i {:keys [map] :as m}]]
            (assoc m
                   :complexity (prime-limit-complexities map)
                   :semitones i)))
     (sort-by :complexity))
