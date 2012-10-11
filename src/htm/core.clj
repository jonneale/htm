(ns htm.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io  :as io])
  (:use [clojure.tools.cli :only (cli)]))

(defn process-data
  [file]
  (let [data (map #(Integer/parseInt %)
                   (last (with-open [in-file (io/reader (io/file file))]
                           (doall (csv/read-csv in-file)))))
        sparse-representation (create-sparse-representation data)]
    sparse-representation))

(def permanence-threshold
  0.5)

(def min-overlap
  0)

(def desired-local-activity
  1)

(def off
  "off")

(def on
  "on")

(def permanence-inc
  0.1)

(defn on?
  [synapse]
  (= on (:state synapse)))

(defn off?
  [synapse]
  (not (on? synapse)))

(defn random-permanence
  []
  (+ (rand permanence-threshold) (/ permanence-threshold 2)))

(defn create-synapse
  [state input]
  {:state state
   :input (dec input)
   :permanence (random-permanence)})

(defn column
  [size position]
  {:boost (int (rand 10)) :position position :active false :synapses (reduce #(into %1 [(create-synapse off %2)]) []  (range 1 (inc size)))})

(def region
  (map #(column 4 %) (range 1 5)))

(defn update-column-states
  [{:keys [synapses] :as column}]
  (assoc column :synapses
         (map #(if (> (:permanence %) permanence-threshold)
                                   (assoc % :state "on")
                                   %)
                                synapses)))

(defn calculate-overlap
  ;;Pretty sure this is wrong. When calculating overlap we should probably be looking at the activation value of the synapse and not the value of the input?
  [region input]
  (map (fn [{:keys [boost synapses] :as column}]
         (let [connected-synapses (filter on? synapses)
               overlap (reduce #(+ %1 (nth input (:input %2))) 0 connected-synapses)
               weighted-overlap (if (> overlap min-overlap) (* boost overlap) 0)]
           (assoc column :overlap weighted-overlap)))
       region))

(defn neighbours
  [column region]
  (filter #(or (= (:position %) (inc (:position column))) (= (:position %) (dec (:position column)))) region))

(defn kth-score
  [column region k]
  (let [column-neighbours (neighbours column region)
        sorted-neighbours (sort-by :overlap column-neighbours)
        position (dec (min (count sorted-neighbours) k))]
    (:overlap (nth sorted-neighbours position))))

(defn inhibit
  [column all-columns]
  (let [local-kth-score (kth-score column all-columns desired-local-activity)]
    (when (> (:overlap column) local-kth-score) (> (:overlap column) 0)
          column)))

(defn activate-columns
  [columns]
  (map (fn [column]
         (assoc column :active
                (-> column
                    (inhibit columns)
                    nil?
                    not)))
       columns))

(defn update-synapse-permanence
  [{:keys [synapses active] :as column}]
  (assoc column :synapses
         (map (fn [{:keys [permanence] :as synapse}]
                (let [operator (if active + -)]
                  (assoc synapse :permanence (operator permanence permanence-inc))))
              synapses)))


;;TODO: Boost is not implemented yet
(defn create-sparse-representation
  {:doc "Input is expected to have been processed into a vector the same length as the single region"}
  [input]
  (let [updated-region (map update-column-states region)
        overlap (calculate-overlap updated-region input)
        activated-columns (activate-columns overlap)
        updated-permanence-columns (map update-synapse-permanence activated-columns)]
    updated-permanence-columns))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["-f"  "--filepath" "The full path to the comma seperated input file"])]
    (when (or (:help options)
              (not (:filepath options)))
      (println banner)
      (System/exit 0))
    (let [report-path     (options :filepath)]
      (process-data report-path))))
