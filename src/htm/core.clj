(ns htm.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(def off
  "off")

(def on
  "on")

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
  {:boost (int (rand 10)) :position position :synapses (reduce #(into %1 [(create-synapse off %2)]) []  (range 1 (inc size)))})

(def region
  (map #(column 4 %) (range 1 5)))

(def permanence-threshold
  0.5)

(def min-overlap
  0)

(def desired-local-activity
  1)

(defn update-column-states
  [{:keys [synapses] :as column}]
  (assoc column :synapses
         (map #(if (> (:permanence %) permanence-threshold)
                                   (assoc % :state "on")
                                   %)
                                synapses)))

(defn calculate-overlap
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

(defn create-sparse-representation
  {:doc "Input is expected to have been processed into a vector the same length as the single region"}
  [input]
  (let [updated-region (map update-column-states region)
        overlap (calculate-overlap updated-region input)
        active-columns (filter (comp not nil?) (map #(inhibit % overlap) overlap))]
    active-columns))
