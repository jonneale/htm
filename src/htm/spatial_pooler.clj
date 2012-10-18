(ns htm.spatial-pooler
  (:use [htm.config.constants]))

(defn on?
  [synapse]
  (= on (:state synapse)))

(defn off?
  [synapse]
  (not (on? synapse)))

(defn random-permanence
  []
  (+ (rand permanence-threshold) (/ permanence-threshold 2)))

(defn create-cell
  [position column]
  {:lateral-synapses []
   :predictive-state off
   :active-state off
   :column column
   :position position})

(defn create-synapse
  [state input]
  {:state state
   :input (dec input)
   :permanence (random-permanence)})

(defn create-lateral-synapse
  [from-cell to-cell]
  {:from-column (:column from-cell)
   :from-cell (:position from-cell)
   :to-column (:column to-cell)
   :to-cell (:column to-cell)
   :state off})

(defn column
  [size position]
  {:boost (int (rand 10))
   :position position
   :active false
   :cells (reduce #(into %1 [(create-cell %2 position)]) []
                     (range 1 (inc size)))
   :synapses (reduce #(into %1 [(create-synapse off %2)]) []
                     (take (max (int (rand size)) 1)
                                   (sort-by rand (range 1 (inc size)))))})


(defn pick-random-cell
  [region]
  (let [column (nth region (rand (count region)))
        random (min (int (rand (count (:cells column)))) (dec (count (:cells column))))]
    (nth (:cells column) random)))

(defn add-lateral-synapses
  [cell region]
  (reduce (fn [agg _] (into agg [(create-lateral-synapse cell (pick-random-cell region))])) [] (range 1 (inc synapses-per-segment))))


(defn add-segments
  [cell region]
  (assoc cell :segments (for [i (range 1 (inc segments-per-cell))]
                          (add-lateral-synapses cell region))))

(defn add-segments-to-cells
  [region]
  (map (fn [{:keys [cells] :as column}]
         (assoc column :cells (map #(add-segments % region) cells)))
       region))

(defn update-column-states
  [{:keys [synapses] :as column}]
  (assoc column :synapses
         (map #(if (> (:permanence %) permanence-threshold)
                                   (assoc % :state "on")
                                   %)
                                synapses)))

(defn region
  [cells-in-column number-of-columns]
  (let [region (map #(column cells-in-column %) (range 1 (inc number-of-columns)))]
    (add-segments-to-cells region)))

(defn calculate-overlap
  {:doc "Takes the overlapping values of each of the columns and calculates the number of inputs connected to each column via an active synapse (a synapse associated with the column with a permanance value above the permanence threshold)"}
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
    (:overlap (first sorted-neighbours))))

(defn inhibit
  [column all-columns]
  (let [local-kth-score (kth-score column all-columns desired-local-activity)]
    (when (> (:overlap column) local-kth-score) (> (:overlap column) 0)
          column)))


;;TODO - there can currently be no active columns
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



(defn create-sparse-representation
  {:doc "Input can be any size - output of this function is a list of active columns (the list of columns that win due to bottom up input at time t) which should then be used as input to the temporal pooler"}
  [input]
  (let [;;region (initialise-region input columns-in-region)
        updated-region (map update-column-states (region cells-per-column columns-in-region))
        overlap (calculate-overlap updated-region input)
        activated-columns (filter (comp true? :active) (activate-columns overlap))
        updated-permanence-columns (map update-synapse-permanence activated-columns)]
    {:active-columns updated-permanence-columns
     :region overlap}))
