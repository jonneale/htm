(ns htm.temporal-pooler
  (:use [htm.config.constants]))

(defn active?
  [cell]
  (= on (:active-state cell)))

(defn get-cell-state
  [{:keys [to-column to-cell] :as info} entire-region]
  (:active-state (first (filter (fn [{:keys [position]}] (= position to-cell))
                                (:cells (nth entire-region to-column))))))

(defn predictive-state?
  [column]
  (= on (:predictive-state column)))

(defn all-cells-to-predictive-state
  [{:keys [cells] :as column}]
  (assoc column :cells (map #(assoc % :active-state on) cells)))

(defn update-activation
  [active-columns cells-in-predictive-state active-segments]
  (if (or (empty? cells-in-predictive-state) (empty? active-segments))
    ;;turn all cells on, as input is novel. The first set of input data is always novel
    (map all-cells-to-predictive-state active-columns)
    active-columns))

(defn update-predictive-state
  [{:keys [cells] :as column} region]
  (map (fn [{:keys [segments] :as cell}]
         (let [active-segments (filter active? (map (fn [lateral-synapses] (map #(get-cell-state % region) lateral-synapses)) segments))]
           (if (> (count active-segments) activation-threshold)
             (assoc cell :predictive-state on)
             cell)))
       cells))

(defn active-sequence-segment?
  []
  false)

(defn create-temporal-representation
  [{:keys [active-columns region] :as input}]
  (let [cells-in-predictive-state (filter predictive-state? active-columns)
        active-segments (filter active-sequence-segment? cells-in-predictive-state)
        columns (update-activation active-columns cells-in-predictive-state active-segments)
        updated-predictive-states (map #(update-predictive-state % region) columns)]
    updated-predictive-states
    ))

    ;;TODO fix get active segment - not implemented yet as first iteration it wont be used
