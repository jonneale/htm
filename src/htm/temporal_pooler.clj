(ns htm.temporal-pooler
  (:use [htm.config.constants]))



(defn predictive-state?
  [column]
  (= on (:predictive-state column)))

(defn active-sequence-segment?
  [column])

(defn all-cells-to-predictive-state
  [{:keys [cells] :as column}]
  (assoc column :cells (map #(assoc % :active-state on) cells)))

(defn update-activation
  [active-columns cells-in-predictive-state active-segments]
  (if (or (empty? cells-in-predictive-state) (empty? active-segments))
    ;;turn all cells on, as input is novel. The first set of input data is always novel
    (map all-cells-to-predictive-state active-columns)
    active-columns))

(defn create-temporal-representation
  [active-columns]
  (let [cells-in-predictive-state (filter predictive-state? active-columns)
        active-segments (filter active-sequence-segment? cells-in-predictive-state)
        columns (update-activation active-columns cells-in-predictive-state active-segments)]
    ;;TODO fix get active segment - not implemented yet as first iteration it wont be used))
