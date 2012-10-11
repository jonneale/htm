(ns htm.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))


(def off
     "off")

(defn create-cell
  [state input]
  {:state state
   :input input})

(defn column
  [size]
  (reduce #(into %1 [(create-cell off %2)]) []  (range 1 (inc size))))

(def region
  (for [i (range 1 5)]
    (column 4)))

(defn on?
  [value]
  (= 1 value))

(defn off?
  [value]
  (not (on? value)))

(def permanence-threshold
  10)

;;if set to 2, then 2% of input will remain activated
(def sparse-representation-percentage
  10)

(defn calculate-overlap
  [data model]
  )

(defn create-sparse-representation
  {:doc "Input is expected to have been processed into a vector the same length as the single region"}
  [input]
  (let [connected-synapses (filter #(> (:permanance %) permanence-threshold) region)
        overlap (calculate-overlap connected-synapses )
        ]))
