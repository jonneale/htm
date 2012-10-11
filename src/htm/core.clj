(ns htm.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))


(def off
     "off")

(defn create-synapse
  [state input]
  {:state state
   :input input
   :permanence 1})

(defn column
  [size]
  {:boost 1 :synapses (reduce #(into %1 [(create-synapse off %2)]) []  (range 1 (inc size)))})

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
  0.5)

;;if set to 2, then 2% of input will remain activated
(def sparse-representation-percentage
  10)

(def min-overlap
  10)

(defn calculate-overlap
  [region input]
  (map (fn [{:keys [boost synapses]}]
         (println "here")


         (let [connected-synapses (filter #(> (:permanence %) permanence-threshold) synapses)
               overlap (reduce #(+ %1 (input (:input %2))) 0 connected-synapses)
               weighted-overlap (if (> overlap min-overlap) (* boost overlap) 0)]
           (assoc column :overlap weighted-overlap)))
       region))

(defn create-sparse-representation
  {:doc "Input is expected to have been processed into a vector the same length as the single region"}
  [input]
  (let [overlap (calculate-overlap region input)]
    overlap))
