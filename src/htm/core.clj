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
   :input (dec input)
   :permanence 1})

(defn column
  [size position]
  {:boost (int (rand 10)) :position position :synapses (reduce #(into %1 [(create-synapse off %2)]) []  (range 1 (inc size)))})

(def region
  (map #(column 4 %) (range 1 5)))

(defn on?
  [value]
  (= 1 value))

(defn off?
  [value]
  (not (on? value)))

(def permanence-threshold
  0.5)

(def min-overlap
  0)

(def desired-local-activity
  1)

(defn calculate-overlap
  [region input]
  (map (fn [{:keys [boost synapses] :as column}]
         (let [connected-synapses (filter #(> (:permanence %) permanence-threshold) synapses)
               overlap (reduce #(+ %1 (nth input (:input %2))) 0 connected-synapses)
               weighted-overlap (if (> overlap min-overlap) (* boost overlap) 0)]
           (assoc column :overlap weighted-overlap)))
       region))

(defn neighbours
  [column region]
  (filter #(or (= (:position %) (inc (:position column))) (= (:position %) (dec (:position column)))) region))

(defn kth-score
  [column region k]
  (let [column-neighbours (neighbours column region)]
    (:overlap (nth (sort-by :overlap column-neighbours) (dec (min (count column-neighbours) k))))))

(defn inhibit
  [column all-columns]
  (let [local-kth-score (kth-score column all-columns desired-local-activity)]
    (when (> (:overlap column) local-kth-score) (> (:overlap column) 0)
          column)))

(defn create-sparse-representation
  {:doc "Input is expected to have been processed into a vector the same length as the single region"}
  [input]
  (let [overlap (calculate-overlap region input)
        active-columns (filter (comp not nil?) (map #(inhibit % overlap) overlap))]
    active-columns))
