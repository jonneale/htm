(ns htm.core
  (:require [htm.spatial-pooler :as spatial-pooler]
            [htm.temporal-pooler :as temporal-pooler]
            [clojure.data.csv :as csv]
            [clojure.java.io  :as io])
  (:use [clojure.tools.cli :only (cli)]))

(defn process-data
  [file]
  (let [data (map #(Integer/parseInt %)
                   (last (with-open [in-file (io/reader (io/file file))]
                           (doall (csv/read-csv in-file)))))
        sparse-representation (spatial-pooler/create-sparse-representation data)
        temporal-representation (temporal-pooler/create-temporal-representation sparse-representation)]
    temporal-representation))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["-f"  "--filepath" "The full path to the comma seperated input file"])]
    (when (or (:help options)
              (not (:filepath options)))
      (println banner)
      (System/exit 0))
    (let [report-path     (options :filepath)]
      (process-data report-path))))
