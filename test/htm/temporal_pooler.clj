(ns htm.test.temporal-pooler
  (:use [clojure.test])
  (:require [htm.temporal-pooler :as t]))

(deftest all-cells-to-predictive-state-sets-active-state-flag-to-on-in-cells-in-column
  (let [column {:cells [{:active-state "off"}]}]
    (is  (= "on" (-> column
                     t/all-cells-to-predictive-state
                     :cells
                     first
                     :active-state)))))

(deftest update-activation-turns-on-all-cells-if-no-active-segments-or-predictive-cells
  (let [active-columns [{:cells [{:active-state "off"}]}]]
    (is (= "on" (-> active-columns
                    (t/update-activation () ())
                    first
                    :cells
                    first
                    :active-state)))))

(deftest update-activation-returns-active-columns-if-active-segment-and-some-cells-in-predictive-mode
  (let [active-columns [{:cells [{:active-state "off"}]}]]
    (is (= active-columns (t/update-activation active-columns [{:cells []}] ["some-segment"])))))
