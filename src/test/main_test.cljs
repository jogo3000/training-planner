(ns test.main-test
  (:require [app.main :as sut]
            [cljs.test :refer [deftest is testing] :as t :include-macros true])
  (:import [goog.date Date]))


(deftest exercise-serialization
  (letfn [(round-trip [x]
            (->> (sut/serialize-exercises x)
                 (sut/deserialize-exercises)))
          (make-comparable [x]
            (->> (map (fn [[id ex]]
                        [id (update ex :start-time #(.toIsoString %))]) x)
                 (into {})))]

    (testing "Exercise with all the data can be serialized and returned back"
      (let [id (random-uuid)
            exercises {id {:start-time (Date. 2020 5 1)
                           :id id :x 250 :y 15 :z 30 :description "Pk 10 km"}}
            actual (round-trip exercises)]
        (is (= (make-comparable exercises)
               (make-comparable actual)))))))
