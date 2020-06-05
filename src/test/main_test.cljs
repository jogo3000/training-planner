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


(deftest exercise-editing
  (testing "New exercise is added to an empty db"
    (let [db (sut/handle :create-exercise {:editor "PK 10 km"
                                           :exercises {}})
          exercises (:exercises db)]
      (is (= 1 (count exercises)))

      (let [[id ex] (first exercises)]
        (is (= id (:selected-element db)))
        (is (= id (:id ex)))
        (is (= {:x 0 :y 0 :z 0 :description "PK 10 km" :id id}
               ex)))))

  (testing "New exercise is added to a db with existing exercise"
    (let [existing-id (random-uuid)
          db (sut/handle :create-exercise {:editor "PK 10 km"
                                           :exercises {existing-id {:id existing-id}}})
          exercises (:exercises db)]
      (is (= 2 (count exercises)))
      (let [new-exercise-id (->> (keys exercises)
                                 (filter #(not= existing-id %))
                                 first)
            exercise (get exercises new-exercise-id)]
        (is (= new-exercise-id (:id exercise)))
        (is (= {:id new-exercise-id
                :x 0 :y 0 :z 0 :description "PK 10 km"}
               exercise))
        (is (= new-exercise-id (:selected-element db)))))))
