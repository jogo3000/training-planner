(ns training-planner-test
  (:require [training-planner :as sut]
            [cljs.test :refer [deftest is testing run-tests] :as t :include-macros true])
  (:import [goog.date Date DateTime]))

(comment
  (run-tests))

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
        (is (= new-exercise-id (:selected-element db))))))

  (testing "Starting to drag an exercise selects it and stores the offset"
    (with-redefs [sut/mouse-position (constantly {:x 100 :y 24})]
      (let [id (random-uuid)
            description "2 x 10' / 5'"
            db {:selected-element nil
                :editor ""
                :exercises {id {:id id :x 80 :y 20 :z 0 :description description}}}

            modified-db (sut/handle :start-drag db #js {} {:id id :x 80 :y 20 :description description})]
        (is (= description (:editor modified-db)))
        (is (= id (:selected-element modified-db)))
        (is (true? (get-in modified-db [:drag :dragging?])))
        (is (= {:x 20 :y 4} (get-in modified-db [:drag :offset])))))))
