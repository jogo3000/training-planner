(ns exercise-parser-test
  (:require [clojure.test :include-macros true :refer [deftest is testing]]
            [exercise-parser :refer [parse-exercise]]))


(deftest exercise-text-parsing
  (letfn [(read-volume [s]
            (->> (parse-exercise s) :volume))]
    (testing "Parsing simple length measures"
      (is (= 10000 (read-volume "10 km")))
      (is (= 10000 (read-volume "10 000 m") )))

    (testing "Parsing length measures with decimal units"
      (is (= 5880 (read-volume "5,88 km"))))))
