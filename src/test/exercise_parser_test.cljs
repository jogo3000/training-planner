(ns exercise-parser-test
  (:require [clojure.test :include-macros true :refer [deftest is testing]]
            [exercise-parser :refer [parse-exercise parser]]))


(deftest exercise-text-parsing
  (letfn [(read-volume [s]
            (->> (parse-exercise s) :volume))]
    (testing "Parsing simple length measures"
      (is (= 10000 (read-volume "10 km")))
      (is (= 10000 (read-volume "10 000 m") )))

    (testing "Parsing length measures with decimal units"
      (is (= 5880 (read-volume "5,88 km"))))))

(deftest valid-exercises
  (letfn [(valid? [s]
            (= :S (first (parser s))))]
    (is (valid? "10 km"))
    (is (valid? "10000 m"))

    (is (valid? "10'"))
    (is (valid? "10min"))

    (is (valid? "10h"))
    (is (valid? "10:"))
    (is (valid? "10 h"))

    (is (valid? "15''"))
    (is (valid? "15 s"))


    (is (valid? "10'45"))
    (is (valid? "10'10''"))
    (is (valid? "10 h 10 min"))))
