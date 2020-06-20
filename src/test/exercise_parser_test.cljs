(ns exercise-parser-test
  (:require [clojure.test :include-macros true :refer [deftest is testing]]
            [exercise-parser :as sut]))


(deftest combine-segments-test
  (is (= {:volume 1000
          :min-volume 1000
          :max-volume 1000} (sut/combine-segments [{:volume 500 :min-volume 200 :max-volume 300}
                                               {:volume 500 :min-volume 800 :max-volume 700}]))))

(deftest distance-units-test
  (is (= 1000 (sut/distance-units 1 :KILOMETER))))

(deftest distance-node-test
  (is (= {:volume 10000 :min-volume 10000 :max-volume 10000}
         (sut/distance-node
          [:DISTANCE [:NUMBER "10"] [:LENGTH-UNIT [:KILOMETER "km"]]]))))

(deftest distance-range-node-test
  (is (= {:volume 6000 :min-volume 6000 :max-volume 8000}
         (sut/distance-range-node
          [:DISTANCE-RANGE [:NUMBER "6"] [:NUMBER "8"] [:LENGTH-UNIT [:KILOMETER "km"]]]))))

(deftest repeated-segment-test
  (is (= {:volume 6000 :min-volume 6000 :max-volume 6000}
         (sut/repeated-segment
          [:REPEATED-SEGMENT [:MULTIPLIER [:NUMBER "6"]] {:volume 1000 :min-volume 1000 :max-volume 1000}]))))

(deftest exercise-text-parsing
  (letfn [(read-volume [s]
            (->> (sut/parse-exercise s) :volume))]
    (testing "Parsing simple length measures"
      (is (= 10000 (read-volume "10 km")))
      (is (= 10000 (read-volume "10000 m") )))

    (testing "Parsing length measures with decimal units"
      (is (= 5880 (read-volume "5,88 km"))))

    (testing "Interval exercises"
      (is (= 8000 (read-volume "vr 2 km + 4 x 1000 m / 1' + vr 2 km"))))

    (testing "nested repetitions"
      (is (= 10000 (read-volume "2x5x1000m"))))

    (testing "distance based rests"
      (is (= 6000 (read-volume "5x1000m/200m"))))

    (testing "Range of distances"
      (is (= {:min-volume 6000
              :max-volume 8000
              :volume 6000} (sut/parse-exercise "pk 6-8 km"))))))

(deftest valid-exercises
  (letfn [(valid? [s]
            (= :S (first (sut/parser s))))]
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
