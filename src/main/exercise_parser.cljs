(ns exercise-parser
  (:require [clojure.string :as str]))

(defn interpret-decimal [n c]
  (+ (* 10 n)
     (js/parseInt c)))

(defn interpret-unit [{id :unit-identifier :as state} c]
  (let [unit (str id c)]
    (assoc state
           :unit-identifier unit
           :unit-type :length
           :multiplier ({"km" 1000
                         "m" 1} unit))))

(defn finalize-segment [{s :current-segment
                         {:keys [multiplier]} :current-unit :as state}]
  (update state :volume + (* multiplier s)))

(defn number-char? [c]
  (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} c))

(defn whitespace? [c]
  (boolean (re-matches #"\s" c)))

(defn letter? [c]
  (boolean (re-matches #"[a-zA-Z]" c)))

(defn symbol? [c]
  (boolean (re-matches #"\W" c)))

(defn decimal-separator? [c]
  (or (= "," c)
      (= "." c)))

(defn parse-exercise [s]
  (->> (reduce (fn [{mode :mode :as state} c]
                 (case mode
                   :start (cond
                            (number-char? c)
                            (assoc state :mode :number
                                   :work c)

                            (letter? c)
                            (assoc state :mode :word
                                   :work c)

                            (whitespace? c)
                            state)

                   :number (cond
                             (number-char? c)
                             (update state :work str c)

                             (decimal-separator? c)
                             (update state :work str ".")

                             (whitespace? c)
                             state

                             (symbol? c)
                             (assoc )))
                 (cond-> state
                   (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} c) (update :current-segment interpret-decimal c)
                   (#{"k" "m"} c) (update :current-unit interpret-unit c))) {:mode :start
                                                                             :volume 0} s)
       finalize-segment))
