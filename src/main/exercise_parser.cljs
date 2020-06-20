(ns exercise-parser
  (:require
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [instaparse.core :as insta]))

;;; use defparser maybe?
(def parser
  (insta/parser
   "
S = SEGMENT [<SPACE*> <#'\\+|,'> <SPACE*> SEGMENT ]*
<SEGMENT> = [ <QUALIFIER> <SPACE*> ] ( REPEATED-SEGMENT | SINGLE-SEGMENT ) [ <SPACE*> <QUALIFIER> ];
REPEATED-SEGMENT = MULTIPLIER SEGMENT | MULTIPLIER <'('> <SPACE*> [ SEGMENT ]+ <')'>;
SINGLE-SEGMENT = ( DISTANCE-RANGE  | DISTANCE | TIME ) [ REST ];
MULTIPLIER = NUMBER <SPACE*> <#'x'> <SPACE*>;
QUALIFIER = 'vk' | 'pk' | 'mk' | 'n' | 'vr' | 'mäki' | 'tv kova';
REST = [ <SPACE*> ] <'/'> [ <SPACE*> ] ( DISTANCE | TIME );
DISTANCE = NUMBER <SPACE*> LENGTH-UNIT;
DISTANCE-RANGE = NUMBER <SPACE*> <'-'> <SPACE*> NUMBER <SPACE*> LENGTH-UNIT;
LENGTH-UNIT = KILOMETER | METER
TIME = NUMBER <SPACE*> HOUR [<SPACE*> NUMBER <SPACE*> MINUTE ] [<SPACE*> NUMBER <SPACE*> SECOND ] | NUMBER <SPACE*> MINUTE [<SPACE*> NUMBER <SPACE*> [SECOND]] | NUMBER <SPACE*> SECOND;
NUMBER = #'[0-9]+(\\,[0-9]+)?';
KILOMETER = 'km'
METER = 'm';
SECOND = #'s|\\'\\'';
MINUTE = #\"min|'\";
HOUR = #'h|:';
SPACE = ' ';
"))

(defn unify-decimals [s]
  (str/replace s #"," "."))

(defn distance-units [n unit]
  (* n
     (case unit
       :KILOMETER 1000
       :METER 1
       (throw (js/Error. (str "Not supported distance unit: " unit))))))

(defn interpret-distance [d]
  (let [s (js/parseFloat (->> (second d) second unify-decimals))
        unit (->> (last d) last first)]
    (distance-units s unit)))

(defn combine-segments [segments]
  (reduce (fn [all segment]
            {:volume (+ (:volume all) (:volume segment))
             :min-volume (+ (:min-volume all) (:min-volume segment))
             :max-volume (+ (:max-volume all) (:max-volume segment))})
          segments))

(defn repeated-segment [node]
  (let [[[_ [_ multiplier]] & segments] (rest node)
        combined (combine-segments segments)
        multiplier (js/parseInt multiplier)]
    (-> (update combined :volume * multiplier)
        (update :min-volume * multiplier)
        (update :max-volume * multiplier))))

(defn distance-node [node]
  (let [distance (interpret-distance node)]
    {:volume distance
     :min-volume distance
     :max-volume distance}))

(defn distance-range-node [node]
  (let [[_ [_ minimum] [_ maximum] [_ [length-unit _]]] node
        minimum (js/parseFloat minimum)
        maximum (js/parseFloat maximum)]
    {:volume (distance-units minimum length-unit)
     :min-volume (distance-units minimum length-unit)
     :max-volume (distance-units maximum length-unit)}))

(defn parse-exercise [s]
  (let [ast (parser s)

        volume
        (postwalk
         (fn [node]
           (if (vector? node)
             (case (first node)
               :DISTANCE (distance-node node)
               :DISTANCE-RANGE (distance-range-node node)
               :REPEATED-SEGMENT (repeated-segment node)
               :SINGLE-SEGMENT (combine-segments (rest node))
               :TIME {}
               :REST (second node)
               :S (combine-segments (rest node))
               node)
             node))
         ast)]
    (if (map? volume)
      volume
      {})))
