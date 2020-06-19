(ns exercise-parser
  (:require
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [instaparse.core :as insta]))

;;; use defparser maybe?
(def parser
  (insta/parser
   "S = DISTANCE | TIME;
DISTANCE = NUMBER <SPACE*> LENGHT-UNIT;
LENGHT-UNIT = KILOMETER | METER
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

(defn interpret-distance [d]
  (let [s (js/parseFloat (->> (second d) second unify-decimals))
        unit (->> (last d) last first)]
    (* s
       (case unit
         :KILOMETER 1000
         :METER 1
         (throw (js/Error. (str "Not supported distance unit: " unit)))))))

(defn parse-exercise [s]
  (let [ast (parser s)

        volume
        (postwalk
         (fn [node]
           (if (vector? node)
             (case (first node)
               :DISTANCE (interpret-distance node)
               node)
             node))
         ast)]
    {:volume (second volume)}))
