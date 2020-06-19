(ns exercise-parser
  (:require [instaparse.core :as insta]))

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

(defn parse-exercise [s]
  (let [ast (parser s)]
    ast))
