(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom])
  (:import [goog.date Date Interval]))

(def app (r/atom {:startdate (Date.)}))

(defn date->last-monday [date]
  (let [day (.getDate date)
        weekday (.getIsoWeekday date)
        year (.getYear date)
        month (.getMonth date)]
    (Date. year month (- day weekday))))

(defn inc-date [date days]
  (let [new-date (.clone date)]
    (->> (Interval. Interval/DAYS days)
         (.add new-date))
    new-date))

(defn root []
  (fn []
    (let [monday (date->last-monday (:startdate @app))]
      (into [:div]
            (for [i (range 0 7)]
              [:h1 (.toString (inc-date monday i))])))) )

(defn ^:export main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))
