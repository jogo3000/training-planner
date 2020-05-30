(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom])
  (:import [goog.date Date Interval]))

(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(def app (r/atom {:startdate (Date.)
                  :exercises [{:x 5 :y 50 :description "Pk 10 km"}
                              {:x 5 :y 200 :description "Vr 10' + 2 x 10' / 2' + vr 10'"}]}))

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

(defn pprint-date [date]
  (let [weekday (-> (.getIsoWeekday date)
                        (case
                            0 "Maanantai"
                            1 "Tiistai"
                            2 "Keskiviikko"
                            3 "Torstai"
                            4 "Perjantai"
                            5 "Lauantai"
                            6 "Sunnuntai"))]
    (str weekday " " (.getDate date) "." (.getMonth date) "." (.getYear date))))

(defn week-grid [monday]
  (let [date-headers (-> (mapv #(pprint-date (inc-date monday %)) (range 7))
                         (conj "Yhteenveto"))]
    (map-indexed  (fn [i title]
                    (let [top 0
                          left (* i day-width)]
                      [:<>
                       [:rect {:width day-width
                               :height canvas-height
                               :stroke-width 2
                               :stroke "black"
                               :x left
                               :y top
                               :fill "white"}]
                       [:text {:x (+ left 10)
                               :y (+ top 20)}
                        title]]))
                  date-headers)))

(defn render-exercise [{:keys [x y description]}]
  (let [text-offset-x (+ 5 x)
        text-offset-y (+ 25 y)]
    [:<>
     [:rect {:width 250
             :height 50
             :x x :y y
             :fill "green"}]
     [:text {:x text-offset-x :y text-offset-y
             :fill "black"} description]]))

(defn exercises [exercises]
  (map render-exercise exercises))

(defn root []
  (fn []
    (let [monday (date->last-monday (:startdate @app))
          date-headers (-> (mapv #(pprint-date (inc-date monday %)) (range 7))
                           (conj "Yhteenveto"))]
      (-> (into [:svg {:width canvas-width :height canvas-height}]
                (week-grid monday))
          (into (exercises (:exercises @app)))))) )

(defn ^:export ^:dev/after-load main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))
