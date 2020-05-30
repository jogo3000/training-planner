(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom])
  (:import [goog.date Date Interval]))

(def canvas-id "week-svg")
(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(def app (r/atom {:startdate (Date.)
                  :drag {:dragging? false
                         :element nil}
                  :exercises [{:id 1 :x 5 :y 50 :description "Pk 10 km"}
                              {:id 2 :x 5 :y 200 :description "Vr 10' + 2 x 10' / 2' + vr 10'"}]}))

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

(defn start-drag [id]
  (swap! app (fn [app]
               (-> (assoc-in app [:drag :dragging?] true)
                   (assoc-in [:drag :element] id)))))

(defn stop-drag []
  (swap! app (fn [app]
               (assoc app :drag {:dragging? false :element nil}))))

(defn mouse-position [evt]
  (let [ctm (.getScreenCTM (js/document.getElementById canvas-id))
        ctm-a (.-a ctm)
        ctm-e (.-e ctm)
        ctm-d (.-d ctm)
        ctm-f (.-f ctm)
        mouse-x (.-clientX evt)
        mouse-y (.-clientY evt)]
    (js/console.log ctm-a ctm-e ctm-d ctm-f)
    {:x (/ (- mouse-x ctm-e) ctm-a)
     :y (/ (- mouse-y ctm-f) ctm-d)}))

(defn mousemove [evt]
  (let [dragging? (get-in @app [:drag :dragging?])
        selected (get-in @app [:drag :element])]
    (when dragging?
      (swap! app update :exercises
             (fn [exes]
               (let [mouse-position (mouse-position evt)]
                 (mapv (fn [{id :id :as ex}]
                         (if-not (= id selected)
                           ex
                           (merge ex mouse-position)))
                       exes)))))))

(defn render-exercise [{:keys [id x y description]}]
  (let [text-offset-x (+ 5 x)
        text-offset-y (+ 25 y)]
    [:g {:id id
         :on-mouse-down #(start-drag id)
         :on-mouse-up stop-drag}
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
      [:<>
       (-> (into [:svg {:id canvas-id
                        :width canvas-width :height canvas-height
                        :on-mouse-move mousemove
                        :on-mouse-up stop-drag}]
                 (week-grid monday))
           (into (exercises (:exercises @app))))
       [:p (str (get-in @app [:drag]))]
       [:p (str (get-in @app [:exercises]))]])) )

(defn ^:export ^:dev/after-load main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))
