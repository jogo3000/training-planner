(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom])
  (:import [goog.date Date Interval]))

(def canvas-id "week-svg")
(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(def db (r/atom {:startdate (Date.)
                  :drag {:dragging? false
                         :selected-element nil}
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

(defn render-date [date]
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
  (let [date-headers (-> (mapv #(render-date (inc-date monday %)) (range 7))
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

(defn mouse-position [evt]
  (let [ctm (.getScreenCTM (js/document.getElementById canvas-id))
        ctm-a (.-a ctm)
        ctm-e (.-e ctm)
        ctm-d (.-d ctm)
        ctm-f (.-f ctm)
        mouse-x (.-clientX evt)
        mouse-y (.-clientY evt)]
    {:x (/ (- mouse-x ctm-e) ctm-a)
     :y (/ (- mouse-y ctm-f) ctm-d)}))

(defn start-drag [evt id x y]
  (let [mouse-position (mouse-position evt)]
    (swap! db (fn [db]
                 (-> (assoc-in db [:drag :dragging?] true)
                     (assoc-in [:drag :selected-element] id)
                     (assoc-in [:drag :offset] {:x (- (:x mouse-position) x)
                                                :y (- (:y mouse-position) y)}))))))

(defn stop-drag []
  (swap! db (fn [db]
               (assoc db :drag {:dragging? false :selected-element nil}))))

(defn correct-mouse-position [mouse offset]
  {:x (- (:x mouse) (:x offset))
   :y (- (:y mouse) (:y offset))})

(defn mousemove [evt]
  (let [{:keys [dragging?
                selected-element
                offset]} (:drag @db)]
    (when dragging?
      (swap! db update :exercises
             (fn [exes]
               (let [mouse-position (-> (mouse-position evt)
                                        (correct-mouse-position offset))]
                 (mapv (fn [{id :id :as ex}]
                         (if-not (= id selected-element)
                           ex
                           (merge ex mouse-position)))
                       exes)))))))

(defn render-exercise [{:keys [id x y description]}]
  (let [text-offset-x (+ 5 x)
        text-offset-y (+ 25 y)]
    [:g {:id id
         :on-mouse-down #(start-drag % id x y)
         :on-mouse-leave stop-drag
         :on-mouse-up stop-drag}
     [:rect {:width 250
             :height 50
             :x x :y y
             :fill "green"}]
     [:text {:x text-offset-x :y text-offset-y
             :fill "black"} description]]))

(defn render-exercises [exercises]
  (map render-exercise exercises))

(defn week-day-headers [monday]
  (-> (mapv #(render-date (inc-date monday %)) (range 7))
      (conj "Yhteenveto")))

(defn root []
  (fn []
    (let [monday (date->last-monday (:startdate @db))
          date-headers (week-day-headers monday)
          exercises (:exercises @db)]
      [:<>
       (-> (into [:svg {:id canvas-id
                        :width canvas-width :height canvas-height
                        :on-mouse-move mousemove
                        :on-mouse-up stop-drag}]
                 (week-grid monday))
           (into (render-exercises exercises)))
       ;; FIXME: pois debugit. Voisko tähän saada aidon debuggerin kiinni?
       [:p (with-out-str (cljs.pprint/pprint @db))]])) )

(defn ^:export ^:dev/after-load main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))
