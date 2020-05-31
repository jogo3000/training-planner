(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom])
  (:import [goog.date Date Interval]))

(def canvas-id "week-svg")
(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(def db (r/atom {:startdate (Date.)
                 :editor ""
                 :keys-down #{}
                 :drag {:dragging? false}
                 :selected-element nil
                 :exercises []}))

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

(defn copy-exercise [id exercises]
  (let [copy (-> (filter #(= id (:id %)) exercises)
                 first
                 (assoc :id (random-uuid))
                 (update :z dec))]
    (conj exercises copy)))

(defn maybe-copy-exercise [db id]
  (if ((:keys-down db) "Control")
    (update db :exercises (partial copy-exercise id))
    db))

(defn start-drag [evt id x y]
  (let [mouse-position (mouse-position evt)]
    (swap! db (fn [db]
                (-> (maybe-copy-exercise db id)
                     (assoc-in [:drag :dragging?] true)
                     (assoc :selected-element id)
                     (assoc-in [:drag :offset] {:x (- (:x mouse-position) x)
                                                :y (- (:y mouse-position) y)}))))))

(defn stop-drag []
  (swap! db (fn [db]
               (assoc db :drag {:dragging? false}))))

(defn correct-mouse-position [mouse offset]
  {:x (- (:x mouse) (:x offset))
   :y (- (:y mouse) (:y offset))})

(defn mousemove [evt]
  (let [{:keys [dragging?
                offset]} (:drag @db)
        selected-element (:selected-element @db)]
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

(defn render-exercise [selected-exercise {:keys [id x y description]}]
  (let [text-offset-x (+ 5 x)
        text-offset-y (+ 25 y)]
    [:g {:id id
         :on-mouse-down #(start-drag % id x y)
         :on-mouse-up stop-drag}
     [:rect (merge {:width 250
                    :height 50
                    :x x :y y
                    :fill "green"}
                   (when (= selected-exercise id)
                     {:stroke "black"
                      :stroke-width 3}))]
     [:text {:x text-offset-x :y text-offset-y
             :fill "black"} description]]))

(defn render-exercises [exercises selected-exercise]
  (map (partial render-exercise selected-exercise) (sort :z exercises)))

(defn week-day-headers [monday]
  (-> (mapv #(render-date (inc-date monday %)) (range 7))
      (conj "Yhteenveto")))

(defn update-editor [evt]
  (swap! db assoc :editor (-> evt .-target .-value)))

(defn create-exercise []
  (swap! db
         (fn [db]
           (update db :exercises
                   conj {:id (random-uuid)
                         :x 0
                         :y 0
                         :z 0
                         :description (:editor db)}))))

(defn delete-exercise []
  (swap! db
         (fn [db]
           (let [selected (:selected-element db)]
             (update db :exercises
                     #(filter (fn [ex]
                                (not= selected (:id ex))) %))))))

(defn root []
  (fn []
    (let [monday (date->last-monday (:startdate @db))
          date-headers (week-day-headers monday)
          selected-exercise (:selected-element @db)
          exercises (:exercises @db)]
      [:div {:class "flex-down"}
       (-> (into [:svg {:id canvas-id
                        :width canvas-width :height canvas-height
                        :on-mouse-move mousemove
                        :on-mouse-up stop-drag}]
                 (week-grid monday))
           (into (render-exercises exercises selected-exercise)))
       [:textarea {:rows 10
                   :on-change update-editor}]
       [:button {:on-click create-exercise}
        "Luo harjoitus"]
       [:button {:on-click delete-exercise}
        "Poista harjoitus"]
       ;; FIXME: pois debugit. Voisko tähän saada aidon debuggerin kiinni?
       [:p (with-out-str (cljs.pprint/pprint @db))]])) )

(defn read-key [evt]
  (.-key evt))

(defn on-key-down [evt]
  (swap! db update :keys-down conj (read-key evt)))

(defn on-key-up [evt]
  (swap! db update :keys-down disj (read-key evt)))

(defn ^:export ^:dev/after-load main! []

  (js/document.addEventListener "keydown" on-key-down)
  (js/document.addEventListener "keyup" on-key-up)

  (rdom/render
   [root]
   (js/document.getElementById "app")))
