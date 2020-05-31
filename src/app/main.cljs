(ns app.main
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [reagent.dom :as rdom])
  (:import [goog.date Date DateTime Interval]))

(def canvas-id "week-svg")
(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(defn date->last-monday [date]
  (let [day (.getDate date)
        weekday (.getIsoWeekday date)
        year (.getYear date)
        month (.getMonth date)]
    (if (zero? weekday)
      date
      (Date. year month (- day weekday)))))

(defn inc-date [date days]
  (let [new-date (.clone date)]
    (->> (Interval. Interval/DAYS days)
         (.add new-date))
    new-date))

(defn dec-week [date]
  (let [new-date (.clone date)]
    (->> (Interval. Interval/DAYS -7)
         (.add new-date))
    new-date))

(defn inc-week [date]
  (let [new-date (.clone date)]
    (->> (Interval. Interval/DAYS 7)
         (.add new-date))
    new-date))


(defn inc-hours [datetime hours]
  (let [new (.clone datetime)]
    (->> (Interval. Interval/HOURS hours)
         (.add new))
    new))

(defonce db (r/atom {:start-date (date->last-monday (Date.))
                 :editor ""
                 :keys-down #{}
                 :drag {:dragging? false}
                 :selected-element nil
                 :exercises []}))


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
    (str weekday " " (.getDate date) "." (inc (.getMonth date)) "." (.getYear date))))

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

(defn identify-datetime [start-date {:keys [x y]}]
  (let [weekday (js/Math.floor (/ x day-width))
        hour (if (< y 200) 6 16)
        date (inc-date start-date weekday)]
    (DateTime. (.getYear date) (.getMonth date) (.getDate date) hour 0)))

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
        start-date (:start-date @db)
        selected-element (:selected-element @db)]
    (when dragging?
      (swap! db update :exercises
             (fn [exes]
               (let [mouse-position (-> (mouse-position evt)
                                        (correct-mouse-position offset))
                     new-datetime (identify-datetime start-date mouse-position)]
                 (mapv (fn [{id :id :as ex}]
                         (if-not (= id selected-element)
                           ex
                           (merge
                            ex
                            mouse-position
                            {:start-time new-datetime})))
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

(defn ical-render-summary [description]
  (first (str/split-lines description)))

(defn ical-render-description [description]
  (str/escape description {\newline "\\n"}))

(defn ical-render-exercise [exercise]
  (if-let [start (:start-time exercise)]
    (let [created "20200524T135634Z"
          uid (:id exercise)
          end (inc-hours start 1)
          description (:description exercise)]
      (str
       "\nBEGIN:VEVENT"
       "\nDTSTAMP:" created
       "\nUID:" uid
       "\nDTSTART:" (.toString start)
       "\nDTEND:" (.toString end)
       "\nSUMMARY:" (ical-render-summary description)
       "\nDESCRIPTION:" (ical-render-description description)
       "\nCATEGORIES:training-plan"
       "\nEND:VEVENT"))))

(defn to-ical []
  (let [exercises (:exercises @db)]
    (str "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:harjoitukset
PRODID:-//Matti Uusitalo//training-planner//EN
X-WR-TIMEZONE:EEST
X-WR-CALDESC:Viikon harjoitukset
CALSCALE:GREGORIAN"
         (apply str (map ical-render-exercise exercises))
         "\nEND:VCALENDAR
")))

(defn previous-week []
  (swap! db update :start-date dec-week))

(defn next-week []
  (swap! db update :start-date inc-week))

(defn root []
  (fn []
    (let [monday (date->last-monday (:start-date @db))
          date-headers (week-day-headers monday)
          selected-exercise (:selected-element @db)
          exercises (:exercises @db)]
      [:div {:class "flex-down"}
       [:div {:class "flex-right"}
        [:button {:on-click previous-week}"Edellinen viikko"]
        [:button {:on-click next-week} "Seuraava viikko"]]
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
       [:p (with-out-str (cljs.pprint/pprint @db))]
       [:a {:href (str "data:text/plain;charset=utf-8," (js/encodeURIComponent (to-ical)))
            :download "harjoitukset.ics"}
        "Lataa viikon harjoitukset"]])) )


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
