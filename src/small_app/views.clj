(ns small-app.views
  (:use [hiccup.page :only [html5 include-css]]
        [c2.layout.histogram :only [histogram]]
        [c2.core :only [unify]])
  (:require [clojure.string :as str]
            [c2.scale :as scale]
            [clj-http.client :as client]
            [c2.svg :as svg]
            [c2.ticks :as tick]))

(defn gen-page-head
  [title]
    [:head
       [:title (str "c2-experiments Homepage " title)]
          (include-css "/css/style.css")])



(defn- style [& info]
  {:style (.trim (apply str (reduce #(conj %1
                                           (-> %2 first name) ":" (last %2) ";")
                                    []
                                    (partition 2 info))))})

(defn- extract-data [chart-data]
  (let [{:keys [data field_xpath]} chart-data
        na-fix (fn [s] (if (nil? s) "NA" (str s)))]
    (into {} (for [data-item data]
               [(na-fix ((keyword field_xpath) data-item))
                (:count data-item)]))))

(defn- extract-data-for-histogram [chart-data bins]
  (let [{:keys [data field_xpath]} chart-data
        qn-key (keyword field_xpath)
        non-nil-data (remove (fn [el] (nil? (qn-key el))) data)
        data-with-numbers (map (fn [el]
                                 (assoc el qn-key (Integer. (qn-key el))))
                               non-nil-data)
        domain (map qn-key data-with-numbers)
        xmin (- (apply min domain) 1)
        xmax (+ (apply max domain) 1)
        binned-data (histogram data-with-numbers :value qn-key :bins bins)]

    (for [data-item binned-data]
      [(:x (meta data-item))
       (:dx (meta data-item))
       (apply + (map :count data-item))])))

; (extract-data-for-histogram n-data)
; (def chart-data n-data)

(defn numeric-chart
  [chart-data]
  (let [chart-width 700 chart-height 300  bins 10
        extracted-data (extract-data-for-histogram chart-data bins)
        {:keys [field_label field_xpath]} chart-data
        x-series (map first extracted-data)
        dx-series (map second extracted-data)
        y-series (map last extracted-data)
        xmin (apply min x-series)
        xmax (apply max x-series)
        x-scale (scale/linear :domain [xmin xmax]
                              :range [0 chart-width])
        y-scale (scale/linear :domain [0 (apply max y-series)]
                              :range [0 chart-height])
        ticks (:ticks (tick/search [xmin xmax] :clamp? true))
        margin 0 small-margin 2
        bin-width (- (/ chart-width bins) small-margin)]
    [:table#histogram.table-bordered
     [:thead [:tr [:th field_label]]]
     [:tbody [:tr [:td
        [:svg {:width (+ margin chart-width) :height (+ margin chart-height)}
         (unify extracted-data
                (fn [[x dx y]]
                  (let [x-scaled (float (x-scale x))
                        y-scaled (float (y-scale y))]
                    [:g
                     [:g.bar {:transform (svg/translate [x-scaled (- chart-height y-scaled)])}
                     [:rect {:x 1 :style "fill:grey"
                             :height y-scaled
                             :width bin-width}]]
                    ])))
         (unify ticks
                (fn [t]
                  [:g.tick {:transform
                            (svg/translate [(float (x-scale t)) 20])}
                   [:text (str t)]]))
         (svg/axis x-scale ticks :orientation :bottom)
         ]]]]]))
; (numeric-chart n-data)
;        [:g.axis {:transform (svg/translate [0 (- chart-height 50)])}
;           (svg/axis x-scale ticks :orientation :bottom)]


(defn category-chart [chart-data]
  (let [bar-max-width 500
        bar-height 20 other-width 50
        {:keys [field_xpath field_label]} chart-data
        extracted-data (extract-data chart-data)
        s (if (> (count extracted-data) 0)
          (scale/linear :domain [0 (apply max (vals extracted-data))]
                        :range [0 bar-max-width]))]
    [:table#bars.table-bordered
     [:thead
      [:tr
       [:th {:colspan 2} field_label] [:th "Count"]]]
     [:tbody
      (unify extracted-data (fn [[label val]]
                              [:tr
                               [:td label]
                               [:td [:div#bar (style :height (str bar-height "px")
                                                     :width (str (float (s val)) "px")
                                                     :background-color "grey"
                                                     :opacity 0.6
                                                     :margin "2px")]]
                               [:td val]]))]]))


(defn data-for-qn [qn]
  (:body (client/get
          (str "http://localhost:3000/" qn ".json")
          {:as :json})))

(def n-data (data-for-qn "q8"))
(def c-data (data-for-qn "q18a"))

(defn home-page
  []
  (html5
   (gen-page-head "Home")
   [:div [:div#container
     (category-chart c-data)
     (numeric-chart n-data)]]))
