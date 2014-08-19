(ns small-app.views
  (:use [hiccup.page :only [html5 include-css]]
        [c2.layout.histogram :only [histogram]]
        [c2.core :only [unify]])
  (:require [clojure.string :as str]
            [c2.scale :as scale]
            [clj-http.client :as client]
            [c2.svg :as svg]))

(defn gen-page-head
  [title]
    [:head
       [:title (str "Small-app Homepage " title)]
          (include-css "resources/public/css/styles.css")])



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

(defn- extract-data-for-histogram [chart-data]
  (let [{:keys [data field_xpath]} chart-data
        qn-key (keyword field_xpath)
        non-nil-data (remove (fn [el] (nil? (qn-key el))) data)
        data-with-numbers (map (fn [el]
                                 (assoc el qn-key (Integer. (qn-key el))))
                               non-nil-data)
        binned-data (histogram data-with-numbers :value qn-key :range [0 60] :bins 16)]
    (for [data-item binned-data]
      [(:x (meta data-item))
       (:dx (meta data-item))
       (apply + (map :count data-item))])))

;(extract-data-for-histogram (data-for-qn "q8"))

(defn numeric-chart
  [chart-data]
  (let [chart-width 700 chart-height 300 margin 50 small-margin 2
        extracted-data (extract-data-for-histogram chart-data)
        {:keys [field_label field_xpath]} chart-data
        x-series (map first extracted-data)
        dx-series (map second extracted-data)
        y-series (map last extracted-data)
        xmin (apply min x-series)
        xmax (apply max x-series)
        x-scale (scale/linear :domain [xmin xmax]
                              :range [0 chart-width])
        y-scale (scale/linear :domain [0 (apply max y-series)]
                              :range [0 chart-height])]
    [:table#histogram.table-bordered
     [:thead [:tr [:th field_label]]]
     [:tbody [:tr [:td
        [:svg {:width (+ margin chart-width) :height (+ margin chart-height)}
         (unify extracted-data (fn [[x dx y]]
                               [:g.bar {:transform (str "translate("
                                                        (float (x-scale x)) ","
                                                          (float (y-scale y)) ")")}
                                [:rect {:x 1 :style "fill:grey"
                                        :height (- chart-height (float (y-scale y)))
                                        :width (- (x-scale dx) small-margin)}]]))]]]]]))

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
          (str "https://ona.io/api/v1/charts/196.json?field_name=" qn)
          {:as :json})))

(defn home-page
  []
  (html5
   (gen-page-head "Home")
   [:div [:div#container
     (category-chart (data-for-qn "q18a"))
     (numeric-chart (data-for-qn "q8"))]]))
