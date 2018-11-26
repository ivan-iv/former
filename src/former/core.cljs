(ns former.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(defn- check-by-rules [rules values field]
  (some (fn [{msg :msg test-fn :test}]
          (let [ok (test-fn (get values field) values field)]
            (when-not ok msg)))
        rules))

(defn- check-by-plan [plan values field]
  (when-let [p (get plan field)]
    (check-by-rules (:rules p) values field)))

(defn- cmap [coll f & args]
  (reduce (fn [acc x] (->> x (conj (vec args)) (apply f) (merge acc)))
          {}
          coll))

(defn- validate-field
  "Validate field and all of it dependencies recusively with correct handling of cyclical dependencies"
  [plan values field]
  (letfn [(validate [prev-validated errs f]
            (when-not (some (set [f]) prev-validated)
              (let [field-err {f (check-by-plan plan values f)}
                    deps-errs (cmap (get-in plan [f :deps])
                                    validate
                                    (conj prev-validated f)
                                    errs)]
                (merge errs field-err deps-errs))))]
    (validate [] {} field)))

(defn- validate-all-fields [plan values]
  (cmap (keys plan)
        (fn [p v f] (hash-map f (check-by-plan p v f)))
        plan
        values))

(defn- filter-by-touched [errors touched]
  (select-keys errors (for [[k _] errors :when (get touched k)] k)))

(defn- has-errors? [errors]
  (not-every? empty? (vals errors)))

(defn- touch-all [plan]
  (reduce-kv (fn [acc k _] (assoc acc k true)) {} plan))

(defn- validate-field! [a plan field]
  (let [errors (merge (:errors @a)
                      (validate-field plan (:values @a) field))]
    (swap! a assoc :errors (filter-by-touched errors (:touched @a)))))

(defn- validate-all-fields! [a plan]
  (let [errors (validate-all-fields plan (:values @a))]
    (swap! a assoc :errors (filter-by-touched errors (:touched @a)))))

(defn- touch-all! [a plan]
  (swap! a assoc :touched (touch-all plan)))

(defn- update-field! [a value field]
  (swap! a assoc-in [:values field] value))

(defn- touch-field! [a field]
  (swap! a assoc-in [:touched field] true))

(defn former
  ([form a]
   (former form a {}))
  ([form a plan]
   (let [handle-change (fn [field value]
                         (update-field! a value field)
                         (validate-field! a plan field))
         handle-touch (fn [field]
                        (touch-field! a field)
                        (validate-field! a plan field))]
     (fn [props]
       [form (merge props {:values (:values @a)
                           :errors (:errors @a)
                           :invalid (has-errors? (:errors @a))
                           :on-change handle-change
                           :on-touch handle-touch
                           :on-submit (fn [e]
                                        (when e (.preventDefault e))
                                        (touch-all! a plan)
                                        (validate-all-fields! a plan)
                                        (when-let [on-submit (:on-submit props)]
                                          (when-not (has-errors? (:errors @a))
                                            (on-submit (:values @a)))))})]))))

;; Example

(def only-digit {:msg "Allowed only numbers"
                 :test #(.test #"^\d+$" %)})

(def only-alpha {:msg "Allowed only letters"
                 :test #(.test #"^[a-zA-Z]+$" %)})

(def required {:msg "Field is required"
               :test #(if (string? %)
                        (pos? (count %))
                        (some? %))})

(def age-dep-on-name {:msg "If name is John, age must be greater 10"
                      :test (fn [value values field]
                              (if (= (:name values) "John")
                                (> (int value) 10)
                                true))})

(defonce app-state (r/atom {:values {:name "John"
                                     :age 10}}))

;; @app-state

(defn input [{:keys [value field on-change on-blur placeholder]}]
  [:input {:type "input"
           :value value
           :name field
           :on-change (fn [e] (on-change field (.. e -target -value)))
           :on-blur (fn [_] (on-blur field))
           :placeholder placeholder}])

(defn error-msg [{:keys [err]}]
  (when err
    [:div err]))

(def person-form
  (former
   (fn [{:keys [on-change on-touch on-submit values errors invalid]}]
     [:form {:on-submit on-submit :auto-complete "off"}
      [:div
       [input {:value (:name values)
               :field :name
               :on-change on-change
               :on-blur on-touch
               :placeholder "Name"}]
       [error-msg {:err (:name errors)}]]
      [:div
       [input {:value (:age values)
               :field :age
               :on-change on-change
               :on-blur on-touch
               :placeholder "Age"}]
       [error-msg {:err (:age errors)}]]
      [:button {:type "submit"
                :disabled invalid}
       "Submit form"]])
   app-state
   {:name {:rules [required only-alpha]
           :deps [:age]}
    :age {:rules [required only-digit age-dep-on-name]}}))

(defn app-component []
  [:div
   [person-form {:on-submit #(println "On Submit:" %)}]])

(defn mountit [app]
  (r/render [app]
            (.getElementById js/document "app")))

(mountit app-component)
