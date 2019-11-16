(ns com.kardans.rationale
  (:require [com.kardans.taxa :as taxa]))

(def db (atom {:person/id {1 {:person/id 1
                              :person/name "Daniel"
                              :person/birth 1981
                              :person/sex "M"}
                           2 {:person/id 2
                              :person/name "Lisa"
                              :person/birth 1982
                              :person/sex "F"}
                           3 {:person/id 3
                              :person/name "Vera"
                              :person/birth 2012
                              :person/sex "F"}
                           4 {:person/id 4
                              :person/name "Kaj"
                              :person/birth 1995
                              :person/sex "M"}
                           5 {:person/id 5
                              :person/name "Stina"
                              :person/birth 2014
                              :person/sex "F"}
                           6 {:person/id 6
                              :person/name "Vesper"
                              :person/birth 2019
                              :person/sex "F"}
                           7 {:person/id 7
                              :person/name "Karl"
                              :person/birth 2019
                              :person/sex "M"}}
               :family/id {1 {:family/id 1
                              :family/name "Sunnerek"
                              :family/members [[:person/id 1]
                                               [:person/id 2]
                                               [:person/id 3]
                                               [:person/id 5]
                                               [:person/id 7]]}}}))



(defn validate-input [{:keys [family/id] :as in}]

  (if (and (contains? in :family/id)
           (number? id))
    (taxa/taxon id)
    (taxa/taxon {:reason "family/id not a number"
                 :input in} ::taxa/err)))

(defn get-family [{:keys [family/id]}]
  (if-some [family (get-in @db [:family/id id])]
    (taxa/taxon family)
    (taxa/taxon {:reason (str "Could not find family with id " id)}
                ::taxa/err)))

(defn get-members [family]
  (let [members (map #(get-in @db %)
                     (-> family :family/members))]
    (if (empty? members)
      (taxa/taxon {::reason "No family members"} :taxa/err)
      (taxa/taxon members))))

(defn filter-kids [members]
  (let [current-year (.getValue (java.time.Year/now))
        children (filter #(< (- current-year (:person/birth %))
                             18)
                         members)]
    (if (empty? children)
      (taxa/taxon {:reason "No children"} :taxa/err)
      (taxa/taxon children))))

(defn api [in]
  (taxa/rel-> in
              validate-input
              get-family
              get-members
              filter-kids
              ))

(api {:family/id 1})


(let [plan [{:fn validate-input
             :ret x}
            {:fn get-family
             :args [db x]
             :ret y}
            {:fn get-members
             :ret z}]]
  (taxa/exec plan


             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defn f [plan & {:keys [executor tag] :or {executor :default}}]
  {:plan plan
   :executor executor})

(f [{:fn :f}] :executor :async)

(comment

  (let [plan [{:fn f
               :ret x}
              {:fn g
               :args [db x]
               :ret y}
              {:fn h
               :args [y]
               :ret z}]]
    (taxa/exec plan ))
  )
