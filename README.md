# Taxa [![CI](https://github.com/kardan/taxa/workflows/CI/badge.svg)](https://github.com/kardan/taxa/actions) [![cljdoc badge](https://cljdoc.org/badge/org.clojars.kardan/taxa)](https://cljdoc.org/d/org.clojars.kardan/taxa/CURRENT) [![Clojars Project](https://img.shields.io/clojars/v/com.kardans/taxa.svg)](https://clojars.org/com.kardans/taxa)


A thought experiment in hierarchical domain logic for Clojure & ClojureScript.

## Rationale

```clojure
(ns com.kardans.taxa.rationale
  (:require
   [com.kardans.taxa :as taxa]
   #?(:clj [com.kardans.taxa.flow.clj :refer [while->]]
      :cljs [com.kardans.taxa.flow.cljs :refer-macros [while->]])))


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
                                               [:person/id 7]]}
                           7 {:family/id 7
                              :family/name "Bond"
                              :family/members []}}}))


(defn validate-input
  [in]
  (let [{:keys [family/id] :as in}  (taxa/thing in)]
       (if (and (contains? in :family/id)
                (number? id))
         (taxa/taxon in)
         (taxa/taxon {:reason "family/id not a number"
                      :input in} ::taxa/err))))

(defn get-family
  [in]
  (let [{:keys [family/id]} (taxa/thing in)]
    (if-some [family (get-in @db [:family/id id])]
      (taxa/taxon family)
      (taxa/taxon {:reason (str "Could not find family with id " id)}
                  ::taxa/err))))

(defn get-members
  [in]
  (let [family (taxa/thing in)
        members (map #(get-in @db %)
                     (-> family :family/members))]
    (if (empty? members)
      (taxa/taxon {::reason "No family members"} :taxa/err)
      (taxa/taxon members))))

(defn filter-kids
  [in]
  (let [members (taxa/thing in)
        current-year (.getValue (java.time.Year/now))
        children (filter #(< (- current-year (:person/birth %))
                             18)
                         members)]
    (if (empty? children)
      (taxa/taxon {:reason "No children"} :taxa/err)
      (taxa/taxon children))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
;;;

(defn api
  [in]
  (while-> in
    validate-input
    get-family
    get-members
    filter-kids))

(api {:family/id ""})
;; #:com.kardans.taxa{:tag :com.kardans.taxa/err,
;;                    :thing {:reason "family/id not a number",
;;                            :input #:family{:id ""}}}

(api {:family/id 0})
;; #:com.kardans.taxa{:tag :com.kardans.taxa/err,
;;                    :thing {:reason "Could not find family with id 0"}}

(api {:family/id 7})
;; #:com.kardans.taxa{:tag :taxa/err,
;;                    :thing #:com.kardans.rationale{:reason "No family members"}}

(api {:family/id 1})
;; #:com.kardans.taxa{:tag :com.kardans.taxa/ok,
;;                    :thing (#:person{:id 3, :name "Vera", :birth 2012, :sex "F"}
;;                            #:person{:id 5, :name "Stina", :birth 2014, :sex "F"}
;;                            #:person{:id 7, :name "Karl", :birth 2019, :sex "M"})}



```


## Run Clojure tests
```
./bin/kaocha clj
```

## Run ClojureScript tests
```
./bin/kaocha cljs
```

## License
Copyright © 2019 - present Daniel Sunnerek

Distributed under the Eclipse Public License version 2.0
