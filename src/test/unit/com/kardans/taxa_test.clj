(ns com.kardans.taxa-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.kardans.taxa :as taxa]))


(deftest tag
  (let [thing {:k :v}]

    (testing "taxon"
      (is (nil? (taxa/tag {})))
      (is (= ::taxa/ok
             (taxa/tag {::taxa/tag ::taxa/ok
                        ::taxa/thing thing})))
      (is (= ::taxa/ok
             (-> thing taxa/taxon taxa/tag)))
      (is (= ::taxa/err
             (taxa/tag (taxa/taxon thing ::taxa/err)))))

    (testing "keyword"
      (is (= ::taxa/ok
             (taxa/tag ::taxa/ok))))

    (testing "variant"
      (is (nil? (taxa/tag [])))
      (is (= ::taxa/ok
             (taxa/tag [::taxa/ok ])))
      (is (= ::taxa/ok
             (taxa/tag [::taxa/ok thing]))))

    (testing "default"
      (is (nil? (taxa/tag nil)))
      (is (nil? (taxa/tag "")))
      (is (nil? (taxa/tag 1))))))


(deftest thing
  (let [thing {:k :v}]

    (testing "taxon"
      (is (= thing
             (taxa/thing (taxa/taxon thing)))))

    (testing "variant"
      (is (= thing
             (taxa/thing [::taxa/ok thing]))))))


(deftest taxon
  (let [thing {:k :v}]

    (testing "default"
      (is (= {::taxa/tag ::taxa/ok
              ::taxa/thing thing}
             (taxa/taxon thing))))

    (testing "alternative tag"
      (is (= {::taxa/tag ::taxa/err
              ::taxa/thing thing}
             (taxa/taxon thing ::taxa/err))))))


(deftest taxon->variant
  (testing "thing->taxon->variant->[tag thing] round trip"
    (let [thing {:k :v}
          [new-tag new-thing] (-> thing taxa/taxon taxa/taxon->variant)]
      (is (= new-tag ::taxa/ok))
      (is (= new-thing thing)))))


(deftest taxon?
  (testing "Is taxon"
    (is (taxa/taxon? (taxa/taxon {:k :v})))
    (is (not (taxa/taxon? {:k :v})))
    (is (not (taxa/taxon? nil)))
    (is (not (taxa/taxon? {::tag :tag})))))


(deftest taxed?
  (testing "tags"
    (is (taxa/taxed? ::taxa/root))
    (is (taxa/taxed? ::taxa/err))
    (is (taxa/taxed? ::taxa/ok))
    (is (not (taxa/taxed? ::taxa/maybe)))
    (is (not (taxa/taxed? :ok))))

  (testing "taxons"
    (is (taxa/taxed? (taxa/taxon {:k :v})))
    (is (taxa/taxed? (taxa/taxon {:k :v} ::taxa/ok)))
    (is (taxa/taxed? (taxa/taxon {:k :v} ::taxa/err)))
    (is (not (taxa/taxed? (taxa/taxon {:k :v} ::taxa/maybe))))
    (is (not (taxa/taxed? (taxa/taxon {:k :v} :ok)))))

  (testing "oddities"
    (is (not (taxa/taxed? nil)))
    (is (not (taxa/taxed? ())))
    (is (not (taxa/taxed? false)))))


(deftest non-and-effecting-fns
  (testing "Effecting-fns wrapped in atom"
    (is (= (type taxa/effecting-fns)
           clojure.lang.Atom)))

  (testing "Non-effecting-fns wrapped in atom"
    (is (= (type taxa/non-effecting-fns)
           clojure.lang.Atom))))


(deftest custom-hierarchy
  (let [test-hierarchy (atom (make-hierarchy))]

    (testing "Empty hierarchy"
      (let [{:keys [parents descendants ancestors] :as th} @test-hierarchy]
        (is (every? #(contains? th % ) [:ancestors :descendants :parents]))
        (is (empty? parents))
        (is (empty? descendants))
        (is (empty? ancestors))))

    (testing "Hierarchy alteration"
      (let [{:keys [parents descendants ancestors] :as updated-hierarchy}
            (taxa/in-taxa derive test-hierarchy ::car ::vehicle)]
        (is (contains? (-> updated-hierarchy :parents ::car)
                       ::vehicle))
        (is (contains? (-> updated-hierarchy :ancestors ::car)
                       ::vehicle))
        (is (contains? (-> updated-hierarchy :descendants ::vehicle)
                       ::car))))

    (testing "Taxed? in updated updated-hierarchy"
      (is (taxa/taxed? ::car test-hierarchy ::vehicle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test fns
;;;

(defn f1
  [thing]
  (taxa/taxon (assoc thing :f1 "f1")))

(defn f2
  ([thing]
   (f2 thing "f2"))
  ([thing arg]
   (taxa/taxon (assoc thing :f2 arg))))

(defn f3
  [thing]
  (taxa/taxon (assoc thing :f3 "f3") ::taxa/err))



(deftest when-rel

  (testing "Default tag case"
    (let [{:keys [::taxa/thing]} (taxa/when-rel [t (f1 {:k :v})]
                                                (f2 (taxa/thing t)))]
      (is (= thing {:k :v, :f1 "f1", :f2 "f2"}))))

  (testing "Custom tag case"
    (let [{:keys [::taxa/thing]} (taxa/when-rel [t (f3 {:k :v})]
                                                (f1 (taxa/thing t))
                                                ::taxa/err)]
      (is (= thing {:k :v :f1 "f1" :f3 "f3"}))))

  (testing "Failing tag custom tag case"
    (let [{:keys [::taxa/thing]} (taxa/when-rel [t (f3 {:k :v})]
                                                (f1 (taxa/thing t)))]
      (is (= thing {:k :v :f3 "f3"})))))


;; success tag in current namespace derived from ::taxa/ok

(defn f4
  [thing]
  (let [{:keys [token]} (meta thing)]
    (taxa/taxon (assoc thing :f4 token) ::success)))

(taxa/in-taxa derive ::success ::taxa/ok)

(deftest rel->
  (testing "Simple"
    (is (= (taxa/rel-> {:k :v}
                       f1
                       f2)
           (taxa/taxon {:k :v
                        :f1 "f1"
                        :f2 "f2"}))))

  (testing "Short circut"
    (let [{:keys [::taxa/thing ::taxa/err-ctx] :as t} (taxa/rel-> {:k :v}
                                                                  f1
                                                                  f3
                                                                  f2)]

      (clojure.pprint/pprint t)
      (is (= (taxa/tag t) ::taxa/err))
      (is (= thing {:k :v :f1 "f1" :f3 "f3"}))
      (is (= (-> err-ctx ::taxa/err-thing)
             {:k :v :f1 "f1"}))
      (is (= (-> err-ctx ::taxa/err-fn) f3))))


  (testing "Local tag using meta data"

    (let [m (with-meta {:k :v} {:token "abc123"})
          t (taxa/rel-> m
                        f1
                        f4
                        f2)]
      (is (= (taxa/tag t) ::taxa/ok))
      (is (= (taxa/thing t)
             (assoc m
                    :f1 "f1"
                    :f2 "f2"
                    :f4 "abc123"))))))
