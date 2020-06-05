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


(deftest taxed?
  (is (taxa/taxed? ::taxa/root))
  (is (taxa/taxed? ::taxa/ok))
  (is (taxa/taxed? ::taxa/err))
  (is (taxa/taxed? ::taxa/ok {:parent ::taxa/ok}))
  (is (not (taxa/taxed? ::taxa/err {:parent ::taxa/ok})))
  (is (taxa/taxed? (taxa/taxon {:k :v} ::taxa/ok) {:parent ::taxa/ok}))
  (is (not (taxa/taxed? (taxa/taxon {:k :v} ::what) {:parent ::taxa/ok})))
  (is (not  (taxa/taxed? ::ok))))


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

    (testing "related?"
      (is (taxa/taxed? ::taxa/ok))
      (is (taxa/taxed? (taxa/taxon {:k :v})))
      (is (taxa/taxed? ::car {:hierarchy test-hierarchy
                              :parent ::vehicle}))
      (is (taxa/taxed? (taxa/taxon {:brand "saab"} ::car)
                       {:hierarchy test-hierarchy
                        :parent ::vehicle}))
      (is (not (taxa/taxed? (taxa/taxon {:brand "saab"} ::mouse)
                            {:hierarchy test-hierarchy
                             :parent ::vehicle}))))

    (testing "Taxed? in updated updated-hierarchy"
      (is (taxa/taxed? ::car {:hierarchy test-hierarchy
                              :parent ::vehicle}))
      (is (taxa/taxed? (taxa/taxon {:k :v} ::car) {:hierarchy test-hierarchy
                                                   :parent ::vehicle}))
      (is (not (taxa/taxed? (taxa/taxon {:k :v} ::cat) {:hierarchy test-hierarchy
                                                        :parent ::vehicle})))
      (is (taxa/taxed? ::car {:hierarchy test-hierarchy
                              :parent (taxa/taxon {:k :v} ::vehicle)})))

    (testing "Applying not added function"
      (is (thrown? UnsupportedOperationException
                   (taxa/in-taxa assoc test-hierarchy ::mouse ::vehicle))))))
