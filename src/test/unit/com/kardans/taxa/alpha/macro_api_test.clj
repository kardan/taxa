(ns com.kardans.taxa.alpha.macro-api-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.kardans.taxa :as taxa]
            [com.kardans.taxa.alpha.macro-api :as macro-api]))


(taxa/in-taxa derive ::success ::taxa/ok)


(deftest when-taxed?
  (testing "Flow"
    (let [x (macro-api/when-taxed? [t (taxa/taxon {:k :v})]
                                   (assoc t :when true))]
      (is (:when x))))
  (testing "No flow"
    (let [x (macro-api/when-taxed? [t (taxa/taxon {:k :v} ::taxa/err)]
                                   (assoc t :when true))]
      (is (nil? (:when x)))))
  (testing "No flow"
    (let [x (macro-api/when-taxed? [t (taxa/taxon {:k :v})]
                                   (assoc t :when true)
                                   ::taxa/ok)]
      (is (:when x))))
  (testing "No flow"
    (let [x (macro-api/when-taxed? [t (taxa/taxon {:k :v})]
                                   (assoc t :when true)
                                   ::taxa/err)]
      (is (nil? (:when x)))))
  )


;; success tag in current namespace derived from ::taxa/ok


#_(deftest when-rel

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

#_(deftest rel->
    #_(testing "Simple"
        (is (= (taxa/rel-> {:k :v}
                           f1
                           f2)
               (taxa/taxon {:k :v
                            :f1 "f1"
                            :f2 "f2"}))))

    (testing "With args to fn"
      (is (= (taxa/rel-> {:k :v}
                         f1
                         (f2 "F2"))
             (taxa/taxon {:k :v
                          :f1 "f1"
                          :f2 "f2"}))))

    #_(testing "Short circut"
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


    #_(testing "Local tag using meta data"

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
