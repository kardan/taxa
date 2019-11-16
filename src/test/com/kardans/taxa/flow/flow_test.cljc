(ns com.kardans.taxa.flow.flow-test
  (:require
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
   [com.kardans.taxa :as taxa]
   #?(:clj [com.kardans.taxa.flow.clj :refer [taxed-let while->]]
      :cljs [com.kardans.taxa.flow.cljs :refer-macros [taxed-let while->]])
   [com.kardans.taxa.flow.test-fns :as fns]))

;; Custom hierarchy
(def custom-hierarchy (atom (make-hierarchy)))
(taxa/in-taxa derive custom-hierarchy ::car ::vehicle)
(taxa/in-taxa derive custom-hierarchy ::station-wagon ::car)
(taxa/in-taxa derive custom-hierarchy ::motorcycle ::vehicle)

(defn motorcylize [t]
  (-> (taxa/thing t)
      (assoc :motorcycle true)
      (taxa/taxon ::motorcycle)))

(deftest taxed-let-test

  (testing "Base case, binding value"
    (let [t1 (taxa/taxon {:k :v})]
      (is (= (assoc-in t1 [::taxa/thing :f1] "f1")
             (taxed-let [t2 t1]
                            (fns/f1 t2))))))

  (testing "Base case, binding via function call"
    (is (= (-> (taxa/taxon {:k :v})
               (assoc-in [::taxa/thing :f1] "f1"))
           (taxed-let [t (taxa/taxon {:k :v})]
                          (fns/f1 t)))))

  (testing "Else case"
    (let [expected (-> (taxa/taxon {:k :v} ::taxa/err)
                       (assoc-in [::taxa/thing :f2] "f2"))]
      (is (= expected
             (taxed-let [t (taxa/taxon {:k :v} ::taxa/err)]
                            (fns/f1 t)
                            (fns/f2 t))))))

  (testing "Custom hierarchy"
    (is (= (-> (taxa/taxon {:k :v} ::station-wagon)
               (assoc-in [::taxa/thing :f1] "f1"))
           (taxed-let [t (taxa/taxon {:k :v} ::station-wagon)]
                          {:hierarchy custom-hierarchy
                           :parent ::car}
                          (fns/f1 t)
                          (fns/f2 t)))))

  (testing "Custom hierarchy else branch"
    (is (= (-> (taxa/taxon {:k :v} ::motorcycle)
               (assoc-in [::taxa/thing :f2] "f2"))
           (taxed-let [t (taxa/taxon {:k :v} ::motorcycle)]
                          {:hierarchy custom-hierarchy
                           :parent ::car}
                          (fns/f1 t)
                          (fns/f2 t)))))

  (testing "Custom hierarchy but faulty parent for else branch"
    (is (= (-> (taxa/taxon {:k :v} ::car)
               (assoc-in [::taxa/thing :f2] "f2"))
           (taxed-let [t (taxa/taxon {:k :v} ::car)]
                          {:hierarchy custom-hierarchy}
                          (fns/f1 t)
                          (fns/f2 t)))))

  (testing "Empty opts"
    (is (= (-> (taxa/taxon {:k :v})
               (assoc-in [::taxa/thing :f1] "f1"))
           (taxed-let [t (taxa/taxon {:k :v})]
                          {}
                          (fns/f1 t)
                          (fns/f2 t))))))

(deftest while->-test

  (testing "identity"
    (is (= {:k :v}
           {:k :v})))

  (testing "No functions"
      (is (= (taxa/thing (while-> {:k :v}))
             {:k :v})))

  (testing "One function"
    (let [t (while-> {:k :v} fns/f1)]
      (is (= (taxa/tag t) ::taxa/ok))
      (is (= (taxa/thing t) {:k :v
                             :f1 "f1"}))))

  (testing "Flow two functions"
    (let [t (while-> {:k :v} fns/f1 fns/f2)]
      (is (= (taxa/tag t) ::taxa/ok))
      (is (= (taxa/thing t) {:k :v
                             :f1 "f1"
                             :f2 "f2"}))))

  (testing "Short circut  functions"
    (let [t (while-> {:k :v} fns/f1 fns/f3 fns/f2)]
      (is (= (taxa/tag t) ::taxa/err))
      (is (= (taxa/thing t) {:k :v
                             :f1 "f1"
                             :f3 "f3"}))))

  (testing "Meta on expr"
    (let [m {:meta "meta"}
          exp (with-meta {:k :v} m)
          t (while-> exp fns/f1 fns/f2)]
      (is (= (-> t taxa/thing meta)
             m))))

  (testing "custom hierarchy & short circuit"
    (let [r (while-> (taxa/taxon {:k :v} ::station-wagon)
              {:hierarchy custom-hierarchy
               :parent ::car}
              fns/f1
              motorcylize
              fns/f2)
          {:keys [f1 f2 motorcycle]} (taxa/thing r)]
      (is (= ::motorcycle (taxa/tag r)))
      (is (= "f1" f1))
      (is (= true motorcycle))
      (is (nil? f2))))

  )

(comment
  #?(:cljs (run-tests))
  )
