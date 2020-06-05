(ns com.kardans.alpha.data-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exec
;;;

(defn exec
  ""
  [plan & {:keys [executor tag]
           :or {executor :default
                tag :com.kardans.taxa/ok}}]
  {:plan plan
   :executor executor
   :tag tag})


(comment

  (exec [{:f :f}])
  (exec [{:f :f}] :executor :async)
  (exec [{:f :f}] :executor :siappari :tag ::err)

  )
