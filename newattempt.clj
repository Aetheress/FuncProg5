(require '[clojure.string :as str])
(defn translate[message]
  (str/replace
    (str/replace message #"[^A-Za-z ]" "")
  #" " "_")
)
(def message 
  (translate "Umm hello there?")
)

(defn getCycle [rails]
  (- (* rails 2) 2)
)

(defn rail [collected remainder message rails row shift]
  (if (not= row 1)
    (if (= (count remainder) (count message))
      (rail 
        "" 
        (subs remainder (- row 1)) 
        message rails row "no")
      (if (= rails row)
        (cond
          (>= (count remainder) (+ (getCycle rails) 1))
            (rail 
              (str collected (subs remainder 0 1)) 
              (subs remainder (* (- rails 1) 2))
              message rails row "no")
          (> (count remainder) 0)
            (str collected (subs remainder 0 1))
          :else
            collected
        )
        (cond
          (>= (count remainder) (/ (+ (getCycle rails) 1) 2))
            (cond
              (= shift "no")
                (rail
                  (str collected (subs remainder 0 1))
                  (subs remainder (- (* (- rails 1) 2) (* (- row 1) 2))) message rails row "yes")
              (= shift "yes")
                (rail
                  (str collected (subs remainder 0 1))
                  (subs remainder (* (- row 1) 2)) message rails row "no")
            )
          (> (count remainder) 0)
            (str collected (subs remainder 0 1))
          :else
            collected
        )
      )
    )
    (cond
      (>= (count remainder) (+ (getCycle rails) 1))
        (rail
          (str collected (subs remainder 0 1)) 
          (subs remainder (* (- rails 1) 2)) 
          message rails row "no")
      (> (count remainder) 0)
        (str collected (subs remainder 0 1))
      :else
        collected
    )
  )
)
(defn combine [result message rails row]
  (cond
    (= rails row)
      (str result 
        (rail "" message message rails row "no")
      )
    (> rails row)
      (combine 
        (str result 
            (rail "" message message rails row "no")
        ) 
        message 
        rails
        (+ row 1)
      )
  )
)
(defn encrypt [message rails]
  (combine "" message rails 1)
)
