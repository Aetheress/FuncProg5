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

(defn rail [collected remainder message rails row]
  (if (not= row 1)
    (if (= (count remainder) (count message))
      (rail 
        "" 
        (subs remainder (- row 1)) 
        message rails row)
      (if (= rails row)
        (cond
          (>= (count remainder) (+ (getCycle rails) 1))
            (rail 
              (str collected (subs remainder 0 1)) 
              (subs remainder (* (- rails 1) 2))
              message rails row)
          (> (count remainder) 0)
            (str collected (subs remainder 0 1))
          :else
            collected
        )
        nil;;if one of the middle rows
      )
    )
    (cond
      (>= (count remainder) (+ (getCycle rails) 1))
        (rail
          (str collected (subs remainder 0 1)) 
          (subs remainder (* (- rails 1) 2)) 
          message rails row)
      (> (count remainder) 0)
        (str collected (subs remainder 0 1))
      :else
        collected
    )
  )
)
