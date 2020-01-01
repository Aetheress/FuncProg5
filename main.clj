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
  (if (not= row 1);;if the row isnt the first
    (if (= (count remainder) (count message));;manipulate the string so that the first letter of the rail is in the first position
      (rail 
        "" 
        (subs remainder (- row 1)) 
        message rails row "no")
      (if (= rails row);;if the row is the last
        (cond
          (>= (count remainder) (+ (getCycle rails) 1));;check for whether there are any more letters to get
            (rail 
              (str collected (subs remainder 0 1)) 
              (subs remainder (* (- rails 1) 2))
              message rails row "no")
          (> (count remainder) 0);;acquire the last letter in certain scenarios
            (str collected (subs remainder 0 1))
          :else;;not sure if this has an actual purpose but this is intended for when the remainder is empty
            collected
        )
        (cond;;if the row is one of the middle rows
          (>= (count remainder) (/ (+ (getCycle rails) 1) 2));;check for whether there are any more letters to get whilst paying attention that middle rows have more letters than side rows
            (cond
              (= shift "no");;the shift variable is used to determine which gap size is present at the given moment, as sometimes the middle rows will have cycling gap sizes
                (rail
                  (str collected (subs remainder 0 1))
                  (subs remainder (- (* (- rails 1) 2) (* (- row 1) 2))) message rails row "yes")
              (= shift "yes")
                (rail
                  (str collected (subs remainder 0 1))
                  (subs remainder (* (- row 1) 2)) message rails row "no")
            )
          (> (count remainder) 0);;acquire the last letter in certain scenarios
            (str collected (subs remainder 0 1))
          :else
            collected
        )
      )
    )
    (cond;;if the row is the first row
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
