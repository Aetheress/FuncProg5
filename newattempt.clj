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
(defn rail-one [collected remainder rails]
  (cond
    (>= (count remainder) (getCycle rails))
      (rail-one
        (str collected (subs remainder 0 1))
        (subs remainder (getCycle rails))
        rails
      )
    (> (count remainder) 0)
      (str collected (subs remainder 0 1))
    :else
      collected
  )
)

(defn rail-three [collected remainder message rails]
  (cond
    (= (count remainder) (count message))
      (rail-three "" (subs remainder 2) message rails)
    :else
      (cond
        (>= (count remainder) (getCycle rails))
          (rail-three
            (str collected
              (subs remainder 0 1)
            )
            (subs remainder (getCycle rails))
            message
            rails
          )
        (> (count remainder) 0)
          (str collected (subs remainder 0 1))
        :else
          collected
      )
  )
)
