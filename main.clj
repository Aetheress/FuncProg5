(require '[clojure.string :as str])

(defn translate[message]
  (str/replace
    (str/replace message #"[^A-Za-z ]" "")
  #" " "_")
)

(def message 
  (translate "Umm hello there?")
)

(defn rail [collected remainder row rows]
  (when remainder
    (cond
      (> row rows)
        "Error."
      (>= (count remainder) rows)
          (str collected 
            (rail 
              (subs remainder (- row 1) row)
              (subs remainder rows)
              row 
              rows
            )
          )
      (>= (count remainder) row)
        (str collected
          (subs remainder (- row 1) row))
      (< (count remainder) rows)
        collected
    )
  )
)

(defn combine [result message row rows]
  (cond
    (= rows row)
      (str result 
        (rail "" message rows rows)
      )
    (> rows row)
      (combine 
        (str result 
            (rail "" message row rows)
        ) 
        message 
        (+ row 1) 
        rows
      )
  )
)

(defn encrypt [message rows]
  (combine "" message 1 rows)
)
