
;; I hope to add some more examples to this list in the future


;; Create a list of points

(setq pointList (list))

value = 1

(for index 0 9
     (setq pointList (append pointList (list index))))

(printf "%L \n" pointList)

;; Run a foreach loop
