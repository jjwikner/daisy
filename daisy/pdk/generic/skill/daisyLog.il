
(dbCreatePolygon 
 (hiGetCurrentWindow)->cellView
 (list "text" "drawing2")

 (setq polyList 
       (list (list -2 4.2) (list -1 4.2) 
	     (list -1 3*tan(PI/6)) (list  0 2*tan(PI/6) )
;;	     (list 1 3*tan(PI/6)) (list 1 4.2) 
;;	     (list 2 4.2) (list 2 2*tan(PI/6) )
	     (list 0 0) (list -2 2*tan(PI/6) )
	     )
       )
 )


theta = 2*PI/3;

rotMat = (list (list (cos theta) -(sin theta))
	       (list (sin theta)  (cos theta)))

newPolyList = (list )

(foreach point polyList
	 
	 newX = (car point) * (caar rotMat) + (cadar rotMat) * (cadr point)
	 newY = (car point) * (caadr rotMat) + (cadadr rotMat) * (cadr point)
	 (setq newPolyList (append (list (list newX newY)) newPolyList)))

	 
(dbCreatePolygon 
 (hiGetCurrentWindow)->cellView
 (list "text" "drawing2")

 (setq polyList newPolyList)
 )
newPolyList = (list )

(foreach point polyList
	 
	 newX = (car point) * (caar rotMat) + (cadar rotMat) * (cadr point)
	 newY = (car point) * (caadr rotMat) + (cadadr rotMat) * (cadr point)
	 (setq newPolyList (append (list (list newX newY)) newPolyList)))

	 
(dbCreatePolygon 
 (hiGetCurrentWindow)->cellView
 (list "text" "drawing2")

 (setq polyList newPolyList)
 )
