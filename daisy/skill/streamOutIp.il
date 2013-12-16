;; Snippets to stream out Ips
;; This is used for the stream-out process
;; Should be copied into the repo area / skill

(defun poCellNameMap (libName cellName viewName)
  (prog (mapName)
	(if (not (equal cellName "vcoAdcTop"))	    
	    (sprintf mapName "vcoAdc_%s" cellName) )
	(return mapName)))
