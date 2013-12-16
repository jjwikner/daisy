;;
;; This file is received from Graham Leach at Riverbeck, UK.
;; Daisyhas made some changes so that code uses daisy nomenclature.
;; J Jacob Wikner
;;

(printf " ... loading ribeCheckSaveAll.il \n");

(defun CCSgetCellName (lib_name)
  /*************************************************************************
  * DISCLAIMER: The following code is provided for Cadence customers       *
  * to use at their own risk. The code may require modification to         *
  * satisfy the requirements of any user. The code and any modifications   *
  * to the code may not be compatible with current or future versions of   *
  * Cadence products. THE CODE IS PROVIDED "AS IS" AND WITH NO WARRANTIES, *
  * INCLUDING WITHOUT LIMITATION ANY EXPRESS WARRANTIES OR IMPLIED         *
  * WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.         *
  *************************************************************************/
  (let (( libName "" )
	(my_cell_list (list)))
    (setq libName (ddGetObj lib_name))
    (foreach cellName libName~>cells
	     (setq my_cell_list 
		   (cons cellName my_cell_list)))
    my_cell_list ))


(defun ribeCheckSaveAll (libName)
  ;; Riverbeck
  ;; Graham Leach

  ;; JJW : changed to lisp nomenclature
  ;; JJW : changed some variable names
  (prog (objs)
	
	(printf "Updating transistor properties...\n")
	(printf "Selecting only schematic view types from specified cells...\n")
	(setq ccvs (CCSgetCellName libName))
	(setq newccvs nil)
	(foreach cellId ccvs
		 (foreach viewId cellId~>views
			  (when (member "sch.cdb" viewId~>files~>name )
			    (setq newccvs (cons viewId newccvs)) )))	
	(foreach viewId newccvs
		 (printf "----> Processing (%s %s %s)\n" 
			 viewId~>lib~>name 
			 viewId~>cell~>name 
			 viewId~>name)		 
		 (setq cv (dbOpenCellViewByType 
			   viewId~>lib~>name 
			   viewId~>cell~>name 
			   viewId~>name "" "a"))
		 ;; Check and save each cell.		 
		 (schCheck cv)
		 (dbSave cv) )))
