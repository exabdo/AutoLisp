;; Copyright (c) 2025 Abdalrahman
;; Licensed under the Creative Commons Attribution-NonCommercial 4.0 International License.
;; See LICENSE for full details.
;;---------------------=={ Area Label Enhanced (Named Units) }==---------------------;;
;;                                                                     ;;
;;  Allows the user to label picked areas or objects by category       ;;
;;  (Suite, Balcony, Void) and display the areas in an ACAD Table      ;;
;;  or write to file. Includes Duplex option for summing Suite areas.  ;;
;;  User will be prompted for a name for each unit.                    ;;
;;---------------------------------------------------------------------;;
;;  Enhanced by AbdalRahman - 2025                                     ;;
;;  Modifications for Named Units - 2025                               ;;
;;---------------------------------------------------------------------;;
;;  Based on Version 1.9    -    29-10-2011                            ;;
;;  v4 - Fixes for "bad argument type : safearrayp nil" in _centroid_revised ;;
;;       by adding checks for valid safearrays from GetBoundingBox.    ;;
;;---------------------------------------------------------------------;;

(defun c:ATN nil (AreaLabelEnhancedNamed   t))  ;; Areas to Table (Named)
(defun c:AFN nil (AreaLabelEnhancedNamed nil))  ;; Areas to File (Named)

;;------------------------------------------------------------;;

(defun AreaLabelEnhancedNamed ( to-table-flag / *error* _startundo _endundo _centroid_revised _text _open _getobjectid _isannotative
                               acdoc acspc cf cm el fd fo n of om p1 pf pt sf st th ts tx ucsxang ucszdir
                               ;; New variables for enhanced functionality
                               h1 t_num t_suite t_balc t_void t_total ap as de
                               layer_suite layer_balcony layer_void
                               action current-unit-id-string is-duplex
                               suite-area suite-obj suite-polys
                               balcony-area balcony-obj
                               void-area void-obj
                               total-calc-area main-unit-text-obj main-label-pt
                               row-idx initial-prompt-string table_choice tb
                               ;; *al:num related variables are no longer central to unit ID
                         )

  ;;------------------------------------------------------------;;
  ;;                         Adjustments                        ;;
  ;;------------------------------------------------------------;;

  (setq h1 "Detailed Area Table"  ;; Main Heading for the table
        t_num "Unit ID"           ;; Title for Unit Name/Number column (changed from "Unit No.")
        t_suite "Suite Area"      ;; Title for Suite Area column
        t_balc "Balcony Area"     ;; Title for Balcony Area column
        t_void "Void Area"        ;; Title for Void Area column
        t_total "Total Calc. Area" ;; Title for Total Calculated Area column

        layer_suite "ld_unit_boundary" ;; Layer for Suite polylines
        layer_balcony "ld_unit_balcony" ;; Layer for Balcony polylines
        layer_void "Void"             ;; Layer for Void polylines

        pf ""                     ;; Number Prefix (optional, "" if none) - Less relevant for named units
        sf ""                     ;; Number Suffix (optional, "" if none) - Less relevant for named units
        ap ""                     ;; Area Prefix (optional, "" if none)
        as " sqm"                 ;; Area Suffix (e.g., " sqm", " sqft")
        cf 1.0                    ;; Area Conversion Factor (e.g. 1e-6 for mm2->m2, 1.0 for current units)
        fd t                      ;; Use fields to link numbers/objects to table (t=yes, nil=no)
        fo "%lu2%qf1"             ;; Area field formatting
  )

  ;;------------------------------------------------------------;;
  ;;                  Original Helper Functions                 ;;
  ;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if cm (setvar 'CMDECHO cm))
    (if el (progn (entdel el) (setq el nil)))
    (if acdoc (_EndUndo acdoc))
    (if (and of (eq 'FILE (type of))) (close of))
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
        (princ (strcat "\n--> Error: " msg))
    )
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;; Revised centroid function
  (defun _centroid_revised ( space vla_objs_list / reg cen temp_vla_obj minpt maxpt )
    ;; Expects vla_objs_list to be a list of VLA-OBJECTS
    (setq cen nil minpt nil maxpt nil) ; Initialize local variables
    (if (and vla_objs_list (listp vla_objs_list) (car vla_objs_list)
             (vlax-method-applicable-p (car vla_objs_list) 'AddRegion))
        (progn
          (setq reg (vl-catch-all-apply 'vla-AddRegion (list space vla_objs_list)))
          (if (not (vl-catch-all-error-p reg))
              (if (and reg (listp reg) (car reg)) 
                (progn
                  (setq reg (car reg)) 
                  (setq cen (vlax-get reg 'centroid))
                  (vla-delete reg)
                  (setq cen (trans cen 1 0)) 
                )
                (progn (princ "\nWarning: AddRegion did not return a valid region object.") (setq cen nil))
              )
              (progn (princ (strcat "\nWarning: Error creating region for centroid: " (vl-catch-all-error-message reg))) (setq cen nil))
          )
        )
        (if (and vla_objs_list (listp vla_objs_list) (car vla_objs_list))
            (princ "\nWarning: AddRegion method not applicable to selected objects for centroid.")
            (if (null vla_objs_list) (princ "\nWarning: Object list for centroid was empty."))
        )
    )

    (if (not cen)
        (cond
          ((and vla_objs_list (listp vla_objs_list) (= 1 (length vla_objs_list))) 
           (setq temp_vla_obj (car vla_objs_list))
           (if (vlax-method-applicable-p temp_vla_obj 'GetBoundingBox)
               (progn
                 (vl-catch-all-apply 'vlax-invoke (list temp_vla_obj 'GetBoundingBox 'minpt 'maxpt))
                 (if (and minpt maxpt (eq 'SAFEARRAY (type minpt)) (eq 'SAFEARRAY (type maxpt)))
                     (progn
                       (setq minpt (vlax-safearray->list minpt)
                             maxpt (vlax-safearray->list maxpt)
                       )
                       (setq cen (mapcar (function (lambda (a b) (/ (+ a b) 2.0))) minpt maxpt))
                     )
                     (progn (princ "\nWarning: GetBoundingBox failed to return valid points for centroid fallback.") (setq cen nil))
                 )
               )
               (progn (princ "\nWarning: GetBoundingBox not applicable for single object centroid fallback.") (setq cen nil))
           )
          )
          ((and vla_objs_list (listp vla_objs_list) (> (length vla_objs_list) 1)) 
             (princ "\nWarning: Centroid via region failed for multiple objects. Fallback (BoundingBox) is for single object. Cannot determine centroid for multiple objects this way.")
             (setq cen nil)
          )
          (t 
             (if (and vla_objs_list (not (listp vla_objs_list))) (princ "\nWarning: Input for centroid fallback was not a list."))
             (if (null vla_objs_list) (princ "\nWarning: Object list for centroid fallback was empty."))
             (setq cen nil)
          )
        )
    )
    (if (not cen) (progn (princ "\nCannot determine centroid. Using default origin (0,0,0).") (setq cen '(0 0 0))))
    cen
  )

  (defun _text ( space point string height rotation / textobj )
    (setq textobj (vla-addtext space string (vlax-3D-point point) height))
    (vla-put-alignment textobj acalignmentmiddlecenter)
    (vla-put-textalignmentpoint textobj (vlax-3D-point point))
    (vla-put-rotation textobj rotation)
    textobj
  )

  (defun _Open ( target / Shell result )
    (if (setq Shell (vl-catch-all-apply 'vla-getInterfaceObject (list (vlax-get-acad-object) "Shell.Application")))
      (if (not (vl-catch-all-error-p Shell))
        (progn
          (setq result
            (and
              (or (eq 'INT (type target)) (setq target (findfile target)))
              (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply 'vlax-invoke (list Shell 'Open target))
                )
              )
            )
          )
          (vlax-release-object Shell)
        )
        (princ (strcat "\nError getting Shell.Application: " (vl-catch-all-error-message Shell)))
      )
    )
    result
  )

  (defun _GetObjectID ( doc obj )
    (if (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
      (vlax-invoke-method (vla-get-Utility doc) 'GetObjectIdString obj :vlax-false)
      (itoa (vla-get-Objectid obj))
    )
  )

  (defun _isAnnotative ( style / object annotx )
    (and
      (setq object (tblobjname "STYLE" style))
      (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
      (= 1 (cdr (assoc 1070 (reverse annotx))))
    )
  )

  ;;------------------------------------------------------------;;
  ;;                  New Helper Functions                      ;;
  ;;------------------------------------------------------------;;

  (defun select_and_calc_area (type-description layer-name allow-multiple unit-id-prompt /
                               ss total-raw-area idx ent vla-obj selected-objs single-obj-for-link prompt-str)
    (setq total-raw-area 0.0
          selected-objs nil
          single-obj-for-link nil
          idx 0
    )
    ;; unit-id-prompt will now be the user-entered name
    (setq prompt-str (strcat "\nUnit [" unit-id-prompt "] - Select " type-description " polyline(s) on layer <" layer-name ">"))
    (if allow-multiple (setq prompt-str (strcat prompt-str " (Press Enter after selection)")))
    (princ prompt-str)
    (princ ". Or press Enter to skip: ")

    (if allow-multiple
        (setq ss (ssget (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 layer-name))))
        (progn 
          (setvar 'ERRNO 0)
          (setq ss (ssget "_:S:E:L" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 layer-name)))) 
          (if (= 7 (getvar 'ERRNO)) (setq ss nil)) 
        )
    )

    (if ss
        (while (setq ent (ssname ss idx))
          (setq vla-obj (vlax-ename->vla-object ent))
          (if (and vla-obj (vlax-property-available-p vla-obj 'Area)) 
              (if (vlax-curve-isclosed ent) 
                  (progn 
                    (setq total-raw-area (+ total-raw-area (vla-get-area vla-obj)))
                    (setq selected-objs (cons vla-obj selected-objs))
                  )
                  (princ (strcat "\nWarning: Object " (vl-princ-to-string ent) " on layer " layer-name " is not a closed polyline. Ignored."))
              )
          )
          (setq idx (1+ idx))
        )
    )

    (if (and selected-objs (= (length selected-objs) 1))
        (setq single-obj-for-link (car selected-objs))
    )
    (list total-raw-area single-obj-for-link (reverse selected-objs)) 
  )

  (defun format_area_for_table (raw-area vla-obj-for-field use-fields main-doc field-code prefix suffix conv-factor / result-str)
    (if (and use-fields vla-obj-for-field)
        (setq result-str (strcat "%<\\AcObjProp Object(%<\\_ObjId " (_GetObjectID main-doc vla-obj-for-field) ">%).Area \\f \"" field-code "\">%"))
        (setq result-str (strcat prefix (rtos (* conv-factor raw-area) 2 2) suffix))
    )
    result-str
  )

  (defun get_next_action (existing-text-labels / cmd_opt)
    (princ "\n------------------------------------")
    (setq initial-prompt-string "\nEnter command [Add unit")
    (if existing-text-labels (setq initial-prompt-string (strcat initial-prompt-string "/Undo")))
    (setq initial-prompt-string (strcat initial-prompt-string "/Exit] <Add>: "))

    (initget (strcat "Add Undo Exit " (if existing-text-labels "" "Undo@"))) ; Prevent Undo if no labels
    (setq cmd_opt (getkword initial-prompt-string))

    (cond
      ((null cmd_opt) "ADD")
      ((eq cmd_opt "Add") "ADD")
      ((eq cmd_opt "Undo") (if existing-text-labels "UNDO" (progn (princ "\nNothing to undo.") "REPROMPT")))
      ((eq cmd_opt "Exit") "EXIT")
      (t (princ "\nInvalid option.") "REPROMPT")
    )
  )

  ;;------------------------------------------------------------;;
  ;;                       Main Function Body                   ;;
  ;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace))
        ucszdir (trans '(0. 0. 1.) 1 0 t)
        ucsxang (angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucszdir))
        tx nil      ;; List of text objects created for units (for undo)
        row-idx 1   ;; Initial row index for table (after header)
  )
  (_StartUndo acdoc)
  (setq cm (getvar 'CMDECHO)) (setvar 'CMDECHO 0)

  (setq ts
    (/ (getvar 'TEXTSIZE)
      (if (_isAnnotative (getvar 'TEXTSTYLE))
        (cond ( (getvar 'CANNOSCALEVALUE) ) ( 1.0 )) 1.0
      )
    )
  )

  ;; Removed prompt for starting unit number and *al:num logic

  (if to-table-flag
      ;; --- Output to AutoCAD Table ---
      (progn
        (setq th (* 2.5 ts)) 
        (if (< th (* 0.5 ts)) (setq th (* 1.5 ts))) 
        (if (< th 1.0) (setq th (* 1.5 (getvar "DIMTXT")))) 

        (initget "New Add")
        (setq table_choice (getkword "\nCreate [New] table or [Add] to existing? <New>: "))
        (if (or (null table_choice) (eq table_choice "New"))
            (progn
              (setq pt (getpoint "\nPick Point for New Table: "))
              (if pt
                  (progn
                    (setq tb (vla-addtable acspc (vlax-3D-point (trans pt 1 0)) 2 5 th (* 1.5 th))) 
                    (vla-put-columnwidth tb 0 (* 1.2 th)) 
                    (vla-put-columnwidth tb 1 (* 2.0 th)) 
                    (vla-put-columnwidth tb 2 (* 2.0 th)) 
                    (vla-put-columnwidth tb 3 (* 2.0 th)) 
                    (vla-put-columnwidth tb 4 (* 2.2 th)) 
                    (vla-put-direction tb (vlax-3D-point (getvar 'UCSXDIR)))
                    (vla-settext tb 0 0 h1) 
                    (vla-settext tb 1 0 t_num)  ; Uses updated t_num "Unit ID"
                    (vla-settext tb 1 1 t_suite)
                    (vla-settext tb 1 2 t_balc)
                    (vla-settext tb 1 3 t_void)
                    (vla-settext tb 1 4 t_total)
                    (setq row-idx 1) ; Start after header row
                  )
                  (progn (princ "\nNo point selected for table. Exiting.") (_EndUndo acdoc) (exit))
              )
            )
            (progn ; Add to existing table
              (setq p1 (entsel "\nSelect Table to Add to: "))
              (if (and p1 (setq tb (vlax-ename->vla-object (car p1))) (eq "AcDbTable" (vla-get-objectname tb)))
                  (progn
                    (setq row-idx (vla-get-rows tb)) ; Get current number of rows to append
                    (if (not (= 5 (vla-get-columns tb)))
                        (progn
                          (princ "\nSelected table does not have 5 columns. Cannot add to this table. Exiting.")
                          (_EndUndo acdoc) (exit)
                        )
                    )
                  )
                  (progn (princ "\nInvalid table selected or no table selected. Exiting.") (_EndUndo acdoc) (exit))
              )
            )
        )
        (if (not tb) (progn (princ "\nTable could not be created or selected. Exiting.") (_EndUndo acdoc) (exit)))

        (setq action "ADD") 
        (while (not (eq action "EXIT"))
          (cond
            ((eq action "REPROMPT") (setq action (get_next_action tx)))
            ((eq action "UNDO")
              (if tx ; Check if there are any text labels (and corresponding table rows) to undo
                  (progn
                    (vla-deleterows tb row-idx 1) ; Delete the last added row
                    (vla-delete (car tx))         ; Delete the corresponding text object
                    (setq tx (cdr tx))            ; Remove text object from list
                    (setq row-idx (1- row-idx))   ; Decrement row index
                    ;; No *al:num to decrement
                    (princ "\nLast unit undone.")
                  )
                  (princ "\nNothing to undo.")
              )
              (setq action (get_next_action tx))
            )
            ((eq action "ADD")
              ;; Get Unit Name from User
              (setq current-unit-id-string (getstring t "\nEnter Unit Name/Identifier: "))
              
              (if (or (null current-unit-id-string) (= "" current-unit-id-string))
                  (princ "\nUnit Name cannot be empty. Unit not added.")
                  (progn ; Proceed only if unit name is provided
                    (princ (strcat "\n--- Adding Unit: " current-unit-id-string " ---"))

                    (initget "Yes No")
                    (setq is-duplex-str (getkword (strcat "\nIs Suite for Unit [" current-unit-id-string "] a Duplex? (Yes/No) [No]: ")))
                    (setq is-duplex (if (eq is-duplex-str "Yes") t nil))

                    (setq suite-data (select_and_calc_area "Suite" layer_suite is-duplex current-unit-id-string))
                    (setq suite-area (nth 0 suite-data) suite-obj (nth 1 suite-data) suite-polys (nth 2 suite-data))

                    (if (null suite-polys)
                        (princ (strcat "\nNo Suite polylines selected for Unit [" current-unit-id-string "]. Unit not added."))
                        (progn 
                          (setq balcony-data (select_and_calc_area "Balcony" layer_balcony t current-unit-id-string))
                          (setq balcony-area (nth 0 balcony-data) balcony-obj (nth 1 balcony-data))

                          (setq void-data (select_and_calc_area "Void" layer_void t current-unit-id-string))
                          (setq void-area (nth 0 void-data) void-obj (nth 1 void-data))

                          (setq total-calc-area (- (+ suite-area balcony-area) void-area))
                          
                          (if (and suite-polys (listp suite-polys) (car suite-polys))
                              (setq main-label-pt (_centroid_revised acspc suite-polys))
                              (progn
                                (princ "\nNo valid suite polylines to determine label position for table.")
                                (setq main-label-pt nil) 
                              )
                          )
                          (if main-label-pt
                              (setq main-unit-text-obj (_text acspc main-label-pt current-unit-id-string ts ucsxang))
                              (setq main-unit-text-obj nil) 
                          )
                          (if main-unit-text-obj (setq tx (cons main-unit-text-obj tx))) ; Add to list for undo

                          ;; Increment row_idx for new row if it's a new table OR if we are at the end of an existing one
                          ;; If adding to existing, row-idx is already set to (vla-get-rows tb)
                          ;; We always insert a new row, so row_idx should point to where the new row WILL BE.
                          (if (or (eq table_choice "New") (= row-idx (vla-get-rows tb)))
                              (setq row-idx (1+ row-idx))
                              ;; If adding to existing and not at the very end (e.g. after an undo),
                              ;; row-idx is already correct for insertion.
                          )
                          (vla-insertrows tb row-idx th 1)
                          
                          (vla-settext tb row-idx 0 (if (and fd main-unit-text-obj)
                                                       (strcat "%<\\AcObjProp Object(%<\\_ObjId " (_GetObjectID acdoc main-unit-text-obj) ">%).TextString>%")
                                                       current-unit-id-string))
                          (vla-settext tb row-idx 1 (format_area_for_table suite-area suite-obj fd acdoc fo ap as cf))
                          (vla-settext tb row-idx 2 (format_area_for_table balcony-area balcony-obj fd acdoc fo ap as cf))
                          (vla-settext tb row-idx 3 (format_area_for_table void-area void-obj fd acdoc fo ap as cf))
                          (vla-settext tb row-idx 4 (format_area_for_table total-calc-area nil nil acdoc fo ap as cf)) 

                          (princ (strcat "\nUnit [" current-unit-id-string "] added to table."))
                        )
                    )
                  )
              )
              (setq action (get_next_action tx)) 
            )
          )
        )
      )
      ;; --- Output to File ---
      (progn 
        (setq fl (getfiled "Create Output File" (cond ( (assoc "AreaLabelEnhancedNamedFile" *file-history*) (cdr (assoc "AreaLabelEnhancedNamedFile" *file-history*)) ) ( (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) ".txt") )) "txt;csv;xls" 1))
        (if fl (if (not *file-history*) (setq *file-history* nil)) (setq *file-history* (cons (cons "AreaLabelEnhancedNamedFile" (vl-filename-directory fl)) *file-history*)))


        (if (and fl (setq of (open fl "w")))
            (progn
              (setq de (cdr (assoc (strcase (vl-filename-extension fl) t) '((".txt" . "\t") (".csv" . ",") (".xls" . "\t"))))
              )
              (write-line (strcat t_num de t_suite de t_balc de t_void de t_total) of)

              (setq action "ADD")
              (while (not (eq action "EXIT"))
                (cond
                  ((eq action "REPROMPT") (setq action (get_next_action nil))) ; Undo not available in file mode
                  ((eq action "UNDO") (princ "\nUndo not available in file mode.") (setq action (get_next_action nil)))
                  ((eq action "ADD")
                    ;; Get Unit Name from User
                    (setq current-unit-id-string (getstring t "\nEnter Unit Name/Identifier for file: "))

                    (if (or (null current-unit-id-string) (= "" current-unit-id-string))
                        (princ "\nUnit Name cannot be empty. Unit not added to file.")
                        (progn ; Proceed only if unit name is provided
                          (princ (strcat "\n--- Adding Unit: " current-unit-id-string " for file ---"))

                          (initget "Yes No")
                          (setq is-duplex-str (getkword (strcat "\nIs Suite for Unit [" current-unit-id-string "] a Duplex? (Yes/No) [No]: ")))
                          (setq is-duplex (if (eq is-duplex-str "Yes") t nil))

                          (setq suite-data (select_and_calc_area "Suite" layer_suite is-duplex current-unit-id-string))
                          (setq suite-area (nth 0 suite-data) suite-polys (nth 2 suite-data))

                          (if (null suite-polys)
                              (princ (strcat "\nNo Suite polylines selected for Unit [" current-unit-id-string "]. Unit not added to file."))
                              (progn
                                (setq balcony-data (select_and_calc_area "Balcony" layer_balcony t current-unit-id-string))
                                (setq balcony-area (nth 0 balcony-data))
                                (setq void-data (select_and_calc_area "Void" layer_void t current-unit-id-string))
                                (setq void-area (nth 0 void-data))
                                (setq total-calc-area (- (+ suite-area balcony-area) void-area))
                                
                                (if (and suite-polys (listp suite-polys) (car suite-polys)) 
                                    (setq main-label-pt (_centroid_revised acspc suite-polys))
                                    (progn
                                      (princ "\nNo valid suite polylines to determine label position for file mode.")
                                      (setq main-label-pt nil) 
                                    )
                                )
                                (if main-label-pt (_text acspc main-label-pt current-unit-id-string ts ucsxang))

                                (write-line
                                  (strcat current-unit-id-string de
                                          (rtos (* cf suite-area) 2 2) de       
                                          (rtos (* cf balcony-area) 2 2) de
                                          (rtos (* cf void-area) 2 2) de
                                          (rtos (* cf total-calc-area) 2 2)
                                  ) of
                                )
                                (princ (strcat "\nUnit [" current-unit-id-string "] written to file."))
                              )
                          )
                        )
                    )
                    (setq action (get_next_action nil)) ; Pass nil as no text objects list for file mode undo
                  )
                )
              )
              (close of)
              (princ (strcat "\nData written to: " fl))
              (_Open fl) 
            )
            (princ "\nFile could not be opened for writing.")
        )
      )
  )

  (setvar 'CMDECHO cm)
  (_EndUndo acdoc)
  (princ)
)

;; Global variable to store last used directory for file output (optional enhancement)
;; (if (not *file-history*) (setq *file-history* nil))
;; This is better placed inside the function or handled carefully if defined globally.
;; For simplicity, I've added a basic version inside the file section.

;;------------------------------------------------------------;;
(vl-load-com)
(princ "\n:: AreaLabelEnhancedNamed.lsp Loaded (Named Units Mod) ::")
(princ "\n:: Original by Lee Mac © 2011 www.lee-mac.com ::")
(princ "\n:: Enhanced for Abd alRahman 2025 ::")
(princ "\n:: Commands: \"ATN\" for ACAD Table (Named), \"AFN\" for File (Named) ::")
(princ)
;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;
