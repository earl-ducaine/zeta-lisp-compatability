
(in-package :primitive-zlisp)


(defparameter *defstructs*
  '((DEFSTRUCT (ESSENTIAL-LABEL-MIXIN (:CONSTRUCTOR NIL) (:CONC-NAME NIL)
				      (:ALTERANT ALTER-ESSENTIAL-LABEL-MIXIN) (:PREDICATE NIL)
				      (:COPIER NIL) (:TYPE :LIST))
      LABEL-LEFT					;Coordinates of the label, all relative to the
      LABEL-TOP					;edges of the window
      LABEL-RIGHT
      LABEL-BOTTOM)

    (DEFSTRUCT (LABEL-MIXIN (:INCLUDE ESSENTIAL-LABEL-MIXIN) (:CONSTRUCTOR NIL)
			    (:SIZE-SYMBOL LABEL-DEFSTRUCT-SIZE) (:CONC-NAME NIL)
			    (:ALTERANT ALTER-LABEL-MIXIN) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :LIST))
      LABEL-FONT
      LABEL-STRING
      LABEL-VSP
      LABEL-CENTERED
      (LABEL-COLOR *default-label-foreground*)
      (LABEL-BACKGROUND *default-label-background*)
    ;;; >>> added as per HF and Russ's comments that we need control over the label background
      )

    (DEFSTRUCT (MARGIN-REGION
		 (:CONSTRUCTOR           NIL)
		 (:CONC-NAME             NIL)

		 (:ALTERANT              ALTER-MARGIN-REGION)
		 (:PREDICATE             NIL)
		 (:COPIER                NIL)
		 (:TYPE                  :LIST))
      MARGIN-REGION-FUNCTION			;A DTP-SELECT-METHOD for this one
      MARGIN-REGION-MARGIN				;Name of the margin occupied
      MARGIN-REGION-SIZE				;Amount of that to occupy
      MARGIN-REGION-LEFT				;Its area of the screen
      MARGIN-REGION-TOP
      MARGIN-REGION-RIGHT
      MARGIN-REGION-BOTTOM)

    (DEFSTRUCT (MARGIN-SCROLL-REGION
		 (:INCLUDE               MARGIN-REGION)
		 (:CONSTRUCTOR           NIL)
		 (:CONC-NAME             NIL)

		 (:ALTERANT              ALTER-MARGIN-SCROLL-REGION)
		 (:PREDICATE             NIL)
		 (:COPIER                NIL)
		 (:TYPE                  :LIST))
      MARGIN-SCROLL-REGION-EMPTY-MSG		;Message when nothing more to scroll
      MARGIN-SCROLL-REGION-MORE-MSG			;Other message
      MARGIN-SCROLL-REGION-MSG-FONT			;Font for that
      MARGIN-SCROLL-REGION-CURRENT-STRING		;String now displayed in region
      )

    (DEFSTRUCT (CHOICE-BOX (:CONSTRUCTOR           NIL)
			   (:CONC-NAME             NIL)

			   (:ALTERANT              ALTER-CHOICE-BOX)
			   (:PREDICATE             NIL)
			   (:COPIER                NIL)
			   (:TYPE                  :LIST))
      CHOICE-BOX-NAME
      CHOICE-BOX-STATE
      CHOICE-BOX-FUNCTION
      CHOICE-BOX-X1
      CHOICE-BOX-X2)

    (DEFSTRUCT (CHOICE-TYPE (:CONSTRUCTOR           NIL)
			    (:CONC-NAME             NIL)

			    (:ALTERANT              ALTER-CHOICE-TYPE)
			    (:PREDICATE             NIL)
			    (:COPIER                NIL)
			    (:TYPE                  :LIST))
      CHOICE-TYPE-KEYWORD
      CHOICE-TYPE-NAME
      CHOICE-TYPE-ON-POSITIVE-IMPLICATIONS
      CHOICE-TYPE-ON-NEGATIVE-IMPLICATIONS
      CHOICE-TYPE-OFF-POSITIVE-IMPLICATIONS
      CHOICE-TYPE-OFF-NEGATIVE-IMPLICATIONS)

    (DEFSTRUCT (CHOICE-ITEM (:CONSTRUCTOR           NIL)
			    (:CONC-NAME             NIL)

			    (:ALTERANT              ALTER-CHOICE-ITEM)
			    (:PREDICATE             NIL)
			    (:COPIER                NIL)
			    (:TYPE                  :LIST))
      CHOICE-ITEM-ITEM
      CHOICE-ITEM-NAME
      CHOICE-ITEM-BOXES)

    (DEFSTRUCT (GEOMETRY (:TYPE :LIST) (:CONSTRUCTOR NIL) (:CONC-NAME NIL)
			 (:ALTERANT ALTER-GEOMETRY) (:PREDICATE NIL) (:COPIER NIL))
      GEOMETRY-N-COLUMNS
      GEOMETRY-N-ROWS
      GEOMETRY-INSIDE-WIDTH
      GEOMETRY-INSIDE-HEIGHT
      GEOMETRY-MAX-WIDTH
      GEOMETRY-MAX-HEIGHT)

    (DEFSTRUCT (ICON  :NAMED (:type :ARRAY-LEADER) (:INCLUDE FONT) (:CONC-NAME ICON-)
		      (:COPIER NIL))
      DRAW-FUNCTION       ;Function to be invoked to draw the icon.
      ARGUMENTS           ;Additional arguments to pass to FUNCTION when drawing the icon.
      )

    (DEFSTRUCT (TYPEOUT-ITEM :LIST (:CONSTRUCTOR NIL) (:CONC-NAME NIL)
			     (:ALTERANT ALTER-TYPEOUT-ITEM) (:PREDICATE NIL) (:COPIER NIL) (:TYPE :list))
      TYPEOUT-ITEM-TYPE				;For looking in ITEM-TYPE-ALIST
      TYPEOUT-ITEM-ITEM				;Identifier of item
      TYPEOUT-ITEM-LEFT				;Screen area occupied by item, relative to
      TYPEOUT-ITEM-TOP				;sheet, not to margins
      TYPEOUT-ITEM-RIGHT
      TYPEOUT-ITEM-BOTTOM)))
