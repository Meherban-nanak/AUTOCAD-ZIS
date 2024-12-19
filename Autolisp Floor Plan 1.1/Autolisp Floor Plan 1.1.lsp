(defun c:CreatePlotAndFloorPlan (/ lot-length lot-breadth front-yard side-yard rear-yard 
                                     max-front-yard max-rear-yard min-side-yard max-side-yard 
                                     plot-boundary-layer building-boundary-layer pt1 pt2 pt3 
                                     pt4 pt5 pt6 pt7 pt8 building-width building-height 
                                     ext-wall-thick int-wall-thick usable-width-living 
                                     usable-width-kitchen-dining usable-width-bedroom-bathroom 
                                     living-width kitchen-width dining-width bedroom-width 
                                     bathroom-width row-height dim-offset x-offset y-offset)
  (command "DIMUNIT" "4")
  (setvar "INSUNITS" 2)
  ;; Building Regulations / setbacks
  (setq max-front-yard 300.0
        max-rear-yard  300.0
        min-side-yard  36.0
        max-side-yard  60.0) ;setbacks are in inches
  ;; Layer names
  (setq plot-boundary-layer     "Plot Boundary"
        building-boundary-layer "Building Boundary"
        wall-layer              "Wall"
        wallhatch-layer         "Wall Hatch"
        label-layer             "Label"
        dim-layer               "Dimension"
        furniture-layer         "Furniture")
  (setq dim-offset -70.0) ;;Dimension offset for spacing, Adjust this value to control spacing for dimensions
  ;; Draw plot and building boundaries
  (defun create-layer (layer-name color) 
    (if (not (tblsearch "layer" layer-name))  ; Check if layer exists
      (progn 
        (command "._-layer" "New" layer-name "") ; Create new layer
        (command "._-layer" "Color" (itoa color) layer-name "") ; Assign color
      )
    )
  )
  ;; Create required layers
  (create-layer plot-boundary-layer 1) ; Plot Boundary: Red
  (create-layer building-boundary-layer 2) ; Building Boundary: Yellow
  (create-layer wall-layer 3) ; Wall layer: Green
  (create-layer furniture-layer 4) ; Furniture layer: Cyan
  (create-layer dim-layer 7) ; Dimension layer: White
  (create-layer label-layer 6) ; Label layer: Magenta
  (create-layer wallhatch-layer 7) ; Wall Hatch layer: White
  ;; Get user inputs for lot dimensions
  (setq lot-length (* 12 (getreal "\nEnter lot length (in feet): ")))
  (setq lot-breadth (* 12 (getreal "\nEnter lot breadth (in feet): ")))
  ;; Calculate setbacks for yard spaces
  (setq front-yard (min (* 0.25 lot-length) max-front-yard)) ; Front yard: 25% of lot depth, max 25 feet.
  (setq rear-yard (min (* 0.25 lot-length) max-rear-yard)) ; Rear yard: 25% of lot depth, max 25 feet.
  (setq side-yard (max min-side-yard (min (* 0.1 lot-breadth) max-side-yard))) ;Side yard: 10% of lot width, min 3 ft, max 5 ft.
  ;; Define plot boundary points
  (setq pt1 (list 0 0 0) ; Bottom-left
        pt2 (list lot-breadth 0 0) ; Bottom-right
        pt3 (list lot-breadth lot-length 0) ; Top-right
        pt4 (list 0 lot-length 0)) ; Top-left
  ;; Define building boundary points
  (setq pt5 (list side-yard front-yard 0) ; Bottom-left (building)
        pt6 (list (- lot-breadth side-yard) front-yard 0) ; Bottom-right (building)
        pt7 (list (- lot-breadth side-yard) (- lot-length rear-yard) 0) ; Top-right (building)
        pt8 (list side-yard (- lot-length rear-yard) 0)) ; Top-left (building)
  ;; Extract building dimensions from building boundary points
  (setq building-width (distance pt5 pt6)) ; Building width (distance between pt5 and pt6)
  (setq building-height (distance pt5 pt8)) ; Building height (distance between pt5 and pt8)
  ;; Draw plot boundary
  (command "._layer" "Set" plot-boundary-layer "") ; Set Plot Boundary layer
  (command "._pline" pt1 pt2 pt3 pt4 "C") ; Create plot boundary polyline
  ;; Draw building boundary
  (command "._layer" "Set" building-boundary-layer "") ; Set Building Boundary layer
  (command "._pline" pt5 pt6 pt7 pt8 "C") ; Create building boundary polyline
  (setq arrow-size (/ building-height 20)) ;; Set the desired arrow size
  (setq text-height (/ building-height 20)) ;; Set the desired text height
  (setvar "DIMASZ" arrow-size) ;; Apply the arrow size
  (setvar "DIMTXT" text-height) ;; Apply the text height
  ;; Add dimensions for plot boundary & building boundary
  (command "._layer" "Set" dim-layer "") ; Set Dimension layer
  (command "._dimlinear" pt1 pt2 (list (/ lot-breadth 2) dim-offset 0)) ; Width (lot breadth)
  (command "._dimlinear" pt1 pt4 (list dim-offset (/ lot-length 2) 0)) ; Length (lot length)
  (command "._dimlinear" pt5 pt6 (list (/ (+ side-yard (- lot-breadth side-yard)) 2) (+ dim-offset front-yard) 0)) ; Width (building breadth)
  (command "._dimlinear" pt5 pt8 (list (+ dim-offset side-yard) (/ (+ front-yard (- lot-length rear-yard)) 2) 0)) ; Length (building length)
  (setq ext-wall-thick 6) ; set Ext wall thickness in inches 6 inches
  (setq int-wall-thick 6) ;  set Int wall thickness in inches 6 inches
  ;; Calculate usable space
  (setq usable-width-living (- building-width (* 2 ext-wall-thick))) ; Living Room width
  (setq usable-width-kitchen-dining (- building-width (* 2 ext-wall-thick) int-wall-thick)) ; Kitchen + Dining width
  (setq usable-width-bedroom-bathroom (- building-width (* 2 ext-wall-thick) int-wall-thick)) ; Bedroom + Bathroom width
  (setq row-height (/ (- building-height (* 2 ext-wall-thick) (* 2 int-wall-thick)) 3 )) ; Row heights
  (setq living-width usable-width-living) ; Living Room width
  (setq kitchen-width (* (/ 2 3.0) usable-width-kitchen-dining)) ; Kitchen width
  (setq dining-width (* (/ 1 3.0) usable-width-kitchen-dining)) ; Dining width
  (setq bedroom-width (* (/ 2 3.0) usable-width-bedroom-bathroom)) ; Bedroom width
  (setq bathroom-width (* (/ 1 3.0) usable-width-bedroom-bathroom)) ; Bathroom width
  ;; Initialize offsets for drawing, starting from top-right of the building boundary
  (setq x-offset (- (nth 0 pt7) living-width ext-wall-thick))
  (setq y-offset (- (nth 1 pt7) ext-wall-thick))
  ;; Draw the rooms within the building boundary
  (draw-room x-offset y-offset living-width row-height "Living Room") ;; Living Room at top-right
  (setq y-offset (- y-offset row-height int-wall-thick)) ;; Move y-offset down for next row
  (setq x-offset (+ side-yard ext-wall-thick)) ;; Reset x-offset for next row
  (draw-room x-offset y-offset kitchen-width row-height "Kitchen") ;; Kitchen and Dining Room below Living Room
  (setq x-offset (+ x-offset kitchen-width int-wall-thick)) ;; Move x-offset to the right
  (draw-room x-offset y-offset dining-width row-height "Dining")
  (setq x-offset (+ side-yard ext-wall-thick)) ;; Reset x-offset for next row
  (setq y-offset (- y-offset row-height int-wall-thick)) ;; Move y-offset down for next row
  (draw-room x-offset y-offset bedroom-width row-height "Bedroom") ;; Bedroom and Bathroom at the bottom
  (setq x-offset (+ x-offset bedroom-width int-wall-thick)) ;; Move x-offset to the right
  (draw-room x-offset y-offset bathroom-width row-height "Bathroom")
  ;; Hatch walls
   (setq wall-and-boundary-objects (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE,INSERT") ;; Select only polylines
                                           (8 . "Wall,Building Boundary")))) ;; Restrict to specific layers
  (if wall-and-boundary-objects
    (progn
      (command "._layer" "Set" wallhatch-layer "")
      (command "._-HATCH" "Properties" "SOLID" "")
      (command "._-HATCH" "S" wall-and-boundary-objects "")
      (command "")))
  (princ "\nFloor Plan created successfully!")
  (command "_ZOOM" "_E")
)

;; Function to draw rooms
(defun draw-room (x y width height label) 
  (command "._layer" "Set" wall-layer "")
  (command "RECTANGLE" (list x y) (list (+ x width) (- y height)))
  (command "._layer" "Set" label-layer "")
   (cond
    ((equal label "Living Room") 
      (setq text-x (+ x (/ width 1.85))) 
      (setq text-y (- y (/ height 1.85)))
      (command "TEXT" (list text-x text-y) "12" "0" label)
      (command "TEXT" (strcat (rtos text-x 2 2) "," (rtos (- text-y 18) 2 2)) "8" "0" 
           (strcat (rtos (/ width 12) 2 2) "' x " (rtos (/ height 12) 2 2) "'")))
    ((equal label "Kitchen")
      (setq text-x (+ x (/ width 3))) 
      (setq text-y (- y (/ height 1.5)))
      (command "TEXT" (list text-x text-y) "12" "0" label)
      (command "TEXT" (strcat (rtos text-x 2 2) "," (rtos (- text-y 18) 2 2)) "8" "0" 
           (strcat (rtos (/ width 12) 2 2) "' x " (rtos (/ height 12) 2 2) "'")))
    ((equal label "Bedroom") 
      (setq text-x (+ x (/ width 1.75))) 
      (setq text-y (- y (/ height 1.85)))
      (command "TEXT" (list text-x text-y) "12" "0" label)
      (command "TEXT" (strcat (rtos text-x 2 2) "," (rtos (- text-y 18) 2 2)) "8" "0" 
           (strcat (rtos (/ width 12) 2 2) "' x " (rtos (/ height 12) 2 2) "'")))
    ((equal label "Dining") 
      (setq text-x (+ x (/ width 3.5))) 
      (setq text-y (- y (/ height 1.5)))
      (command "TEXT" (list text-x text-y) "12" "0" label)
      (command "TEXT" (strcat (rtos text-x 2 2) "," (rtos (- text-y 18) 2 2)) "8" "0" 
           (strcat (rtos (/ width 12) 2 2) "' x " (rtos (/ height 12) 2 2) "'")))
     ((equal label "Bathroom") 
       (setq text-x (+ x (/ width 3.5))) 
      (setq text-y (- y (/ height 1.85)))
      (command "TEXT" (list text-x text-y) "12" "0" label)
      (command "TEXT" (strcat (rtos text-x 2 2) "," (rtos (- text-y 18) 2 2)) "8" "0" 
           (strcat (rtos (/ width 12) 2 2) "' x " (rtos (/ height 12) 2 2) "'")))
    )
  ;; Add furniture
  (command "._layer" "Set" furniture-layer "")
  (cond
    ((equal label "Living Room") 
     (command "INSERT" "sofa1.1" (list (+ x 1) (- y 1)) "1" "1" "0")
      (command "INSERT" "livwd1.1" (list (+ x width) (- y height)) "1" "1" "0"))
    ((equal label "Kitchen") (command "INSERT" "kit1.1" (list (+ x 0.3) (- y 0.3)) "1" "1" "0"))
    ((equal label "Bedroom") 
      (command "INSERT" "bed1.1" (list (+ x 0.3) (- y 0.3)) "1" "1" "0")
      (command "INSERT" "Wadrobe1.1" (list (+ x 1) (- y height)) "1" "1" "0"))
    ((equal label "Dining") (command "INSERT" "dining-table1.1" (list (+ x (/ width 4)) (- y (/ height 2))) "1" "1" "0"))
    ((equal label "Bathroom") 
      (command "INSERT" "toiletbathwc1.1" (list (+ x 0.3) (- y 0.3)) "1" "1" "0")
      (command "INSERT" "toiletbasin1.1" (list (+ x width) (- y height)) "1" "1" "0"))
  )
  (command "._layer" "Set" wall-layer "")
  (cond
    ((equal label "Living Room") 
      (command "INSERT" "hwindow1.1" (list (+ x (/ width 2.5)) (+ y 6)) "1" "1" "0")
      (command "INSERT" "hwindow1.1" (list (+ x (/ width 10)) (+ y 6)) "1" "1" "0")
      (command "INSERT" "vwindow1.1" (list (- x 6) (- y (/ height 3))) "1" "1" "0")
      (command "INSERT" "maindoor1.1" (list (+ x width) (+ y 6)) "1" "1" "0"))
    ((equal label "Kitchen") 
      (command "INSERT" "vwindow1.1" (list (- x 6) (- y (/ height 3))) "1" "1" "0")
      (command "INSERT" "Hopening1.1" (list (+ x width) (+ y 6)) "1" "1" "0"))
    ((equal label "Bedroom") 
      (command "INSERT" "vwindow1.1" (list (- x 6) (- y (/ height 3))) "1" "1" "0")
      (command "INSERT" "maindoor1.1" (list (+ x width) (+ y 6)) "1" "1" "0"))
    ((equal label "Dining") 
      (command "INSERT" "vwindow1.1" (list (+ x width) (- y (/ height 3))) "1" "1" "0")
      (command "INSERT" "Vopening1.1" (list (- x 6) (- y (/ height 3))) "1" "1" "0"))
    ((equal label "Bathroom") 
      (command "INSERT" "vwindow1.1" (list (+ x width) (- y (/ height 3))) "1" "1" "0")
      (command "INSERT" "toiletdoor1.1" (list (- x 6) (- y height)) "1" "1" "0")
    )
  )
)