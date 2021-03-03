;;;; matrix-animate.lisp

;; TODO: Make sure to handle errors from SDL2 calls.

(in-package #:matrix-animate)

(defun run ()
  (sdl2:make-this-thread-main (lambda () (main "../neuralnet/output/voltage_matrix.dat"))))

(defmacro with-window-initialized ((window &key title width height) &body body)
  `(progn
     (sdl2:init :video)
     (sdl2-ttf:init)
     (let ((,window (sdl2:create-window :title ,title :w ,width :h ,height)))
       ,@body
       (sdl2:destroy-window ,window))
     (sdl2-ttf:quit)
     (sdl2:quit)))

(defmacro with-font ((font &key fontpath size) &body body)
  `(let ((,font (sdl2-ttf:open-font ,fontpath ,size)))
     ,@body
     (sdl2-ttf:close-font font)))

(defmacro with-renderer ((renderer &key window) &body body)
  `(let ((,renderer (sdl2:create-renderer ,window)))
     ,@body
     (sdl2:destroy-renderer renderer)))

(defun make-lerper (x0 x1 y0 y1)
  (lambda (x)
    (+ y0 (* (- x x0) (/ (- y1 y0) (- x1 x0))))))

(defun color-map-lerper (x0 x1 r0 g0 b0 r1 g1 b1)
  (let ((rlerp (make-lerper x0 x1 r0 r1))
	(glerp (make-lerper x0 x1 g0 g1))
	(blerp (make-lerper x0 x1 b0 b1)))
    (labels ((valid-color-value (x)
	       (let ((value (round x)))
		 (cond ((> value 255) 255)
		       ((< value 0) 0)
		       (t value)))))
      (lambda (x)
	(mapcar #'valid-color-value (list (funcall rlerp x) (funcall glerp x) (funcall blerp x)))))))

(defun red (c) (first c))
(defun green (c) (second c))
(defun blue (c) (third c))

(let* ((low-voltage -80.0)
       (high-voltage 20.0)
       (clerp (color-map-lerper low-voltage high-voltage 122 31 110 184 172 9)))
  (defun draw-color-map-cell (renderer x y width height value)
    (let ((color (funcall clerp value)))
      (sdl2:set-render-draw-color renderer (red color) (green color) (blue color) 255)
      (sdl2:render-fill-rect renderer (sdl2:make-rect x y width height))))
  (defun draw-legend (renderer font x y width height)
    (let* ((low-text-surface (sdl2-ttf:render-text-solid font (format nil "~a" low-voltage) 80 80 80 255))
	   (high-text-surface (sdl2-ttf:render-text-solid font (format nil "~a" high-voltage) 80 80 80 255))
	   (low-width (sdl2:surface-width low-text-surface))
	   (low-height (sdl2:surface-height low-text-surface))
	   (high-width (sdl2:surface-width high-text-surface))
	   (high-height (sdl2:surface-height high-text-surface))
	   (low-text-texture (sdl2:create-texture-from-surface renderer low-text-surface))
	   (high-text-texture (sdl2:create-texture-from-surface renderer high-text-surface))
	   (padding-width 10)
	   (color-bar-width (- width (+ low-width high-width) (* 2 padding-width)))
	   (color-bar-height height)
	   (color-bar-x (+ x low-width padding-width))
	   (color-bar-y y))
      (sdl2:render-copy renderer low-text-texture
			:dest-rect
			(sdl2:make-rect (round (+ x (/ padding-width 2)))
					(round (- (+ y (/ color-bar-height 2)) (/ low-height 2)))
					low-width
					low-height))
      (sdl2:render-copy renderer high-text-texture
			:dest-rect
			(sdl2:make-rect (round (+ color-bar-x color-bar-width (/ padding-width 2)))
					(round (- (+ y (/ color-bar-height 2)) (/ low-height 2)))
					high-width
					high-height))
      (let* ((legend-start color-bar-x)
	     (legend-end (+ color-bar-x color-bar-width))
	     (legend-lerp (make-lerper legend-start legend-end low-voltage high-voltage)))
	(loop :for rect-x :from color-bar-x :to (+ color-bar-x color-bar-width) :do
	  (let ((color (funcall clerp (funcall legend-lerp rect-x))))
	    (sdl2:set-render-draw-color renderer (red color) (green color) (blue color) 255)
	    (sdl2:render-fill-rect renderer (sdl2:make-rect rect-x color-bar-y 1 color-bar-height))))))))

(defun valid-number-first-char-p (c)
  (or (digit-char-p c) (eq c #\-)))

(defun parse-line-of-numbers (line)
  (mapcar #'parse-number:parse-number
	  (cl-ppcre:split "\\s+" line)))

(defun make-voltage-matrices (filename) 
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((parse-state :top)
	  (depth 0)
	  (list-of-rows nil)
	  (list-of-matrices nil)
	  (columns nil)
	  (rows nil))
      (loop :for line := (read-line stream nil) :while line :do
	(let ((c (if (zerop (first (array-dimensions line))) #\Space (aref line 0))))
	  (if (valid-number-first-char-p c)
	      (case parse-state
		(:top
		 (let ((numbers (parse-line-of-numbers line)))
		   (setf list-of-rows (list numbers))
		   (setf columns (length numbers))
		   (setf parse-state :in-matrix)))
		(:in-matrix
		 (setf list-of-rows (append list-of-rows (list (parse-line-of-numbers line)))))
		(:outside-matrix
		 (let ((numbers (parse-line-of-numbers line)))
		   (setf list-of-rows (list numbers))
		   (setf parse-state :in-matrix)))
		(otherwise (error "Invalid state ~a when interpreting a line of numbers." parse-state)))
	      (case parse-state
		(:in-matrix
		 (if (null rows)
		     (setf rows (length list-of-rows))
		     (when (not (= rows (length list-of-rows)))
		       (error "Matrices that are being read have different dimensions.")))
		 (let ((matrix (make-array `(,rows ,columns) :initial-contents list-of-rows)))
		   (setf list-of-matrices (cons matrix list-of-matrices))
		   (setf list-of-rows nil)
		   (setf depth (1+ depth)))
		 (setf parse-state :outside-matrix))
		(:top :continue)
		(:outside-matrix :continue)
		(otherwise (error "Invalid state ~a when interpreting a line of numbers." parse-state))))))
      (when (and (not (null list-of-rows)) (= (length list-of-rows) rows))
	(let ((matrix (make-array `(,rows ,columns) :initial-contents list-of-rows)))
	  (setf list-of-matrices (cons matrix list-of-matrices))))
      (reverse list-of-matrices))))

(defun main (filename)
  (let ((screen-width 800)
	(screen-height 800)
	(header-height 100))
    (with-window-initialized (window :title "matrix animate v0.0.1" :width screen-width :height screen-height)
      (with-font (font :fontpath "LiberationMono-Regular.ttf" :size 24)
	(with-renderer (renderer :window window)
	  (let* ((running t)
		 (millis-per-frame (/ 1000 60))
		 (matrices (make-voltage-matrices filename))
		 (matrix-depth (length matrices))
		 (matrix-dimensions (array-dimensions (first matrices)))
		 (matrix-height (first matrix-dimensions))
		 (matrix-width (second matrix-dimensions))
		 (current-depth 0)
		 (frame-step 1)
		 start-time-millis
		 end-time-millis)
	    (loop :while running :do
	      (setf start-time-millis (sdl2:get-ticks))
	      (sdl2:with-sdl-event (event)
		(loop :until (= (sdl2:next-event event :poll) 0) :do
		  (case (sdl2:get-event-type event)
		    (:keydown (let* ((keysym (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym))
				     (scancode (sdl2:scancode-value keysym)))
				(cond ((sdl2:scancode= scancode :scancode-escape) (setf running nil))
				      ((sdl2:scancode= scancode :scancode-q) (setf running nil))
				      ((sdl2:scancode= scancode :scancode-up)
					 (setf frame-step (1+ frame-step)))
				      ((sdl2:scancode= scancode :scancode-down)
				       (unless (= frame-step 1)
					 (setf frame-step (1- frame-step))))
				      ((sdl2:scancode= scancode :scancode-left)
				       (unless (< (- current-depth frame-step) 0)
					 (setf current-depth (- current-depth frame-step))))
				      ((sdl2:scancode= scancode :scancode-right)
				       (unless (>= (+ current-depth frame-step) matrix-depth)
					 (setf current-depth (+ current-depth frame-step)))))))
		    (:keyup t)
		    (:quit (setf running nil)))))
	      (sdl2:set-render-target renderer nil)
	      (sdl2:set-render-draw-color renderer 220 210 190 255)
	      (sdl2:render-clear renderer)
	      (let ((cell-width (/ screen-width matrix-width))
		    (cell-height (/ (- screen-height header-height) matrix-height))
		    (starting-y header-height))
		(loop :for row :from 0 :below matrix-height :do
		  (loop :for col :from 0 :below matrix-width :do
		    (draw-color-map-cell renderer
					 (ceiling (* col cell-width))
					 (ceiling (+ starting-y (* row cell-height)))
					 (ceiling cell-width)
					 (ceiling cell-height)
					 (aref (nth current-depth matrices) row col)))))
	      (let* ((msg (format nil "Frame: ~a/~a -- Frame step: ~a" (1+ current-depth) matrix-depth frame-step))
		     (text-surface (sdl2-ttf:render-text-solid font msg 80 80 80 255))
		     (text-texture (sdl2:create-texture-from-surface renderer text-surface))
		     (msg-width (sdl2:surface-width text-surface))
		     (msg-height (sdl2:surface-height text-surface)))
		(sdl2:render-copy renderer text-texture
				  :dest-rect (sdl2:make-rect 0 0 msg-width msg-height))
		(draw-legend renderer font 0 msg-height 800 50))
	      (sdl2:render-present renderer)
	      (setf end-time-millis (sdl2:get-ticks))
	      (let ((duration-millis (- start-time-millis end-time-millis)))
		(when (< duration-millis millis-per-frame)
		  (sdl2:delay (floor (- millis-per-frame duration-millis))))))))))))
