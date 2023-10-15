(in-package :cat.amatgil.tic_tac_toe)


(defconstant +board-width+ 3)
(defparameter *turn* :cross)

(defclass board ()
  ((spaces 
    :initargs :spaces
    ; Espai és :empty, :cross o :naught
    :initform (loop for i to (- (* +board-width+ +board-width+) 1) collecting :empty) ; 0 to 8 = 9 
    :accessor :spaces)))
  
(defun state->char (state)
  (ecase state
   (:empty " ")
   (:cross "X")
   (:naught "O")
   (t (error "Unregistered state"))))

(defun pos->idx (pos)
  (+ (first pos) (* (second pos) +board-width+)))

(defmethod print-board ((b board))
  (with-slots (spaces) b
   (loop 
    for item in spaces
    for i from 1 to (length spaces)
    do (progn 
        (format t "|~a" (state->char item))
        (when (zerop (mod i +board-width+)) (format t "|~%"))))))
    
(defmethod play-move ((b board) idx)
  (with-slots (spaces) b
   (if (eql (elt spaces idx) :empty)
    (progn 
     (setf (elt spaces idx) *turn*)
     (switch-turn))
    (format t "Space already occupied!~%"))))

(defmacro check-rows (tiles)
  "Generates the code that checks each of the row's elements amongst themselves"
  (or
   (dotimes (i +board-width+) 
    (and
      (eql (elt tiles (+ 0 (* i +board-width+))) (elt tiles (+ 1 (* i +board-width+))))
      (eql (elt tiles (+ 1 (* i +board-width+))) (elt tiles (+ 2 (* i +board-width+))))
      (not (eql (elt tiles (* i +board-width+)) :empty))))))
          
;(macroexpand-1 `(or
;                 ,(dotimes (i +board-width+) 
;                   (and
;                    (eql (elt tiles (+ 0 (* i +board-width+))) (elt tiles (+ 1 (* i +board-width+))))
;                    (eql (elt tiles (+ 1 (* i +board-width+))) (elt tiles (+ 2 (* i +board-width+))))
;                    (not (eql (elt spaces (* i +board-width+)) :empty))))))
   
;(defmethod check-victory-m ((b board))
;  (with-slots (spaces) b
;   (or
;    (check-rows spaces))))
     
(defmethod check-victory ((b board))
  (with-slots (spaces) b
   (or
    (and
     (eql (elt spaces (+ 0 (* 0 +board-width+))) (elt spaces (+ 1 (* 0 +board-width+))))
     (eql (elt spaces (+ 1 (* 0 +board-width+))) (elt spaces (+ 2 (* 0 +board-width+))))
     (not (eql (elt spaces (* 0 +board-width+)) :empty)))
    (and
     (eql (elt spaces (+ 0 (* 1 +board-width+))) (elt spaces (+ 1 (* 1 +board-width+))))
     (eql (elt spaces (+ 1 (* 1 +board-width+))) (elt spaces (+ 2 (* 1 +board-width+))))
     (not (eql (elt spaces (* 1 +board-width+)) :empty)))
    (and
     (eql (elt spaces (+ 0 (* 2 +board-width+))) (elt spaces (+ 1 (* 2 +board-width+))))
     (eql (elt spaces (+ 1 (* 2 +board-width+))) (elt spaces (+ 2 (* 2 +board-width+))))
     (not (eql (elt spaces (* 2 +board-width+)) :empty))))))
  
(defun switch-turn ()
  (if (eql *turn* :cross)
   (setf *turn* :naught)
   (setf *turn* :cross)))
   
(defun idx-in-range-p (idx)
  (and (< idx (* +board-width+ +board-width+)) (>= idx 0)))

(defun get-input ()
  (1- (or (parse-integer (read-line) :junk-allowed t) 100000)))
  
(defun clear-term ()
  (format t "~c~%" #\Page))
  

;;; The "Main" starts here
(defparameter *game-board* (make-instance 'board))


(print-board *game-board*)
(format t "It is now ~w's turn~%" (state->char *turn*))
 

(loop
  do (let ((input (get-input)))
       ;(format t "[DEBUG]: S'ha triat: ~A~%" input)
       (if (not (idx-in-range-p input))
        (progn 
         (clear-term)
         (print-board *game-board*)
         (format t "Input must be a number between 1 i 9 inclusive. Try again~%"))
        (progn
         (clear-term)
         (play-move *game-board* input)
         (print-board *game-board*)
         (format t "It is now ~w's turn~%" (state->char *turn*))))
       (when (check-victory *game-board*) 
        (progn
         (switch-turn) ; we changed when we shouldn't've. change back
         (format t "Game over! The winner is: ~as!~%" (state->char *turn*))
         (return)))))
         

