(in-package :cat.amatgil.tic_tac_toe)


(defconstant +board-width+ 3)
(defparameter *turn* :cross)

(defclass board ()
  ((espais 
    :initargs :espais
    ; Espai és :empty, :cross o :naught
    :initform (loop for i to (- (* +board-width+ +board-width+) 1) collecting :empty) ; 0 to 8 = 9 
    :accessor espais)))
  
(defun estat->char (estat)
  (ecase estat
   (:empty " ")
   (:cross "X")
   (:naught "O")
   (t (error "Estat imprevist"))))

(defun pos->idx (pos)
  (+ (first pos) (* (second pos) +board-width+)))

(defmethod print-board ((b board))
  (with-slots (espais) b
   (loop 
    for item in espais
    for i from 1 to (length espais)
    do (progn 
        (format t "|~a" (estat->char item))
        (when (zerop (mod i +board-width+)) (format t "|~%"))))))
    
(defmethod play-move ((b board) idx)
  (with-slots (espais) b
   (if (eql (elt espais idx) :empty)
    (progn 
     (setf (elt espais idx) *turn*)
     (switch-turn))
    (format t "Espai ja ocupat!~%"))))

(defmethod check-victory ((b board))
  (print "hi"))
  
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
(format t "Li toca a: ~w~%" (estat->char *turn*))

(loop
  do (let ((input (get-input)))
       ;(format t "[DEBUG]: S'ha triat: ~A~%" input)
       (if (not (idx-in-range-p input))
        (progn 
         (clear-term)
         (print-board *game-board*)
         (format t "La entrada ha de ser un número entra 1 i 9. Torna-ho a intentar~%"))
        (progn
         (clear-term)
         (play-move *game-board* input)
         (print-board *game-board*)
         (format t "Li toca a: ~w~%" (estat->char *turn*))))
       (when (check-victory *game-board*) 
        (progn
         (format t "La partida ha acabat! Han guanyat: les ~as!~%" (estat->char *turn*))
         (return)))))
         

