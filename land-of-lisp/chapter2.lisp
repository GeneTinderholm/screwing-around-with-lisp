(defvar *min* 0)
(defvar *max* 100)
(defvar *guess* 50)

(defun reset-game ()
  (setf *min* 0)
  (setf *max* 100)
  (setf *guess* 50))

(defun guess-my-number ()
  "Guesses number between 0 and 100"
  *guess*)
(defun smaller ()
  (setf *max* *guess*)
  (setf *guess* (round (/ (+ *guess* *min*) 2)))
  *guess*)

(defun bigger ()
  (setf *min* *guess*)
  (setf *guess* (round (/ (+ *guess* *max*) 2))))
