(defun throw-die () (+ 1 (random 6)))
(defun throw-dice () (list (throw-die) (throw-die)))
(defun throw-total (roll) (+ (car roll) (cadr roll)))
(defun snake-eyes-p (roll)
  (if (and (= 1 (car roll)) (= 1 (cadr roll)))
      t
      nil))
(defun boxcars-p (roll)
  (if (and (= 6 (car roll)) (= 6 (cadr roll)))
      t
      nil))

(defun instant-win-p (roll)
  (let ((total (throw-total roll)))
    (if (or (= total 7) (= total 11))
        t
        nil)))

(defun instant-loss-p (roll)
  (let ((total (throw-total roll)))
    (cond
      ((= total 2) t)
      ((= total 3) t)
      ((= total 12) t))))

(defun say-throw (roll)
  (cond
    ((boxcars-p roll) 'boxcars)
    ((snake-eyes-p roll) 'snake-eyes)
    (t (throw-total roll))))

(defun craps ()
  (let ((roll (throw-dice)))
    (cond
      ((instant-loss-p roll)
       (list 'throw (car roll) 'and (cadr roll) '-- (say-throw roll) '-- 'you 'lose))
      ((instant-win-p roll)
       (list 'throw (car roll) 'and (cadr roll) '-- (say-throw roll) '-- 'you 'win))
      (t (list 'throw (car roll) 'and (cadr roll) '-- 'your 'point 'is (say-throw roll))))))

(defun try-for-point (point)
  (let* ((roll (throw-dice))
         (roll-total (throw-total roll)))
    (cond
      ((= roll-total 7)
       (list 'throw (car roll) 'and (cadr roll) '-- 7 '-- 'you 'lose))
      ((= roll-total point)
       (list 'throw (car roll) 'and (cadr roll) '-- point '-- 'you 'win))
      (t (list 'throw (car roll) 'and (cadr roll) '-- 'your 'point 'is roll-total)))))
