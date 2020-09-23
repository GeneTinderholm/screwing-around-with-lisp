; print string
(format t "Hello, World!")

; new line -----------V
(format t "Time flies~%like an arrow.")

; insert new line only if it is not at the beginning of a line
(format t "~&Fruit flies~%like a banana")
; same effect as a single ~&, will not insert extra new lines
(format t "~&~&Fruit flies~%like a banana")

; insert S-expression
(format t "From ~S to ~S in ~S minutes!" 'boston '(new york) 55)

; ~A vs ~S
(format t "~&With escape characters: ~S" "Hello, world")
(format t "~&Without escape characters: ~A" "Hello, world")

; 9.1
(defun new-line-stuff ()
  (format t "There are old pilots~%There are bold pilots~%But there are no old, bold pilots"))

(defun draw-line (n)
  (if (= n 0)
      (format t "~%")
      (progn
        (format t "*")
        (draw-line (- n 1)))))

(defun draw-box (n rows)
  (cond ((zerop rows) (format t "~%"))
        (t (draw-line n)
           (draw-box n (- rows 1)))))

(defun ninety-nine-bottles (n)
  (cond ((zerop n) (format t "Out of beer!~%"))
        (t (format t "~&~S bottles of beer on the wall,~%" n)
           (format t "~S bottles of beer!~%" n)
           (format t "Take one down,~%")
           (format t "Pass it around,~%")
           (format t "~%")
           (ninety-nine-bottles (- n 1)))))

(defun print-board (lst)
  (labels ((print-piece (x)
                       (if (null x)
                           " "
                           x)))
    (format t "~&~A | ~A | ~A~%"
            (print-piece (first lst))
            (print-piece (second lst))
            (print-piece (third lst)))
    (format t "~A | ~A | ~A~%"
            (print-piece (fourth lst))
            (print-piece (fifth lst))
            (print-piece (sixth lst)))
    (format t "~A | ~A | ~A~%"
            (print-piece (seventh lst))
            (print-piece (eighth lst))
            (print-piece (ninth lst)))))

(defun gross-pay ()
  (format t "~&Please enter the worker's hourly pay: ")
  (finish-output)
  (let ((pay (read)))
    (format t "~&Please enter the hours worked: ")
    (finish-output)
    (let ((hours (read)))
      (format t "~&Gross Pay: $~S" (* pay hours)))))

(defun cookie-monster ()
  (format t "~&Give me cookie!!!~%")
  (format t "Cookie? ")
  (let ((item (read)))
        (cond ((equal item 'cookie)
               (format t "Thank you!... Munch munch munch...BURP"))
        (t (format t "No want ~S...~%~%" item)
           (cookie-monster)))))

(defun read-temp-file ()
  (with-open-file (stream "./test.tmp")
    (read stream)))

(defun write-temp-file (sexp)
  (with-open-file (stream "./test.tmp" :direction :output :if-exists :append)
    (format stream "~S~%" sexp)))

; 9.10
(defun space-over (n)
  (cond ((< n 0) (format t "Error!"))
        ((= n 0) nil)
        (t (format t " ")
           (space-over (- n 1)))))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))

(defun plot-points (plotting-string y-lst)
  (mapcar #'(lambda (el) (plot-one-point plotting-string el)) y-lst))

(defun generate (m n)
  (cond ((= m n) (list m))
        (t (cons m (generate (+ 1 m) n)))))

(defun make-graph (func start end)
  (plot-points "." (mapcar func (generate start end))))

(defun square (n) (* n n))
