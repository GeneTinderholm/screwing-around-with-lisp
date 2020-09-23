;; Arrays

(defvar *hist-array*)
(defvar *total-points* 0)

(defun new-histogram (n)
  (setf *hist-array* (make-array n :initial-element 0)))

(defun record-value (n)
  (incf (aref *hist-array* n))
  (incf *total-points*))

(defun print-hist-line (n)
  (let ((times (aref *hist-array* n)))
    (format t "~2S [~3S] " n times)
    (dotimes (el times)
      (format t "*"))
    (format t "~%")))

(defun print-histogram (hist)
  (dotimes (row (length hist))
    (print-hist-line row)))

(defun generate-hist ()
  (dotimes (i 200)
    (record-value (random 11))))

;; Hash Tables

(defvar crypto-text
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfkwj"))

(defvar *encipher-table* (make-hash-table))
(defvar *decipher-table* (make-hash-table))

(defun reset-table (table)
  (clrhash table))

(defun reset-encipher-table () (reset-table *encipher-table*))
(defun reset-decipher-table () (reset-table *decipher-table*))

(defun clear ()
  (reset-encipher-table)
  (reset-decipher-table))

(defun make-substitution (a b)
  (setf (gethash a *decipher-table*) b)
  (setf (gethash b *encipher-table*) a))

(defun undo-substitution (letter)
  (let ((complement-of-letter (gethash letter *decipher-table*)))
    (setf (gethash letter *decipher-table*) nil)
    (setf (gethash complement-of-letter *encipher-table*) nil)))

(defun decipher-string (encoded-string)
  (let ((deciphered-string (make-string (length encoded-string)
                                        :initial-element #\Space)))
    (dotimes (i (length encoded-string) deciphered-string)
      (let ((result (gethash (elt encoded-string i) *decipher-table*)))
        (when result
          (setf (elt deciphered-string i) result))))))

(defun show-line (line)
  (format t "~A~%" line)
  (format t "~A~%" (decipher-string line)))

(defun show-text (cryptogram)
  (format t "===========================================================~%")
  (dolist (line cryptogram)
    (show-line line))
  (format t "===========================================================~%"))

(defun get-first-char (x)
  (char-downcase (char (format nil "~A" x) 0)))

(defun read-letter ()
  (let ((result (read)))
    (cond ((eq result 'end) 'end)
          ((eq result 'undo) 'undo)
          (t (get-first-char result)))))

(defun sub-letter (chr)
  (let ((result (gethash chr *decipher-table*)))
    (if result
        (format t "Character has already been substituted!~%")
        (progn (format t "What does the character decipher to? ")
               (let ((sub (read-letter)))
                 (make-substitution chr sub))))))

(defun undo-letter ()
  (format t "Undo which letter? ")
  (let* ((letter (read-letter))
         (existing-result (gethash letter *decipher-table*)))
    (if existing-result
        (undo-substitution letter)
        (format t "Letter has not been substituted~%"))))

(defun solve ()
  (show-text crypto-text)
  (format t "Substitute which letter? ")
  (let ((letter (read-letter)))
    (cond ((eq letter 'end) (return-from solve t))
          ((eq letter 'undo) (undo-letter))
          ((characterp letter) (sub-letter letter))
          (t (format t "Bad input!~%"))))
  (solve))
