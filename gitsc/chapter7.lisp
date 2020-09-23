;7.11
(defun gt-one-lt-five (lst)
  (remove-if-not #'(lambda (el) (< 1 el 5)) lst))

(defun count-the (lst)
  (length (remove-if-not #'(lambda (el) (equal el 'the)) lst)))

(defun length-two (lsts)
  (remove-if-not #'(lambda (lst) (= (length lst) 2)) lsts))

(defun create-set (lsts)
  (reduce #'union lsts))

(defun total-length (lists)
  (reduce #'(lambda (acc el)
              (+ acc (length el)))
          lists
          :initial-value 0))

(defun all-odd (lst)
  (every #'oddp lst))

(defun none-odd (lst)
  (every #'evenp lst))

(defun not-all-odd (lst)
  (not (all-odd lst)))

(defun not-none-odd (lst)
  (not (none-odd lst)))
