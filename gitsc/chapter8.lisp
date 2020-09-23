;;; recursion

(defun laugh (n)
  (cond ((= n 0) nil)
        (t (cons 'ha (laugh (- n 1))))))

(defun add-up (lst)
  (if (null lst)
      0
      (+ (car lst)
         (add-up (cdr lst)))))

(defun alloddp (lst)
  (cond ((null lst) t)
        ((evenp (car lst)) nil)
        (t (alloddp (cdr lst)))))

(defun rec-member (el lst)
  (cond ((null lst) nil)
        ((equal el (car lst)) lst)
        (t (rec-member el (cdr lst)))))

(defun rec-assoc (el table)
  (cond ((null table) nil)
        ((equal el (caar table)) (car table))
        (t (rec-assoc el (cdr table)))))

(defun rec-nth (n lst)
  (if (= n 0)
      lst
      (rec-nth (- n 1)
               (cdr lst))))

(defun rec-plus (x y)
  (if (= y 0)
      x
      (rec-plus (1+ x) (1- y))))

(defun fib (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(defun add-nums (n)
  (if (= n 0)
      0
      (+ n (add-nums (- n 1)))))

(defun all-equal (lst)
  (cond ((null (cdr lst)) t)
        ((equal (car lst) (cadr lst)) (all-equal (cdr lst)))
        (t nil)))

(defun count-down (n)
  (if (= n 0)
      nil
      (cons n (count-down (- n 1)))))

(defun square-list (lst)
  (if (null lst)
      nil
      (cons (* (car lst) (car lst))
            (square-list (cdr lst)))))

(defun my-nth (n lst)
  (cond ((zerop n) (car lst))
        ((null lst) nil)
        (t (my-nth (- n 1) (cdr lst)))))

(defun my-member (el lst)
  (cond ((null lst) nil)
        ((equal el (car lst)) lst)
        (t (my-member el (cdr lst)))))

(defun compare-lengths (x y)
  (cond ((and (null x) (null y)) 'same-length)
        ((null x) 'second-is-longer)
        ((null y) 'first-is-longer)
        (t (compare-lengths (cdr x)
                            (cdr y)))))

; assumes equal length
(defun pairings (list-a list-b)
  (cond ((null list-a) nil)
        (t (cons (list (car list-a)
                       (car list-b))
                 (pairings (cdr list-a)
                           (cdr list-b))))))

(defun sublists (lst)
  (cond ((null lst) nil)
        (t (cons lst
                 (sublists (cdr lst))))))

(defun reverse-helper (lst ret)
  (cond ((null lst) ret)
        (t (reverse-helper (cdr lst)
                           (cons (car lst)
                                 ret)))))

(defun my-reverse (lst)
  (reverse-helper lst nil))

(defun encapsulated-reverse (lst)
  (labels ((rev-helper (ret)
             (cond ((null lst) ret)
                   (t (reverse-helper (cdr lst)
                                      (cons (car lst)
                                            ret))))))
    (rev-helper nil)))

; debugger stuff
(defun debug-fact (n)
  (cond ((zerop n) (break "N is zero."))
        (t (* n (debug-fact (- n 1))))))
