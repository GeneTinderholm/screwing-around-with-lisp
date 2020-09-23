(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(defun alert (s)
  (setf (starship-shields s) 'up)
  (when (equal (starship-condition s) 'green)
    (setf (starship-condition s) 'yellow))
  'shields-raised)

(defun half (n)
  (/ n 2))

; discrimination set

(defstruct node
  name
  question
  yes-case
  no-case)

(defvar *node-list*)

(defun init () (setf *node-list* nil))

(defun add-node (name question yes-case no-case)
  (setf *node-list* (cons (make-node :name name
                                     :question question
                                     :yes-case yes-case
                                     :no-case no-case)
                          *node-list*))
  name)

(defun find-node (name)
  (find-if #'(lambda (el) (equal (node-name el) name))
           *node-list*))

(add-node 'start
          "Does the engine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)
(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)
(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")
(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-turn-key)
(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)
(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cables-good)

(defun process-node (name)
  (let ((node (find-node name)))
    (cond (node (format t "~A: " (node-question node))
                (let ((answer (read)))
                  (if (equal answer 'yes)
                      (node-yes-case node)
                      (node-no-case node))))
          (t (format t "Hmm... that's a puzzler~%")))))

(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((stringp current-node) (format t "~A~%" current-node))
    (when (null current-node)
      (return nil))))
