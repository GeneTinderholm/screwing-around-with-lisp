(defun fun-generator (win-condition)
  (lambda (other-player)
    (if (equal other-player win-condition) 'player-one-wins 'player-two-wins)))

(defun handle-rock (other-player)
  (funcall (fun-generator 'scissors) other-player))

(defun handle-paper (other-player)
  (funcall (fun-generator 'rock) other-player))

(defun handle-scissors (other-player)
  (funcall (fun-generator 'paper) other-player))

(defun referee (player-one player-two)
  (cond
    ((equal player-one player-two) 'tie)
    ((equal player-one 'rock) (handle-rock player-two))
    ((equal player-one 'paper) (handle-paper player-two))
    (t (handle-scissors player-two))))

(defun loopy (n)
  (loop for i from 0 to n
        collect i))
