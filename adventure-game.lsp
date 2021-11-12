(defparameter *nodes*
  '((living-room 
      (you are in the living-room. a wizard is snoring loudly on the couch.))
    (garden 
      (you are in a beautiful garden. there is a well in front of you.))
    (attic 
      (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges* 
  '((living-room 
      (garden west door)
      (attic upstairs ladder))
    (garden 
      (living-room east door))
    (attic 
      (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *objects-location* 
  '((whiskey living-room) 
    (bucket living-room) 
    (chain garden) 
    (frog garden)))
(defparameter *location* 'living-room)

(defun describe-location (loc nodes)
  (cadr (assoc loc nodes)))

(defun describe-path (edge)
  `(there is a , (caddr edge) going , (cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels 
    ((at-loc-p (obj) 
      (eq loc (cadr (assoc obj obj-locs)))))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels 
    ((desc-obj (obj)
      `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'desc-obj (objects-at loc objs obj-locs)))))

(defun look ()
  (append 
    (describe-location *location* *nodes*)
    (describe-paths *location* *edges*)
    (describe-objects *location* *objects* *objects-location*)))

(defun walk (direction)
  (let 
    ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
    (if next 
      (progn 
        (setf *location* (car next)) 
        (look))
      '(you cannot go that way.))))

(defun pickup (object)
  (cond 
    ((member object (objects-at *location* *objects* *objects-location*))
      (push (list object 'body) *objects-location*)
      `(you are now carrying the ,object))
    (t 
      '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *objects-location*)))
;attic - (ladder) - living-room - (door) - garden

(defun have (object)
  (member object (cdr (inventory))))

#| ;vscodeだとわからないけど複数行コメントアウト
(defun weld (subject object) ;you can weld chain & bucket in the attic
  (if 
    (and
      (eq *location* 'attic)
      (eq subject 'chain)
      (eq object 'bucket)
      (have 'chain)
      (have 'bucket)
      (not *chain-welded*))
    (progn 
      (setf *chain-welded* 't)
      '(the chain is now securely welded to the bucket.))
    '(you cannot weld like that.)))


(defun dunk (subject object)
  (if 
    (and
      (eq *location* 'garden)
      (eq subject 'bucket)
      (eq object 'well)
      (have 'bucket)
      *chain-welded*)
    (progn
      (setf *bucket-filled* 't)
      '(the bucket is now full of water))
    '(you cannot dunk like that.)))
|#

(defparameter *chain-welded* nil)
(defparameter *bucket-filled* nil)
(defmacro game-action (command subj obj place &body body) ; bodyにはprognで囲ったものを渡す
  `(progn
    (defun ,command (subject object)
      (if
        (and
          (eq *location* ',place)
          (eq subject ',subj)
          (eq object ',obj)
          (have ',subj))
        ,@body
        '(i cant ,command like that.)))
    (pushnew ',command *allowed-commands*)))

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
    (progn 
      (setf *chain-welded* 't)
      '(the chain is now securely welded to the bucket.))
    '(you do not have a bucket.)))

(game-action dunk bucket well garden
  (if *chain-welded*
    (progn
      (setf *bucket-filled* 't)
      '(the bucket is now full of water.))
    '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond 
    ((not *bucket-filled*) '(the bucket has nothing in it.))
    ((have 'frog) 
      '(the wizard awakens and sees that you stole his flog.
        he is so upset he banishes you to the netherworlds- you lose! the end.))
    (t 
      '(the wizard awakens from his slumber and greets you warmly.
        hehands you the magic low-carb donut- you win! the end.))))