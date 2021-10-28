
(defun my-length 
    (list)
    (if list  
        (1+ 
            (my-length 
                (cdr list))) 0)
)
(defvar *arch-enemy* nil)
(defun pudding-eater 
    (person)
    (cond 
        (
            (eq person 'henry) 
            (setf *arch-enemy* 'stupid-lisp-alien) 
'
            (curse you lisp alien - you ate my pudding))
        (
            (eq person 'johnny)
            (setf *arch-enemy* 'useless-old-johnny)
'
            (i hope you choked on my pudding johnny))
        (t 
'
            (why you eat my pudding stranger?))))

(defun cake-eater
    (person)
    (case person
        (               
            (henry) 
            (setf *arch-enemy* 'stupid-lisp-alien) 
'
            (curse you lisp alien - you ate my cake))
        ( 
            (johnny) 
            (setf *arch-enemy* 'useless-old-johnny) 
'
            (i hope you choked on my cake johhny))
        (otherwise 
'
            (why you eat my cake stranger?))
))

(defun say-hello
    ()
    (print "say your name: ")
    (let 
        (
            (name 
                (read)))
        (princ "Nice to meet you, ")
        (princ name))
)

