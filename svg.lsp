(defmacro let1 (var val &body body)
  `(let ((,var ,val)) ,@body))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x) x))

(defmacro split (val yes no)
  (let1 g (gensym) ;マクロ展開時に実行される
    `(let1 ,g ,val
      (if ,g
        (let 
          ((head (car ,g))
           (tail (cdr ,g)))
          ,yes)
        ,no))))

(defun pairs (lst)
  (labels 
    ((f (lst acc)
      (split lst
        (if tail
          (f (cdr tail) (cons (cons head (car tail)) acc))
          (reverse acc))
        (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels
      ((self ,(mapcar #'car p) ,@body)) 
      (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst acc 0)
    (split lst
      (self tail (1+ acc))
      acc)))

(defun print-tag (name alst closingp) ;alstは(key . value)のリスト.
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att) (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att))) alst)
  (princ #\>)
  (princ "")) ;to avoid printing the trush

(defmacro tag (name atts &body body) ;これでtagがネストできる.(tag mytag (key0 val0 key1 val1))
  `(progn 
    (print-tag ',name (list ,@(mapcar (lambda (x) `(cons ',(car x) ,(cdr x))) (pairs atts))) nil) 
    ;開き括弧.mapcarの前の@は括弧をはずしてる
    ,@body
    (print-tag ',name nil t)))

(defmacro html (&body body)
  `(tag html () ,@body))

(defmacro body (&body body)
  `(tag body () ,@body))

(defmacro svg (width height &body body)
  `(tag svg 
    (xmlns "http://www.w3.org/2000/svg"
     "xmlns:xlink" "http://www.w3.org/1999/xlink"
     height ,height
     width ,width)
    ,@body))

(defun brightness (col amt) ;明るさ調整
  (mapcar (lambda (x) (min 255 (max 0 (+ x amt)))) col))

(defun svg-style (color)
  (format nil "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
    (append color (brightness color -100))))

(defun circle (center radius color)
  (tag circle
    (cx (car center)
     cy (cdr center)
     r radius
     style (svg-style color))))

;(princ (svg 150 150 (circle '(70 . 50) 50 '(255 0 0)) (circle '(100 . 100) 50 '(0 0 255))))
(defun polygon (points color)
  (tag polygon 
    (points (format nil "~{~a,~a ~}" (mapcan (lambda (tp) (list (car tp) (cdr tp))) points))
    style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
      (random-walk
        (if (zerop (random 2))
          (- value 3)
          (+ value 3))
        (1- length)))))

(with-open-file (*standard-output* "random_walk.svg" :direction :output :if-exists :supersede)
  (svg 1500 500 
    (loop repeat 10
      do (polygon 
        (append 
          '((0 . 500)) 
          (loop for x from 0 for y in (random-walk 500 1500) collect (cons x y))
          '((1500 . 500)))
        (loop repeat 3 collect (random 256))))))