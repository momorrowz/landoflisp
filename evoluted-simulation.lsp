(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal)) ;to use (x,y) for key , make test-func equal
(defparameter *reproduction-energy* 200)

(defun random-plant (left top width height)
  (let
    ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*) ;; jungle内に一つ
  (random-plant 0 0 *width* *height*)) ;; 全領域内に一つ

(defstruct animal x y energy dir genes) ;;座標(x,y), 生存時間energy, 動く向きdir,遺伝情報(動物の向き情報)gene
(defparameter *animals* 
  (list (make-animal 
    :x (ash *width* -1)
    :y (ash *height* -1)
    :energy 1000
    :dir 0 ;0は左上
    :genes (loop repeat 8 collecting (1+ (random 10))))))

(defun move (animal) ;update position according to dir
  (let
    ((dir (animal-dir animal))
     (x (animal-x animal))
     (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x ;; 画面外に行ったら反対側から現れる
      (cond 
        ((and (>= dir 2) (< dir 5)) 1)
        ((or (= dir 1) (= dir 5)) 0)
        (t -1))) *width*))
    (setf (animal-y animal) (mod (+ y
      (cond
        ((and (>= dir 0) (< dir 3)) -1)
        ((and (>= dir 4) (< dir 7)) 1)
        (t 0))) *height*))
    (decf (animal-energy animal))))


(defun turn (animal) ;update dir according to genes
  (let 
    ((x (random (apply #'+ (animal-genes animal))))) ; reduce is better?
    (labels 
      ((angle (genes x) ;calculate next dir with genes & x(random value)
        (let ((xnu (- x (car genes))))
          (if (< xnu 0) 0 (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
        (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8)))))

(defun eat (animal)
  (let 
    ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*)))) ;;remove the plant eaten

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*) ; can reproduce?
      (setf (animal-energy animal) (ash e -1)) ;parent HP becomes half
      (let 
        ((animal-nu (copy-structure animal))
         (genes (copy-list (animal-genes animal)))
         (mutation (random 8))) ;index of genes mutated
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1))) ;+{-1 or 0 or 1}
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal) (<= (animal-energy animal) 0)) *animals*))
  (mapc 
    (lambda (animal)
      (turn animal)
      (move animal)
      (eat animal)
      (reproduce animal))
    *animals*)
  (add-plants))

(defun draw-world ()
  (loop for y below *height* do
    (progn 
      (fresh-line)
      (princ "|")
      (loop for x below *width* do
        (princ 
          (cond 
            ((some 
              (lambda (animal)
                (and (= (animal-x animal) x) (= (animal-y animal) y))) *animals*) #\M)
            ((gethash (cons x y) *plants*) #\*)
            (t #\space))))
      (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let
    ((str (read-line)))
    (cond 
      ((equal str "quit") ())
      (t 
        (let 
          ((x (parse-integer str :junk-allowed t)))
          (if x
            (loop for i below x do (update-world) if (zerop (mod i 1000)) do (princ #\.))
            (update-world)) ;to update for 1 day with [Enter]
          (evolution))))))