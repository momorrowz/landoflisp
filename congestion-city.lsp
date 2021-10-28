(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15) ;;odds = probability

(defun random-node () ;node numberは1~*node-num*
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list) ;;nodeから伸びる全edge
  (remove-if-not
    (lambda (x)
      (eql (car x) node))
    edge-list))

(
)
(defun get-connected (node edge-list) ;;nodeから到達可能な全ノード
  (let
    ((visited nil))
    (labels 
      ((traverse (node) ;;traverse=巡回
        (unless (member node visited) ;;深さ優先探索
          (push node visited)
          (mapc 
            (lambda (edge)
              (traverse (cdr edge)))
            (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list) ;;島=nodesのまとまりを見つける
  (let 
    ((islands nil))
    (labels 
      ((find-island (nodes)
        (let* 
          ((connected (get-connected (car nodes) edge-list)) ;;先頭ノードを含む島を構成するnodes
           (unconnected (set-difference nodes connected)))
          (push connected islands)
          (when unconnected
            (find-island unconnected))))) ;;unconnectedで再帰呼び出し
      (find-island nodes))
    islands)) ;;nodeのリスト=島のリスト

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands)) (connect-with-bridges (cdr islands))))) ;;各islandsの代表=先頭nodeを繋げて、全部つなぐ.それに必要なedgeのリスト

(defun connect-all-islands (nodes edge-list) ;;全nodeをつなげるedgeを作成
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let*
    ((nodes (loop for i from 1 to *node-num* collect i))
     (edge-list (connect-all-islands nodes (make-edge-list))) ;;全nodeをつなげるedgeのリスト
     (cops (remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
     (add-cops (edges-to-alist edge-list) cops))) ;;道の一部にcopを配置

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1) (cons node1 (mapcar (lambda (edge) (list (cdr edge))) (remove-duplicates (direct-edges node1 edge-list) :test #'equal)))) (remove-duplicates (mapcar #'car edge-list))))
  ;;nodeとそこから出てるedgeの先のnodeの連想リスト((node1 (node2) (node3)) (node2 (node3) (node5)))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar
    (lambda (x)
      (let 
        ((node1 (car x))
        (node1-edges (cdr x))) ;;node1につながってるnode
        (cons node1 
          (mapcar 
            (lambda (edge)
              (let ((node2 (car edge)))
                (if (intersection (edge-pair node1 node2) edges-with-cops :test #'equal) ;;copいる?
                  (list node2 'cops)
                  edge)))
            node1-edges))))
      edge-alist))

(defun neighbors (node edge-alist) ;;nodeにつながったnode
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist) ;;bはaにつながってる?
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist) (some (lambda (x) (within-one x b edge-alist)) (neighbors a edge-alist)))) ;;a-b || a-x-bなるxが存在する

(defun make-city-nodes (edge-alist)
  (let
    ((wumpus (random-node)) ;;ワンプスいるnode
    (glow-worms (loop for i below *worm-num* collect (random-node)))) ;;ギャングいるnode
    (loop for n from 1 to *node-num* collect 
      (append 
        (list n)
        (cond 
          ((eql n wumpus) '(wumpus)) ;;wumpusいる -> wumpus
          ((within-two n wumpus edge-alist) '(blood!))) ;;wumpusの近くにいる -> blood
        (cond
          ((member n glow-worms) '(glow-worm)) ;;ギャングいる -> glow-worm
          ((some (lambda (worm) (within-one n worm edge-alist)) glow-worms) '(lights!))) ;;隣接nodeにギャングいる -> light
        (when (some #'cdr (cdr (assoc n edge-alist))) '(sirens!)))))) ;;隣接nodeにcopsいる -> siren

(defun find-empty-node ()
  (let
    ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes () ;;訪問済みのnodeとそこにつながってるnode
  (mapcar
    (lambda (node)
      (if (member node *visited-nodes*)
        (let ((n (assoc node *congestion-city-nodes*)))
          (if (eql node *player-pos*)
            (append n '(*))
            n))
        (list node '?)))
    (remove-duplicates (append *visited-nodes* (mapcan (lambda (node) (mapcar #'car (cdr (assoc node *congestion-city-edges*)))) *visited-nodes*))))) ;;mapcanまででvisited-nodesの一つ先のnodeのリスト

(defun known-city-edges () ;;visited-nodeにつながってるedges
  (mapcar 
    (lambda (node)
      (cons node 
        (mapcar 
          (lambda (x)
            (if (member (car x) *visited-nodes*)
              x
              (list (car x))))
          (cdr (assoc node *congestion-city-edges*)))))
    *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges)) ;;edgeのalist
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node)) ;;初期配置で敵のいるnodeにplayerがいないように
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-new-place (edge pos charging)
  (let*
    ((node (assoc pos *congestion-city-nodes*))
     (has-worm (and (member 'glow-worm node) (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond
      ((member 'cops edge) (princ "You ran into the cops. Game Over."))
      ((member 'wumpus node) (if charging (princ "You found the Wumpus!") (princ "You ran into the Wumpus.")))
      (charging (princ "You wasted your last bullet. Game Over."))
      (has-worm  ;;ギャングがいた場合はランダムな場所にジャンプ
        (let 
          ((new-pos (random-node)))
          (princ "You ran into a Glow Worm Gang! You're now at ")
          (princ new-pos)
          (handle-new-place nil new-pos nil))))))

(defun handle-direction (pos charging)
  (let 
    ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*))))) ;;player-posからposへのedge
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))