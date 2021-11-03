(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;c
(defun board-array (lst) ;trandform lst into array
  (make-array *board-hexnum* :initial-contents lst))

;d
(defun gen-board ()
  (board-array (loop for n below *board-hexnum* collect (list (random *num-players*) (1+ (random *max-dice*))))))

;c
(defun player-letter (n)
  (code-char (+ 97 n)))

;d
(defun draw-board (board)
  (loop for y below *board-size* do 
    (progn
      (fresh-line)
      (loop repeat (- *board-size* y) do (princ " "))
      (loop for x below *board-size* for hex = (aref board (+ x (* *board-size* y))) ;hex = 見てる六角形マス
        do (format t "~a-~a " (player-letter (first hex)) (second hex))))))

;c
(defun game-tree (board player spare-dice first-move) ;与えられた初期状態から可能な全盤面、指し手を表現する木構造を作成
  ;返り値は(player,board,可能な手とその後のtreeのリスト)
  ;引数は盤面の状態,手番のプレイヤー,獲得したサイコロ,最初のさしてかどうか
  (list player board (add-passing-move board 
                                       player
                                       spare-dice 
                                       first-move 
                                       (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves) ;player交代,movesは現在までに集められた可能な指し手
  (if first-move
    moves
    (cons (list nil (game-tree (add-new-dice board player (1- spare-dice)) (mod (1+ player) *num-players*) 0 t)) moves))) ;交代するか(nilが先頭のリスト),指し続けるか(moves)

(defun attacking-moves (board cur-player spare-dice) ;現在のプレイヤーが可能な手を全探索
  (labels
    ((player (pos) ;posのマスを持つplayer
      (car (aref board pos)))
     (dice (pos) ;posにあるサイコロ数
      (cadr (aref board pos))))
    (mapcan ;src -> dstへの全手
      (lambda (src)
        (when (eq (player src) cur-player)
          (mapcan
            (lambda (dst)
              (when (and (not (eq (player dst) cur-player)) (> (dice src) (dice dst)))
                (list (list (list src dst)
                            (game-tree (board-attack board cur-player src dst (dice src)) cur-player (+ spare-dice (dice dst)) nil))))) ;手の形は(((src dst) tree))のリスト
            (neighbors src))))
      (loop for n below *board-hexnum* collect n)))) ;全マス

(defun neighbors (pos) ;posに隣接するマス
  (let
    ((up (- pos *board-size*)) ;posの上
     (down (+ pos *board-size*))) ;posの下
    (loop for p in (append (list up down) ;上下
                           (unless (zerop (mod pos *board-size*)) ;左が壁じゃない?
                             (list (1- up) (1- pos))) ;左と左上
                           (unless (zerop (mod (1+ pos) *board-size*)) ;右が壁じゃない?
                             (list (1+ pos) (1+ down)))) ;右と右下
      when (and (>= p 0) (< p *board-hexnum*))
      collect p)))

(defun board-attack (board player src dst dice) ;return board after attack from src to dst
  (board-array (loop for pos from 0 ;pos: hexの位置
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex))))) ;それ以外そのまま

(defun add-new-dice (board player spare-dice)
  (labels
    ((f (lst n) ;lstにn個のdiceを補給した盤面情報を表すlstを返す
       (cond 
         ((null lst) nil)
         ((zerop n) lst)
         (t
            (let
              ((cur-player (caar lst))
               (cur-dice (cadar lst))) ;先頭マス
              (if (and (eq cur-player player) (< cur-dice *max-dice*))
                (cons (list cur-player (1+ cur-dice)) (f (cdr lst) (1- n))) ;補給できるマスには追加して再帰
                (cons (car lst) (f (cdr lst) n)))))))) ;補給できないの場合
      (board-array (f (coerce board 'list) spare-dice))))

(defun play-vs-human (tree) ;メインループ
  (print-info tree)
  (if (caddr tree)
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree) ;人間とのインターフェース
  (fresh-line)
  (princ "choose your move:")
  (let
    ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let
               ((action (car move))) ;(src,dst)
               (fresh-line)
               (format t "~a. " n)
               (if action
                 (format t "~a -> ~a" (car action) (cadr action))
                 (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves)))) ;手を指した後のtreeが返り値

(defun winners (board) ;boardから勝者(最多のマスを持ってる)を求める
  (let*
    ((tally (loop for hex across board collect (car hex)))
     (totals (mapcar (lambda (player) (cons player (count player tally))) (remove-duplicates tally))) ;(player,playerの持ってるマス数)のリスト
     (best (apply #'max (mapcar #'cdr totals)))) ;マス数の最大
    (mapcar #'car (remove-if (lambda (x) (not (eq (cdr x) best))) totals))))

(defun announce-winner (board)
  (fresh-line)
  (let
    ((w (winners board)))
    (if (> (length w) 1)
      (format t "The game is a tie betweeen ~a" (mapcar #'player-letter w))
      (format t "The winner is ~a" (player-letter (car w))))))

(defun rate-position (tree player) ;盤面の評価値
  (let
    ((moves (caddr tree))) ;可能な指し手があるか
    (if moves
      (apply 
        (if (eq (car tree) player) #'max #'min) ;minmax-method,playerなら指し手ののうちで最大の評価値の手をとる
        (get-ratings tree player))
      (let 
        ((w (winners (cadr tree))))
        (if (member player w) ;playerが勝者になってるか
          (/ 1 (length w));1/勝者数を点数に
          0)))))

(defun get-ratings (tree player) ;ゲーム木のすべての枝にrate-positionをmap
  (mapcar 
    (lambda (move) (rate-position (cadr move) player)) ;(cadr move)はtreeを返す
    (caddr tree)))

(defun handle-computer (tree)
  (let
    ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond
    ((null (caddr tree)) (announce-winner (cadr tree)))
    ((zerop (car tree)) (play-vs-computer (handle-human tree))) ;playerが0=humanなら
    (t (play-vs-computer (handle-computer tree))))) ;それ以外ならAIのターン