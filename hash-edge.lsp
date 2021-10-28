(defun hash-edges (edge-list)  ;;list -> hash table ;edge-listは(node, nodeにつながったnodes)のリスト
  (let
    ((tab (make-hash-table))) ;;table?
    (mapc
      (lambda (x)
        (let
          ((node (car x)))
          (push (cdr x) (gethash node tab))))
      edge-list)
  tab))

(defun get-connected-hash (node edge-tab) ;;nodeから到達可能な全node
  (let
    ((visited (make-hash-table)))
    (labels 
      ((traverse (node) ;;traverse=巡回
        (unless (gethash node visited) ;;深さ優先探索
          (setf (gethash node visited) t)
          (mapc 
            (lambda (edge)
              (traverse edge))
            (gethash node edge-tab)))))
      (traverse node))
    visited))