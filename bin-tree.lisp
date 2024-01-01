;; Binary trees

(defun make-bin-tree-leaf (E)
  (list E))
(defun make-bin-tree-node (E B1 B2)
  (list E B1 B2))

(defun bin-tree-leaf-element (L)
  (first L))
(defun bin-tree-node-element (N)
  (first N))
(defun bin-tree-node-left (N)
  (second N))
(defun bin-tree-node-right (N)
  (third N))

(defun bin-tree-leaf-p (B)
  (and (listp B) (= (list-length B) 1)))
(defun bin-tree-node-p (B)
  (and (listp B) (= (list-length B) 3)))

(defun make-empty-bst ()
  nil)
(defun bst-empty-p (B)
  (null B))

(defun bst-has-element (E B)
  (cond
    ((null B) nil)
    ((bin-tree-leaf-p B) (= E (bin-tree-leaf-element B)))
    ((bin-tree-node-p B)
      (let (
        (b-el (bin-tree-node-element B))
        (b-left (bin-tree-node-left B))
        (b-right (bin-tree-node-right B)))
        (cond
	      ((> E b-el) (bst-has-element E b-right))
	      ((< E b-el) (bst-has-element E b-left))
	      (t t))))))

(defun insert-bst-leaf (E B)
  (let
    ((b-el (bin-tree-leaf-element B)))
        (cond
          ((> E b-el) (make-bin-tree-node b-el nil (make-bin-tree-leaf E)))
          ((< E b-el) (make-bin-tree-node b-el (make-bin-tree-leaf E) nil))
	      (t B))))

(defun insert-bst-nonempty (E B)
  (cond
    ((null B) (make-bin-tree-leaf E))
    ((bin-tree-leaf-p B) (insert-bst-leaf E B))
    ((bin-tree-node-p B)
      (let (
        (b-el (bin-tree-node-element B))
        (b-left (bin-tree-node-left B))
        (b-right (bin-tree-node-right B)))
        (cond
	      ((> E b-el) (make-bin-tree-node b-el b-left (insert-bst-nonempty E b-right)))
	      ((< E b-el) (make-bin-tree-node b-el (insert-bst-nonempty E b-left) b-right))
	      (t B))))))

(defun insert-bst (E B)
  (if (bst-empty-p B)
    (make-bin-tree-leaf E)
    (insert-bst-nonempty E B)))

(defun list-to-tree (L B)
  (if (null L) B 
    (list-to-tree (rest L) (insert-bst (first L) B))))

(defun return-subtree (E B)
  (cond
    ((null B) nil)
    ((bin-tree-leaf-p B) (if (= E (bin-tree-leaf-element B)) B nil))
    ((bin-tree-node-p B)
      (let (
        (b-el (bin-tree-node-element B))
        (b-left (bin-tree-node-left B))
        (b-right (bin-tree-node-right B)))
        (cond
	      ((> E b-el) (return-subtree E b-right))
	      ((< E b-el) (return-subtree E b-left))
	      (t B))))))

(defun rightmost (B)
  (cond
    ((null B) nil)
    ((bin-tree-leaf-p B) B)
    ((bin-tree-node-p B) (rightmost (bin-tree-node-right B)))))

(defun remove-rightmost (B)
  (if (or (null B) (bin-tree-leaf-p B)) nil
    (let (
      (b-el (bin-tree-node-element B))
      (b-left (bin-tree-node-left B))
      (b-right (bin-tree-node-right B)))
      (cond
      ((null (bin-tree-node-right B)) b-left)
      (t (make-bin-tree-node b-el b-left (remove-rightmost b-right)))))))

(defun remove-from-subtree (B)
  (let (
    ;(b-el (bin-tree-node-element B))
    (b-left (bin-tree-node-left B))
    (b-right (bin-tree-node-right B)))
    (cond
      ((null b-left) b-right)
      ((null b-right) b-left)
      ((bin-tree-leaf-p b-left) (make-bin-tree-node (bin-tree-leaf-element b-left) nil b-right))
      (t (make-bin-tree-node (bin-tree-leaf-element (rightmost b-left)) (remove-rightmost b-left) b-right)))))

(defun remove-bst (E B)
  (cond
    ((null B) nil)
    ((bin-tree-leaf-p B) nil)
    ((bin-tree-node-p B)
      (let (
        (b-el (bin-tree-node-element B))
        (b-left (bin-tree-node-left B))
        (b-right (bin-tree-node-right B)))
        (cond
	      ((> E b-el) (make-bin-tree-node b-el b-left (remove-bst E b-right)))
	      ((< E b-el) (make-bin-tree-node b-el (remove-bst E b-left) b-right))
	      (t (remove-from-subtree B)))))))

(defun list-bst (B)
  (cond
  ((null B) nil)
  ((bin-tree-leaf-p B) (cons (bin-tree-leaf-element B) nil))
  ((bin-tree-node-p B) (append 
    (list-bst (bin-tree-node-left B))
    (cons (bin-tree-leaf-element B) nil) 
    (list-bst (bin-tree-node-right B))))))

(defun tree-sort (B)
(list-bst (list-to-tree B (make-empty-bst))))

(defun random-list (N)
(if (= 0 N) nil (cons (random 100) (random-list (- N 1)))))