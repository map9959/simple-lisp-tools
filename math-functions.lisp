(defun square (x) (* x x))

(defun cube (x) (* x x x))

(defun factorial (x)
  (if (= x 1) 1
    (* x (factorial (- x 1)))
  )
)

(defun triangular (x)
  (if (= x 1) 1
    (+ x (triangular (- x 1)))
  )
)

(defun power (x n)
  (if (= n 0) 1
    (* x (power x (- n 1)))
  )
)

(defun fibonacci (n)
  (if (or (= n 0) (= n 1)) 1
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
  )
)

(defun binom (n r)
  (if (or (= r 0) (= r n)) 1
    (+ (binom (- n 1) (- r 1)) (binom (- n 1) r))
  )
)

(defun sum (L)
  (if (null L) 0
    (+ (first L) (sum (rest L)))
  )
)

(defun lastlin (L)
  (if (null L) nil
    (if (null (rest L)) L
      (last (rest L)))))

(defun butlastlin (L)
  (if (null L) nil
    (if (null (rest L)) nil
      (cons (first L) (butlastlin (rest L))))))

(defun list-difference (L1 L2)
  (cond
    ((null L1) nil)
    ((member (first L1) L2)
     (list-difference (rest L1) L2))
    (t (cons (first L1) (list-difference (rest L1) L2)))))

(defun list-union (L1 L2)
  (cond
    ((null L1) L2)
    ((member (first L1) L2)
     (list-union (rest L1) L2))
    (t (cons (first L1) (list-union (rest L1) L2)))))
