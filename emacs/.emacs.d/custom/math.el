(defun write-ket (v)
  "Write v as a ket vector"
  (interactive "sEnter ket vector: ")
  (insert (mapconcat 'identity (list "|" v "\\rang") "")))

(defun write-bra (v)
  "Write v as a bra vector"
  (interactive "sEnter bra vector: ")
  (insert (mapconcat 'identity (list "\\lang" v "|") "")))
  
(defun ket (i n)
  "return ket vector of the i'th basis state in n dimensions"
  (loop for x below n
        collect (if (eq x i) '(1) '(0))))

(defun bra (i n)
  (list (loop for x below n
              collect (if (eq x i) 1 0))))

(defun hadamard (qb)
  "apply the Hadamard gate to a qubit"
  (let* ((rsqrt2 (/ 1 (sqrt 2)))
         (H `((,rsqrt2  ,rsqrt2)
              (,rsqrt2  ,(* -1 (sqrt 0.5))))))
    (mtimes H qb)))

(defun rows (m) (length m))
(defun cols (m) (length (car m)))

(defun mref (m i j) (nth j (nth i m)))

(defun zeros (dims)
  "returns a zeroed list structure with the given dimensions"
  (if (eq 1 (length dims))
      (loop for i below (car dims)
            collect 0)
    (loop for i below (car dims)
          collect (zeros (cdr dims)))))

(defun mref-set (m i j v)
  (setf (nth j (nth i m)) v))


(defun mtimes (A B)
  (let ((A-rows (rows A))
        (A-cols (cols A))
        (B-rows (rows B))
        (B-cols (cols B)))
    (if (not (eq A-cols B-rows))
        nil
      (let ((C (zeros (list A-rows B-cols))))
        (loop for i below A-rows do
              (loop for j below B-cols do
                    (let ((sum 0))
                      (loop for k below A-cols do
                            (incf sum (* (mref A i k)
                                         (mref B k j))))
                      (setf (mref C i j) sum))))
        C))))

;; Should write other matrix operations. add/sub.

(defun rotate-2d (coord theta)
  "theta should be in radians"
  (let ((R (list (list (cos theta) (* -1 (sin theta)))
                 (list (sin theta) (cos theta)))))
    (mtimes R coord)))

(defun deg2rad (theta-deg)
  "return radian value of degree angle"
  (* theta-deg (/ pi 180)))

(defun distance-2d (u v)
  "return 2d Euclidean distance between u and v"
  (let ((ux (car u)) (uy (cadr u))
        (vx (car v)) (vy (cadr v)))
    (sqrt (+ (expt (- vx ux) 2)
             (expt (- vy uy) 2)))))
