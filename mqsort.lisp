(in-package :mqsort)

(declaim (inline sort sv-sort swap select-pivot))

(defmacro muffle (exp)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,exp))

(defun swap (vec &key start1 start2 count)
  (loop REPEAT count
        FOR i OF-TYPE array-index FROM start1
        FOR j OF-TYPE array-index FROM start2
    DO
    (rotatef (aref vec i) (aref vec j))))

(defun select-pivot (vec beg end depth)
  (declare (ignorable vec beg end depth))
  ;;(let ((p (the array-index (+ beg (random (the (mod 100)#|XXX|# (- end beg)))))))
  ;;(rotatef (aref vec p) (aref vec beg))
  ;;beg))
  ;;beg)

  (flet ((code (i &aux (s (aref vec i)))
           (declare (most-efficient-string s))
           (if (>= depth (length s))
               -1
             (char-code (char s depth)))))
    (declare (inline code))
    (let ((p (let ((mid (+ beg (floor (- end beg) 2))))
               (let ((1c (code beg))
                     (2c (code mid))
                     (3c (code (1- end))))
                 (if (< 1c 2c)
                     (if (< 1c 3c) 
                         (if (< 2c 3c) mid (1- end))
                       beg)
                   (if (< 2c 3c) 
                       mid
                     (if (< 1c 3c) beg (1- end))))))))
      (rotatef (aref vec p) (aref vec beg))
      beg)))

(defun partition (vec beg end depth)
  (declare #.*fastest*
           (simple-vector vec)
           (array-index beg end depth))
  (flet ((code (i &aux (s (aref vec i)))
           (declare (most-efficient-string s))
           (if (>= depth (length s))
               -1
             (char-code (char s depth)))))
    (declare (inline code))
    (let* ((pivot (select-pivot vec beg end depth))
           (pivot-code (code pivot))
           (ls-front (1+ beg))
           (ls-last  (1+ beg))
           (gt-front (1- end))
           (gt-last  (1- end)))
      (declare (array-index ls-front ls-last gt-front gt-last))
      (loop
       (loop WHILE (<= ls-last gt-front)
             FOR code = (code ls-last)
             WHILE (<= code pivot-code)
         DO
         (when (= code pivot-code)
           (rotatef (aref vec ls-front) (aref vec ls-last))
           (incf ls-front))
         (incf ls-last))

       (loop WHILE (<= ls-last gt-front)
             FOR code = (code gt-front)
             WHILE (>= code pivot-code)
         DO
         (when (= code pivot-code)
           (rotatef (aref vec gt-front) (aref vec gt-last))
           (decf gt-last))
         (decf gt-front))
       
       (when (> ls-last gt-front)
         (return))
       (rotatef (aref vec ls-last) (aref vec gt-front))
       (incf ls-last)
       (decf gt-front))

      (let ((ls-beg ls-front)
            (ls-end ls-last)
            (gt-beg ls-last)
            (gt-end (1+ gt-last)))
        (let ((len (min (- ls-beg beg) (- ls-end ls-beg))))
          (swap vec :start1 beg :start2 (- ls-end len) :count len))
        (let ((len (min (- end gt-end) (- gt-end gt-beg))))
          (swap vec :start1 (- end len) :start2 gt-beg :count len))

        (values (the array-index (+ beg (- ls-end ls-beg)))
                (the array-index (- end (- gt-end gt-beg))))))))

(defun sv-sort-impl (vec beg end depth)
  (declare #.*fastest*
           (simple-vector vec)
           (array-index beg end depth))
  (if (<= (- end beg) 1)
      vec
    (multiple-value-bind (eql-beg eql-end) (partition vec beg end depth)
      (sv-sort-impl vec beg eql-beg depth)
      (when (< depth (length (the most-efficient-string (aref vec eql-beg))))
        (sv-sort-impl vec eql-beg eql-end (1+ depth)))
      (sv-sort-impl vec eql-end end depth))))

(defun sv-sort(vec)
  (declare #.*fastest*
           (simple-vector vec))
  (sv-sort-impl vec 0 (length vec) 0))

(defun sort (vector)
  (declare #.*interface*
           (vector vector))
  (etypecase vector
    (simple-vector (sv-sort vector))
    (vector        (muffle (sv-sort (coerce vector 'simple-vector))))))




