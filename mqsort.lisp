(defpackage mqsort
  (:use :common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :mqsort)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 1) (debug 1)))
(deftype array-index () `(mod ,array-dimension-limit))
(deftype most-efficient-string () '(simple-array character))

(declaim (inline sort sv-sort partition set-pivot-at-front swap-range swap-if-greater))

(defmacro muffle (exp)
  `(locally
    (declare #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,exp))

(defmacro sref (vector index)
  `(the most-efficient-string (svref ,vector ,index)))

(defun swap-range (ary &key start1 start2 count)
  (loop REPEAT count
        FOR i OF-TYPE array-index FROM start1
        FOR j OF-TYPE array-index FROM start2
    DO
    (rotatef (sref ary i) (sref ary j))))

(defun swap-if-greater (ary x y depth)
  (when (string> (sref ary x) (sref ary y) :start1 depth :start2 depth)
    (rotatef (sref ary x) (sref ary y)))
  ary)

(defun set-pivot-at-front (ary beg end depth)
  (flet ((code (i &aux (s (sref ary i)))
           (if (>= depth (length s))
               -1
             (char-code (char s depth))))
         (set-pivot (pos)
           (rotatef (sref ary beg) (sref ary pos))))
    (declare (inline code set-pivot))
    (let* ((mid (+ beg (floor (- end beg) 2)))
           (las (1- end))
           (a (code beg))
           (b (code mid))
           (c (code las)))
      (if (< a b)
          (when (< a c)
            (if (< b c)
                (set-pivot mid)
              (set-pivot las)))
        (if (< b c)
            (set-pivot mid)
          (unless (< a c)
            (set-pivot las)))))))

(defun partition (ary beg end depth)
  (flet ((code (i &aux (s (sref ary i)))
           (if (>= depth (length s))
               -1
             (char-code (char s depth)))))
    (declare (inline code))
    (set-pivot-at-front ary beg end depth)
    (let* ((pivot (code beg))
           (ls-front (1+ beg))
           (ls-last  (1+ beg))
           (gt-front (1- end))
           (gt-last  (1- end)))
      (declare (array-index ls-front ls-last gt-front gt-last))
      (loop
       (loop WHILE (<= ls-last gt-front)
             FOR code = (code ls-last)
             WHILE (<= code pivot)
         DO
         (when (= code pivot)
           (rotatef (sref ary ls-front) (sref ary ls-last))
           (incf ls-front))
         (incf ls-last))

       (loop WHILE (<= ls-last gt-front)
             FOR code = (code gt-front)
             WHILE (>= code pivot)
         DO
         (when (= code pivot)
           (rotatef (sref ary gt-front) (sref ary gt-last))
           (decf gt-last))
         (decf gt-front))
       
       (when (> ls-last gt-front)
         (return))
       (rotatef (sref ary ls-last) (sref ary gt-front))
       (incf ls-last)
       (decf gt-front))

      (let ((ls-beg ls-front)
            (ls-end ls-last)
            (gt-beg ls-last)
            (gt-end (1+ gt-last)))
        (let ((len (min (- ls-beg beg) (- ls-end ls-beg))))
          (swap-range ary :start1 beg :start2 (- ls-end len) :count len))
        (let ((len (min (- end gt-end) (- gt-end gt-beg))))
          (swap-range ary :start1 gt-beg :start2 (- end len) :count len))

        (values (+ beg (- ls-end ls-beg))
                (- end (- gt-end gt-beg)))))))

(defun sv-sort-impl (ary beg end depth &aux (len (- end beg)))
  (declare #.*fastest*
           (array-index beg end depth))
  (if (<= len 2)
      (if (= len 2)
          (swap-if-greater ary beg (1+ beg) depth)
        ary)
    (multiple-value-bind (eql-beg eql-end) (partition ary beg end depth)
      (sv-sort-impl ary beg eql-beg depth)
      (when (< depth (length (sref ary eql-beg)))
        (sv-sort-impl ary eql-beg eql-end (1+ depth)))
      (sv-sort-impl ary eql-end end depth))))

(defun sv-sort(ary)
  (sv-sort-impl ary 0 (length ary) 0))

(defun sort (str-array)
  (declare #.*interface*
           (vector str-array))
  (etypecase str-array
    (simple-vector (sv-sort str-array))
    (vector        (muffle (sv-sort (coerce str-array 'simple-vector))))))
