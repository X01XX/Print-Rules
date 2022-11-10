;;;; Implement a region as a cons of two values.

; Return a region from two states
(defun make-region (&key state0 state1)
  (assert (value-p state0))
  (assert (value-p state1))

  (cons state0 state1)
)

(defun region-new (state0 &optional state1)
  (if state1
      (make-region :state0 state0 :state1 state1)
      (make-region :state0 state0 :state1 state0))
)

; Return t if an argument is a region
(defun region-p (regx)
  (and
      (consp regx)
      (value-p (car regx))
      (value-p (cdr regx)))
)

; Return the first state of a region
(defun region-state0 (regx)
      (region-p regx)
      (car regx)
)

; Return the second state of a region
(defun region-state1 (regx)
      (region-p regx)
      (cdr regx)
)

;;; Return true if region contains a state
(defun region-contains-state (regx state)
    (assert (region-p regx))
    (assert (value-p state))

    (zerop (value-and (value-xor state (region-state0 regx))
                      (value-xor state (region-state1 regx))))
)

;;; Return the region X mask
(defun region-x-mask (regx)
    (assert (region-p regx))

    (value-xor (region-state0 regx)
               (region-state1 regx))
)

;;; Return the number of X bits
(defun region-num-x (regx) ; -> integer
    (assert (region-p regx))

    (value-num-ones (region-x-mask regx))
)

;;; Return the region non-x mask
(defun region-non-x-mask (regx)
    (assert (region-p regx))

    ;(value-not (region-x-mask regx))
    (value-eqv (region-state0 regx) (region-state1 regx))
)

;;; Return region one mask
(defun region-1-mask (regx)
    (assert (region-p regx))

    (value-and (region-state0 regx)
               (region-state1 regx))
)

;;; Return region zero mask
(defun region-0-mask (regx)
    (assert (region-p regx))

    (value-and (value-not (region-state0 regx))
               (value-not (region-state1 regx)))
)

;;; Return region highest state
(defun region-high-state (regx)
    (assert (region-p regx))

    (value-or (region-state0 regx)
              (region-state1 regx))
)

;;; Return region lowest state
(defun region-low-state (regx)
    (assert (region-p regx))

    (value-and (region-state0 regx)
               (region-state1 regx))
)

;;; Return a string representing a region
(defun region-str (regx)
    (assert (region-p regx))

    (let ((str "0r") (bitx *value-msb*))

        (loop while (plusp bitx) do
           (if (zerop (value-and bitx (region-state0 regx)))
               (progn
                   (if (zerop (value-and bitx (region-state1 regx)))
                       (setf str (concatenate 'string str "0"))
                       (setf str (concatenate 'string str "x")))
                  )
               (progn
                   (if (zerop (value-and bitx (region-state1 regx)))
                       (setf str (concatenate 'string str "X"))
                       (setf str (concatenate 'string str "1")))
                  )
           )
           (setf bitx (value-shift-right bitx))
        )

        ;(setf str (concatenate 'string str "]"))
        str
    )
)

;;; Return a region from a string.
;;; The string will be like: 0r10Xx
;;; The underscore character can be used to split up a region, like [1xx0_1010)
(defun region-from-str (strx)
    (assert (stringp strx))
    (assert (> (length strx) 1))

    (if (not (string-equal (subseq strx 0 2) "0r"))
        (return-from region-from-str nil))

    (let ((sta1 0) (sta2 0))
        (loop for chr across (subseq strx 2) do
            (cond ((char= chr #\_) nil)
                  (t (setf sta1 (value-shift-left sta1))
                     (setf sta2 (value-shift-left sta2))
                     (cond ((char= chr #\0) nil)
                        ((char= chr #\1) (incf sta1) (incf sta2))
                        ((char= chr #\x) (incf sta2))
                        ((char= chr #\X) (incf sta1))
                        (t (format t "~&invalid char ~S" chr) (assert nil))
                     ) ; end cond 2
                  ) ; end cond 1 t
            ) ; end cond 1
        ) ; end loop

       (return-from region-from-str (region-new sta1 sta2))
    ) ; end let
)

;;; Return a difference mask of two region.
(defun region-dif-mask (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (value-and
        (value-xor (region-high-state reg1) (region-high-state reg2))
        (value-xor (region-low-state  reg1) (region-low-state  reg2)))
)

;;; Return true if two regions intersect.
(defun region-intersects (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (zerop (region-dif-mask reg1 reg2))
)

;;; Return true if two regions are adjacent.
(defun region-adjacent (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (value-one-bit (region-dif-mask reg1 reg2))
)

;;; nil is returned if there is no intersection.
(defun region-intersection (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (if (not (region-intersects reg1 reg2)) (return-from region-intersection nil))

    (region-new (value-and (region-high-state reg1) (region-high-state reg2))
                (value-or  (region-low-state  reg1) (region-low-state  reg2)))
)

;;; Return true if two regions are equal
(defun region-eq (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (and
        (= (region-high-state reg1) (region-high-state reg2))
        (= (region-low-state  reg1) (region-low-state  reg2)))
)

;;; Return true if a region is a subset of another.
(defun region-subset (&key sub sup)
   (assert (region-p sub))
   (assert (region-p sup))

   (let ((sub-high-val (region-high-state sub))
         (sub-low-val  (value-not (region-low-state  sub)))
         (sup-high-val (region-high-state sup))
         (sup-low-val  (value-not (region-low-state  sup)))
        )
        (and
            (= sub-high-val (value-and sub-high-val sup-high-val))
            (= sub-low-val  (value-and sub-low-val sup-low-val)))
    )
)

;;; Return distance between two regions
(defun region-distance (reg1 reg2)
    (assert (region-p reg1))
    (assert (region-p reg2))

    (logcount (region-dif-mask reg1 reg2))
)

;;; Return the union of two regions
(defun region-union (regx regy)
    (assert (region-p regx))
    (assert (region-p regy))

    (region-new (value-or (region-high-state regx)
                          (region-high-state regy))
                (value-and (region-low-state regx)
                           (region-low-state regy)))
)


