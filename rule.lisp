;;;; Implement a rule vector as a list of four values.
;;;; Vector of four values representing b00 b01 b11 b10 

;;; Rule result options
(defvar *should-work* 3001)
(defvar *try-again*   3011)
(defvar *recalculate* 3019)

; Return a rule from four masks
(defun make-rule (&key b00 b01 b11 b10)
  (assert (value-p b00))
  (assert (value-p b01))
  (assert (value-p b11))
  (assert (value-p b10))

  (list b00 b01 b11 b10)
)

;;; Make a new rule from a region to a region.
;;; X->x bit changes are not expressable with this.
(defun rule-region-to-region (&key initial result)
    (assert (region-p initial))
    (assert (region-p result))

    (let ((result-x  (region-x-mask result))
          (initial-x (region-x-mask initial))
          (initial-zeros (region-0-mask initial))
          (initial-ones  (region-1-mask initial))
          (result2 result)
          initial-high result-high initial-not result-not
          not-xx
         )

         ; Change 0->X to 0->0
         (setf initial-zeros (value-and initial-zeros result-x))
         (if (not (zerop initial-zeros))
             (setf result2 (region-set-to-0 result2 initial-zeros)))

         ; Change 1->X to 1->1
         (setf initial-ones (value-and initial-ones result-x))
         (if (not (zerop initial-ones))
             (setf result2 (region-set-to-1 result2 initial-ones)))

        (setf initial-high (region-high-state initial))
        (setf initial-not  (value-not (region-low-state  initial)))
        (setf result-high  (region-high-state result2))
        (setf result-not   (value-not (region-low-state  result2)))

        (setf not-xx (value-not (value-and initial-x result-x)))

        (make-rule :b00 (value-and initial-not result-not)
                   :b01 (value-and initial-not result-high not-xx)
                   :b11 (value-and initial-high result-high)
                   :b10 (value-and initial-high result-not not-xx)
        )
    )
)

;;; Make a new rule from a state to state.
(defun rule-state-to-state (&key initial result)
    (assert (value-p initial))
    (assert (value-p result))

    (let ((initial-not (value-not initial))
          (result-not  (value-not result)))

        (let ((b00 (value-and initial-not result-not))
              (b01 (value-and initial-not result))
              (b11 (value-and initial result))
              (b10 (value-and initial result-not)))

                (make-rule :b00 b00 :b01 b01 :b11 b11 :b10 b10)
        )
    )
)

;;; Make a new rule going from a state to a region.
(defun rule-state-to-region (&key initial result)
    (assert (value-p  initial))
    (assert (region-p result))

    (let ((initial-high initial)
          (initial-not  (value-not initial))
          (result-high  (region-high-state result))
          (result-not   (value-not (region-low-state  result))))

        (let ((b00 (value-and initial-not result-not))
              (b01 (value-and initial-not result-high))
              (b11 (value-and initial-high result-high))
              (b10 (value-and initial-high result-not)))

                (make-rule :b00 b00 :b01 b01 :b11 b11 :b10 b10)
        )
    )
)

;;; Make a new rule going from a region to a state.
(defun rule-region-to-state (&key initial result)
    (assert (region-p initial))
    (assert (value-p  result))

    (let ((initial-high    (region-high-state initial))
          (initial-low-not (value-not (region-low-state initial)))
          (result-not      (value-not result)))

        (let ((b00 (value-and initial-low-not result-not))
              (b01 (value-and initial-low-not result))
              (b11 (value-and initial-high result))
              (b10 (value-and initial-high result-not)))

                (make-rule :b00 b00 :b01 b01 :b11 b11 :b10 b10)
        )
    )
)

;;; Return a rule from state or region, to state or region.
(defun rule-new (&key initial result)
    (assert (or (value-p initial) (region-p initial)))
    (assert (or (value-p result)  (region-p result)))

    (if (and (value-p initial) (value-p result))
        (return-from rule-new (rule-state-to-state :initial initial :result result)))

    (if (and (region-p initial) (region-p result))
        (return-from rule-new (rule-region-to-region :initial initial :result result)))

    (if (and (value-p initial) (region-p result))
        (return-from rule-new (rule-state-to-region :initial initial :result result)))

    (if (and (region-p initial) (value-p result))
        (return-from rule-new (rule-region-to-state :initial initial :result result)))
)

; Return t if an argument is a rule
(defun rule-p (rulx)
    (and
      (listp rulx)
      (not (null rulx))
      (= (length rulx) 4)
      (value-p (car rulx))
      (value-p (second rulx))
      (value-p (third rulx))
      (value-p (fourth rulx)))
)

; Return t if a number is a valid rule
(defun valid-rule-p (rulx)
  (assert (rule-p rulx))

  (and (rule-valid-intersection-p rulx)
       (rule-valid-union-p rulx))
)

(defun rule-b00 (rulx)
  (car rulx)
)

(defun rule-b01 (rulx)
  (second rulx)
)

(defun rule-b11 (rulx)
  (third rulx)
)

(defun rule-b10 (rulx)
  (fourth rulx)
)

; Return t if a number is a valid rule intersection
(defun rule-valid-intersection-p (rulx)
  (assert (rule-p rulx))

  (= *value-mask*
     (value-or (rule-b00 rulx) (rule-b01 rulx) (rule-b11 rulx) (rule-b10 rulx)))
)

; Return t if a number is a valid rule union
(defun rule-valid-union-p (rulx)
  (assert (rule-p rulx))

  (and (zerop (value-and (rule-b00 rulx) (rule-b01 rulx)))
       (zerop (value-and (rule-b11 rulx) (rule-b10 rulx))))
)

;;; Return a string representation of a rule
(defun rule-str (rulx)
    (assert (rule-p rulx))

    (let (
          (strs "[")
          (b00 (rule-b00 rulx))
          (b01 (rule-b01 rulx))
          (b11 (rule-b11 rulx))
          (b10 (rule-b10 rulx))
          bitval
          (bit-pos *value-msb*)
          (not-start nil)
         )

         (loop while (plusp bit-pos) do
             (if (zerop (value-and bit-pos b00))
                 (setf bitval 0)
                 (setf bitval 1))
             (if (plusp (value-and bit-pos b01))
                 (setf bitval (+ bitval 2)))
             (if (plusp (value-and bit-pos b11))
                 (setf bitval (+ bitval 4)))
             (if (plusp (value-and bit-pos b10))
                 (setf bitval (+ bitval 8)))

             (if not-start (setf strs (concatenate 'string strs "/")) (setf not-start t))

             (cond ((= bitval  1) (setf strs (concatenate 'string strs "00")))
                   ((= bitval  2) (setf strs (concatenate 'string strs "01")))
                   ((= bitval  3) (setf strs (concatenate 'string strs "0?")))
                   ((= bitval  4) (setf strs (concatenate 'string strs "11")))
                   ((= bitval  5) (setf strs (concatenate 'string strs "XX")))
                   ((= bitval  6) (setf strs (concatenate 'string strs "X1")))
                   ((= bitval  7) (setf strs (concatenate 'string strs "0?11")))
                   ((= bitval  8) (setf strs (concatenate 'string strs "10")))
                   ((= bitval  9) (setf strs (concatenate 'string strs "X0")))
                   ((= bitval 10) (setf strs (concatenate 'string strs "Xx")))
                   ((= bitval 11) (setf strs (concatenate 'string strs "0?10")))
                   ((= bitval 12) (setf strs (concatenate 'string strs "1?")))
                   ((= bitval 13) (setf strs (concatenate 'string strs "1?00")))
                   ((= bitval 14) (setf strs (concatenate 'string strs "1?01")))
                   ((= bitval 15) (setf strs (concatenate 'string strs "1?0?")))
                   (t (setf strs (concatenate 'string strs "..")))
             )
             (setf bit-pos (value-shift-right bit-pos))
         ) ; end-while

    (setf strs (concatenate 'string strs "]"))
    strs
    )
)

;;; Return a rule from a token, like "[00/01/11/10/x0/X0/x1/X1/XX/xx/Xx/xX]"
;;; Do not end a string with the slash character since it shifts the bits to the left.
;;; Keying a bit-position change on the slash character allows testing the output of
;;; disallowed combinations.
;;; Undeclared bit positions to the left, due to more positions in the Value-mask, become 0->0.
(defun rule-from-str (strx)
    (assert (stringp strx))
    (assert (> (length strx) 2))

    (if (not (string-equal (subseq strx 0 1) "["))
        (return-from rule-from-str nil))

    (let ((b00 0) (b01 0) (b11 0) (b10 0) (bit-i nil))
        (loop for chr across (subseq strx 1) do
            (cond ((char= chr #\/)
                     (setf b00 (value-shift-left b00))
                     (setf b01 (value-shift-left b01))
                     (setf b11 (value-shift-left b11))
                     (setf b10 (value-shift-left b10))
                     (setf bit-i nil))
                  ((char= chr #\])
                     (return-from rule-from-str
                                      (make-rule :b00 (value-or b00 (value-not (value-or b01 b11 b10)))
                                                 :b01 b01 :b11 b11 :b10 b10))
                   )
                  ((null bit-i) (setf bit-i chr))
                  (t
                     ;;(format t "~&~S ~S" bit-i chr)
                     (cond ((and (char= bit-i #\0) (char= chr #\0))
                            (incf b00))
                           ((and (char= bit-i #\0) (char= chr #\1))
                            (incf b01))
                           ((and (char= bit-i #\1) (char= chr #\1))
                            (incf b11))
                           ((and (char= bit-i #\1) (char= chr #\0))
                            (incf b10))
                           ((and (char= bit-i #\x) (char= chr #\x))
                            (incf b00)
                            (incf b11))
                           ((and (char= bit-i #\X) (char= chr #\X))
                            (incf b00)
                            (incf b11))
                           ((and (char= bit-i #\X) (char= chr #\x))
                            (incf b01)
                            (incf b10))
                           ((and (char= bit-i #\x) (char= chr #\X))
                            (incf b01)
                            (incf b10))
                           ((and (char= bit-i #\x) (char= chr #\0))
                            (incf b00)
                            (incf b10))
                           ((and (char= bit-i #\X) (char= chr #\0))
                            (incf b00)
                            (incf b10))
                           ((and (char= bit-i #\x) (char= chr #\1))
                            (incf b11)
                            (incf b01))
                           ((and (char= bit-i #\X) (char= chr #\1))
                            (incf b11)
                            (incf b01))
                           ((and (char= bit-i #\0) (or (char= chr #\X) (char= #\x)))
                            (incf b00)
                            (incf b01))
                           ((and (char= bit-i #\1) (or (char= chr #\X) (char= #\x)))
                            (incf b11)
                            (incf b10))
                           (t (error (format t "~&invalid char ~S" chr)))
                     ) ;; end cond 2
                     (setf bit-i nil)
                  ) ;; end cond 1 t
            ) ;; end cond 1
        ) ;; end loop
        ;;(format t "~& b00 ~D b01 ~D b11 ~D b10 ~D" b00 b01 b11 b10)
        (format t "~&missing closing bracket? ~A" strx)
        nil
    ) ;; end let
)

;;; Return a rule-union.
;;; It may be invalid, or partially invalid, due to 1->X or 0->X positions.
;;; Caller to check result, with rule-valid-union-p and possibly rule-partial-valid-union.
(defun rule-union (rulx ruly)
    (assert (rule-p rulx))
    (assert (rule-p ruly))

    (make-rule :b00 (value-or (rule-b00 rulx) (rule-b00 ruly))
               :b01 (value-or (rule-b01 rulx) (rule-b01 ruly))
               :b11 (value-or (rule-b11 rulx) (rule-b11 ruly))
               :b10 (value-or (rule-b10 rulx) (rule-b10 ruly)))
)

;;; Shift all bits of a rule left one bit.  The
;;; resule will be an invalid rule, but may be combined 
(defun rule-shift-left (rulx)
    (assert (rule-p rulx))

    (make-rule :b00 (value-shift-left (rule-b00 rulx))
               :b01 (value-shift-left (rule-b01 rulx))
               :b11 (value-shift-left (rule-b11 rulx))
               :b10 (value-shift-left (rule-b10 rulx)))
)

;;; Return a rule-intersection.
;;; It may be invalid, due to position with no mask set to 1.
;;; Caller to check result, using rule-valid-intersection-p and possibly rule-initial-region.
(defun rule-intersection (rulx ruly)
    (assert (rule-p rulx))
    (assert (rule-p ruly))

    (make-rule :b00 (value-and (rule-b00 rulx) (rule-b00 ruly))
               :b01 (value-and (rule-b01 rulx) (rule-b01 ruly))
               :b11 (value-and (rule-b11 rulx) (rule-b11 ruly))
               :b10 (value-and (rule-b10 rulx) (rule-b10 ruly)))
)

;;; Return T if two rules are equal
(defun rule-eq (rul1 rul2)
    (assert (rule-p rul1))
    (assert (rule-p rul2))

    (and (= (rule-b00 rul1) (rule-b00 rul2))
         (= (rule-b01 rul1) (rule-b01 rul2))
         (= (rule-b11 rul1) (rule-b11 rul2))
         (= (rule-b10 rul1) (rule-b10 rul2)))
)

;;; Return the initial region of a rule.
(defun rule-initial-region (arul)
    (assert (rule-p arul))

    (region-new (value-or (rule-b11 arul) (rule-b10 arul))
                (value-not (value-or (rule-b00 arul) (rule-b01 arul))))
)

;;; Return the result region of a rule.
(defun rule-result-region (arul)
    (assert (rule-p arul))

    (region-new (value-or (rule-b11 arul) (rule-b01 arul))
                (value-not (value-or (rule-b00 arul) (rule-b10 arul))))
)

