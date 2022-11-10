; Lisp handles storage for numbers of any size.

; Also be aware of bit vectors
; >(setq x 10)
; 10
; >(setq y (read-from-string (format nil "#*~7,'0b" x)))
; #*0001010
; (setq z 8)
; 8
; >(setq y (read-from-string (format nil (concatenate 'string "#*~" (write-to-string z) ",'0b") x)))
; #*00001010
; There seems to be no easy way to print a bit vector in hexadecimal.
; (length y) -> 8
;
; (bit x 2) get a bit, left to right.
; (setf (bit x 2) 1) set a bit.
; (bit-ior x y)
; (bit-nor x y)
; (bit-and x y)
; (bit-nand x y)
; (bit-xor x y)
; (bit-eqv x y) bits that are the same.
; (bit-not x)

; Init Value global variables, but use the function value-set-num-bits as needed.
; Reader-eval decimal syntax, binary #b..., and hexadecimal #x..., can be used to specify a value.
(defvar *value-number-bits* 4  "Maximum number of right-shifted bits in a value")
(defvar *value-mask*        15     "A value with all bits set to one")
(defvar *value-msb*         #b1000 "A value with onlythe MSB set to one")
(defvar *value-not-msb*     #x7    "A value with all but the MSD set to one")

;;; Set the number of bits supported
(defun value-set-num-bits (num) ; -> nil
    (assert (integerp num))
    (assert (plusp num))

    (setf *value-number-bits* num)

    ; Set *value-mask*, 15 for 4 bits, as consecutive ones, never going over *value-number-bits* in the calculation.
    ; Don't trigger the use of a bignum if not required.
    (setf *value-mask* (1+ (* 2 (1- (expt 2 (- num 1))))))

    ; Set *value-msb*, 8 for 4 bits, never going over *value-number-bits* in the calculation.
    ; Don't trigger the use of a bignum if not required.
    (setf *value-msb* (expt 2 (- num 1)))

    ; Set mask of all bits except the msb.
    (setf *value-not-msb* (1- *value-msb*))

    (format t "~&Value number bits ~A all bits ~b msb ~b not msb 0~b~&" *value-number-bits* *value-mask* *value-msb* *value-not-msb*)
)

; Return true if an argument is a valid Value.
; The logand operator aborts if not given integers.
(defun value-p (val) ; -> bool
    (and (integerp val) (>= val 0) (<= val *value-mask*))
)

; Return true if a value is a subset (bits set to one) of another.
(defun value-subset (&key sub sup) ; -> bool
    (assert (value-p sub))
    (assert (value-p sup))

    (= (logand sub sup) sub)
)

; Return the bitwise "or" of two, or more, values.
; The logior operator accepts zero or one argument.
(defun value-or (&rest rest) ; -> value
    (assert (> (length rest) 1))
    (loop for valx in rest do (assert (value-p valx)))

    (apply 'logior rest)
)

; Return the bitwise "and" of two, or more, values. 
; The logand operator accepts zero or one argument.
(defun value-and (&rest rest) ; -> value (mask)
    (assert (> (length rest) 1))
    (loop for valx in rest do (assert (value-p valx)))

    (apply 'logand rest)
)

; Return the bitwise "xor" of two values
(defun value-xor (one two) ; -> value (mask)
  (assert (value-p one))
  (assert (value-p two))

  (logxor one two)
)

; Return the bitwise "not" of a value
(defun value-not (val) ; -> value (mask)
    (assert (value-p val))

    (logxor *value-mask* val)
)

; Return a value shifted left by 1
; The MSB is dropped.
; Never going over *value-number-bits* in the calculation, so don't trigger the use of a bignum if not required.
(defun value-shift-left (val) ; -> value
    (assert (value-p val))

    (ash (logand val *value-not-msb*) 1)
)

; Return a value shifted right by 1
; The LSB is dropped.
(defun value-shift-right (val) ; -> value
    (assert (value-p val))

    (ash val -1)
)

;;; Return a list of single-bit 1 values from a given value.
(defun value-split (val) ; -> list of values with only one bit, from input, set each
    (assert (value-p val))

    (let (ret (val2 val) val3 tmp)

        (loop while (not (zerop val2)) do
            (setf val3 (1- val2))
            (setf tmp (value-and (value-not val3) val2))
            (push tmp ret)
            (setf val2 (value-xor val2 tmp))
        ) ; end-loop
        ret
    )
)

;;; Return the most significant bit from a given value.
(defun value-msb (val)
    (assert (value-p val))

    (let (ret (val2 val) val3 tmp)

        (loop while (not (zerop val2)) do
            (setf val3 (1- val2))
            (setf tmp (value-and (value-not val3) val2))
            (setf ret tmp)
            (setf val2 (value-xor val2 tmp))
        ) ; end-loop
        ret
    )
)

;;; Return the least significant bit from a given value.
(defun value-lsb (val)
    (assert (value-p val))

    (if (zerop val) (return-from value-lsb nil))

    (let (val2)
        (setf val2 (1- val))
        (value-and (value-not val2) val)
    )
)

;;; Return true if a value has only one bit set to 1.
(defun value-one-bit (val) ; -> bool
    (assert (value-p val))

    (and (not (zerop val))
         (zerop (logand val (1- val))))
)

;;; Return a string representing a value in binary
(defun value-str-b (val) ; -> string
    (assert (value-p val))

    (format nil (concatenate 'string "0b~" (write-to-string *value-number-bits*) ",'0b") val)
)

;;; Return a string representing a value in hexadecimal
(defun value-str-x (val) ; -> string 
    (assert (value-p val))

    (let ((numx (floor (/ *value-number-bits* 4))))
        (if (> (mod *value-number-bits* 4) 0)
            (setf numx (1+ numx)))
        (format nil (concatenate 'string "0x~" (write-to-string numx) ",'0x") val)
    )
)

;;; Return the number of one bits in a value
(defun value-num-ones (val) ; -> integer
    (assert (value-p val))

    (logcount val)
)

;;; Return a value that is a mask of the same bits
(defun value-eqv (val1 val2) ; -> value (mask)
    (assert (value-p val1))
    (assert (value-p val2))

    (value-not (value-xor val1 val2))
)

;;; Run various tests.
(defun value-tests ()

    (let ((old-num-bits *value-number-bits*))

        (value-set-num-bits 4)
    
        (assert (not (value-p (1+ *value-mask*))))
        (assert (value-p *value-mask*))
        (assert (value-p 0))
        (assert (not (value-p -1)))
        (assert (= 3 (value-or  1 2)))
        (assert (= 6  (value-and 7 14)))
        (assert (= 5  (value-xor 3 6)))
        (assert (= 9  (value-not 6)))
        (assert (= 7  (value-shift-right 14)))
        (assert (= 14 (value-shift-left  7)))
        
        (assert (equal (value-split 13) '(8 4 1)))
    
        (assert (value-one-bit 4))
        (assert (value-one-bit 8))
        (assert (not (value-one-bit 0)))
        (assert (not (value-one-bit 6)))
    
    ;    (assert (= 2 (value-lsb 6)))
    ;    (assert (= 4 (value-msb 6)))
    
        (assert (= 2 (value-num-ones 6)))
        (assert (= 3 (value-eqv 9 5)))

        (value-set-num-bits old-num-bits)
    )
    (format t "~&value-tests OK")
    'OK
)

