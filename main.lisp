;;;; Generate, and pring, all possible rules for a single state,
;;;; then all possible pairs of rules (same initial state, different results).
;;;;
;;;; Change the number of bit on line 24.

;;;; Assuming 4  bits:

;;;; 4 (values: 0->0, 0->1, 1->1, 1->0) ^ 4 (number positions) = 256 possible rules for a single 4-bit state.

;;;; Pairs of rules, with the same initial region, but different result regions.
;;;;
;;;; For any state, there will be 2 ^ 4 = 16 different possible results.
;;;;
;;;; Any 2 of 16 = 120 possible different, different-result, pairs.
;;;;
;;;; 120 * 16 states = 1920 pairs.

(load #p "value.lisp")
(load #p "region.lisp")
(load #p "rule.lisp")
(load #p "any1ofeach.lisp")
(load #p "anyxofn.lisp")

(value-set-num-bits 2)

(defun main () 

  (format t "~& ")

  (let (all-pos-combs all-pos-rules option-list strx ruly valid-rule-pairs)

      ; Load option numbers.
      (loop for i from 1 to *value-number-bits* do
	    (push '(1 2 4 8) option-list))

      (setf all-pos-combs (any-1-of-each option-list))

      (loop for rulx in all-pos-combs do
	(setf strx "")
	(loop for numx in rulx do
	  (if (string/= strx "")
	      (setf strx (concatenate 'string strx "/")))

	  (cond ((= 1 numx)
		 (setf strx (concatenate 'string strx "00")))
		((= 2 numx)
		 (setf strx (concatenate 'string strx "01")))
		((= 4 numx)
		 (setf strx (concatenate 'string strx "11")))
		((= 8 numx)
		 (setf strx (concatenate 'string strx "10")))
          )
	)

	(setf strx (concatenate 'string strx "]"))
	(setf strx (concatenate 'string "[" strx))

	(setf ruly (rule-from-str strx))
	(push ruly all-pos-rules)

	(format t "~&rule ~A" (rule-str ruly))
      )

     (format t "~& ~&All posible rules ~D" (length all-pos-rules))

    (format t "~& ")

    ; Find rule pairs with the same initial region.
    (setf rul-pairs (any-x-of-n 2 all-pos-rules))

    (setf valid-rule-pairs nil)
    (loop for pairx in rul-pairs do
      (when (region-eq (rule-initial-region (car pairx)) (rule-initial-region (second pairx)))
	; Shouldn't happen, since all rules should be different, but just to be paranoid...
        (when (not (region-eq (rule-result-region (car pairx)) (rule-result-region (second pairx))))
	  (push pairx valid-rule-pairs)
	  (format t "~& pair ~A - ~A" (rule-str (car pairx)) (rule-str (second pairx)))
        )
      )
    )
    (format t "~& ~&num valid pairs (same initial state, different result) ~D" (length valid-rule-pairs))
  )
  (format t "~& ~& ")
)

(main)

