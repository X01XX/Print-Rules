;;;; Generate, and print, all possible rules for a single state,
;;;; then all possible pairs of rules (same initial state, different results).
;;;; Then all possible unions.
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
(load #p "rulelist.lisp")
(load #p "any1ofeach.lisp")
(load #p "anyxofn.lisp")

(value-set-num-bits 2)

(defun main () 

  (let (all-rules all-rule-pairs all-rule-unions all-rule-pair-unions)

      (format t "~& ~&Rules:")

      (setf all-rules nil)

      (let (all-pos-combs strx ruly option-list)

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
  	  (push ruly all-rules)
  
          (format t "~&    ~A" (rule-str ruly))
        )
     )
     (format t "~&Number rules = ~D" (length all-rules))

    ; Find rule pairs with the same initial region.
    (format t "~& ~&Rule pairs (same initial state, different results):")
    (setf all-rule-pairs nil)

    (let (rul-pairs)

      (setf rul-pairs (any-x-of-n 2 all-rules))

      (loop for pairx in rul-pairs do
        (when (region-eq (rule-initial-region (car pairx)) (rule-initial-region (second pairx)))
  	  (push pairx all-rule-pairs)
  	  (format t "~&    ~A" (rulelist-str pairx))
        )
      )
    )
    (format t "~&Number rule pairs = ~D" (length all-rule-pairs))
  
    ; Find all possible pairs of single rules
    (format t "~& ~&Rule unions:")
    (setf all-rule-unions nil)

    (let (rule-pairs tmp-rule)

      (setf rule-pairs (any-x-of-n 2 all-rules))

      (loop for rpx in rule-pairs do
	  (setf tmp-rule (rule-union (car rpx) (second rpx)))

	  (when (rule-valid-union-p tmp-rule)
	    (format t "~&    ~A u ~A = ~A" (rule-str (car rpx)) (rule-str (second rpx)) (rule-str tmp-rule))
	    (push tmp-rule all-rule-unions))
      )
    )
    (format t "~&Number rule unions = ~D" (length all-rule-unions))

    ; Find all possible pairs of rule pairs
    (format t "~& ~&Rule pair unions:")
    (setf all-rule-pair-unions nil)

    (let (pair0 pair1 pair-union rule2-pairs)

      (setf rule2-pairs (any-x-of-n 2 all-rule-pairs))

      (loop for rpx in rule2-pairs do

  	(setf pair0 (car rpx))
  	(setf pair1 (second rpx))
  
	(setf pair-union (rulelist-union pair0 pair1))

	(when pair-union
	  (format t "~&    ~A u ~A = ~A"
		  (rulelist-str pair0)
		  (rulelist-str pair1)
		  (rulelist-str pair-union))
	  (push pair-union all-rule-pair-unions)
	)
      )
    )
    (format t "~&Number rule pair unions = ~D" (length all-rule-pair-unions))
  )
  (format t "~& ~& ")
)

(main)

