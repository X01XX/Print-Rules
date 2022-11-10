
;;; Return a list of lists of any x items of a given list, order does not matter.
(defun any-x-of-n (x alist)
  (assert (> x 0))
  (assert (not (> x (length alist))))

  ; The end of the count down of x.
  ; Return a list of each item as a list.
  (when (= x 1)
    (return-from any-x-of-n (mapcar #'list alist))
  )

  ; So x > 1
  (let (itemx ret)
    ; For each item in the list, return that element
    ; pushed to each item returned by any-x-of-n (x-1) (rest of list).
    (setf x (1- x))
    (loop repeat (- (length alist) x) do ; length calculated only once
	  (setq itemx (car alist))
    	  (setq alist (cdr alist))
  	  (setq ret (append ret  (mapcar #'(lambda (y) (push itemx y)) (any-x-of-n x alist))))
    )
    ret
  )
)

