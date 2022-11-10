;;; Return all combinations of one item from each list in a list.
;;; (any-1-of-each '(('a  'b) ('c) ('d 'e 'f))))
;;; Output will be 2 x 1 X 3 = 6 lists.
;;; -> (('A 'C 'D) ('A 'C 'E) ('A 'C 'F) ('B 'C 'D) ('B 'C 'E) ('B 'C 'F))
(defun any-1-of-each (alist)
  (assert (listp alist))
  (assert (> (length alist) 0))

  (let (ret tupx)

    (when (= 1 (length alist))
      (setf tupx (car alist))
      (assert (listp tupx))
      (assert (not (null tupx)))
      (return-from any-1-of-each (mapcar #'list tupx)))

    (setf tupx (car alist))
    (assert (listp tupx))
    (assert (not (null tupx)))

    (loop for itemx in tupx do
      (setq ret (append ret  (mapcar #'(lambda (y) (push itemx y)) (any-1-of-each (cdr alist)))))
    )

    ret
  )
)


