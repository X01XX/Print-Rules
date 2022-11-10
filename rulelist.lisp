;;;; Implement a list for rule structs.

(defun rulelist-p (rullst)
  (or (null rullst) (rule-p (car rullst)))
)

;;; Return a string representing a rule list.
(defun rulelist-str (alist)
    (assert (rulelist-p alist))

    (let ((str "("))

        (loop for itemx in alist
              for count from 0 do

                 (if (plusp count)
                     (setf str (concatenate 'string str " ")))

                 (setf str (concatenate 'string str (rule-str itemx)))
        )
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Return the initial region of a rule list.
(defun rulelist-initial-region (rulsx)
    (assert (rulelist-p rulsx))

    (rule-initial-region (first rulsx))
)

;;; Return the union of two rule lists of the same pn value, or nil.
(defun rulelist-union (rulsx rulsy)
    (assert (rulelist-p rulsx))
    (assert (rulelist-p rulsy))

    (let ((lenx (length rulsx))
          (leny (length rulsy))
          rul-un1 rul-un2
          ruls-un1 ruls-un2
         )

        (if (/= lenx leny) (return-from rulelist-union nil))

        (if (= lenx 0) (return-from rulelist-union (list)))
 
        (when (= lenx 1)
            (setf rul-un1 (rule-union (first rulsx) (first rulsy))) 
            (if (rule-valid-union-p rul-un1)
                (return-from rulelist-union (list rul-un1)))
            (return-from rulelist-union nil)
        )

        (when (= lenx 2)
            (setf rul-un1 (rule-union (first rulsx) (first rulsy))) 
            (setf rul-un2 (rule-union (second rulsx) (second rulsy))) 
            (if (and (rule-valid-union-p rul-un1)
                     (rule-valid-union-p rul-un2))
                (setf ruls-un1 (list rul-un1 rul-un2)))

            (setf rul-un1 (rule-union (first rulsx) (second rulsy))) 
            (setf rul-un2 (rule-union (second rulsx) (first rulsy))) 
            (if (and (rule-valid-union-p rul-un1)
                     (rule-valid-union-p rul-un2))
                (setf ruls-un2 (list rul-un1 rul-un2)))

            (if (and (not (null ruls-un1)) (not (null ruls-un2)))
                (return-from rulelist-union nil))

            (if (not (null ruls-un1))
                (return-from rulelist-union ruls-un1))

            (if (not (null ruls-un2))
                (return-from rulelist-union ruls-un2))

            (return-from rulelist-union nil)
        )
        nil
    )
)

;;; Return a valid rulelist intersection, if any.
(defun rulelist-intersection (ruls1 ruls2)
    (assert (rulelist-p ruls1))
    (assert (rulelist-p ruls2))

    (when (not (= (length ruls1) (length ruls2)))
        (return-from rulelist-intersection nil))

    (let (rulint)
        (when (= (length ruls1) 1)
            (setf rulint (rule-intersection (first ruls1) (first ruls2)))
            (if (rule-valid-intersection-p rulint)
                (return-from rulelist-intersection (list rulint))
                (return-from rulelist-intersection nil))
        )
    )

    ; Rulelist length must be two
    (let (rulint1 rulint2)
     
        (setf rulint1 (rule-intersection (first  ruls1) (first  ruls2)))
        (setf rulint2 (rule-intersection (second ruls1) (second ruls2)))
        (if (and (rule-valid-intersection-p rulint1) (rule-valid-intersection-p rulint2))
            (if (region-eq (rule-initial-region rulint1) (rule-initial-region rulint2))
                (return-from rulelist-intersection (list rulint1 rulint2))
            )
        )

        (setf rulint1 (rule-intersection (first  ruls1) (second ruls2)))
        (setf rulint2 (rule-intersection (second ruls1) (first  ruls2)))
        (if (and (rule-valid-intersection-p rulint1) (rule-valid-intersection-p rulint2))
            (if (region-eq (rule-initial-region rulint1) (rule-initial-region rulint2))
                (return-from rulelist-intersection (list rulint1 rulint2))
            )
        )

        (return-from rulelist-intersection nil)
    )
)

;;; Return true if two rulelists are equal
(defun rulelist-eq (ruls1 ruls2)
    (assert (rulelist-p ruls1))
    (assert (rulelist-p ruls2))

    (when (not (= (length ruls1) (length ruls2)))
        (return-from rulelist-eq nil))

    (when (= (length ruls1) 1)
        (return-from rulelist-eq (rule-eq (first ruls1) (first ruls2)))
    )

    ; Rulelist length must be two
    (or (and (rule-eq (first  ruls1) (first  ruls2))
             (rule-eq (second ruls1) (second ruls2)))
        (and (rule-eq (first  ruls1) (second ruls2))
             (rule-eq (second ruls1) (first  ruls2))))
)

