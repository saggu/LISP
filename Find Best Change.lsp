;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	SUBMITTED BY - AMANDEEP SINGH
;	Input - (find-best-change 30 '( 25 10 1)) 
;	Output - a list of type (coins number) ( (a1 b1) (a2 b2 ... ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coin-sum (lst)   ;;;;returns sum of numbers at even positions in a list ;;critic ok
              (cond ( (null lst) 0)
                    ( (null (cdddr lst)) (car lst)) ;;;ignore the number at last odd position
                    ( t (+ (car lst) (coin-sum (cddr lst))))))

 (defun compare-lists (lstx lsty) ;;;; compares two lists for least number of coins used, if coins are equal then compares the remainder
                  (cond ( (and (null lstx) (null lsty)) nil)
                        ( (null lsty) lstx)
                        ( (= (coin-sum lstx) (coin-sum lsty)) ;;;;coin-sum equal -> compare remainder
                          (if (<= (car(reverse lstx)) (car(reverse lsty)))
                              lstx
                            lsty))
                        (t (if ( < (coin-sum lstx) (coin-sum lsty))
                               (if ( > (car (reverse lstx)) (car (reverse lsty)))
                                   lsty
                                 lstx) 
                             (if (< (car (reverse lstx)) (car (reverse lsty)))                                                                                                                                 
                                 lstx
                               lsty)))))

(defun rmvzero (lst) ;;;;removes any zero in the list ;;;critic ok
               (cond ( (null lst) nil)
                     ( (zerop (car lst)) (rmvzero (cdr lst)))
                     (t (cons (car lst) (rmvzero (cdr lst))))))

(defun ncoinlst ( x y lst)  ;;;;////return a list of (coin, number-of-coins) type 
                  (cond ( (and (null lst) ( < x y )) (cons x nil) )
                        ( (and (null lst) ( >= x y )) (cons (myabs x y) (cons y (cons (rem x y) nil))))
                        ( ( < x y ) (ncoinlst x (car lst) (cdr lst)))
                        ( t (cons (myabs x y) (cons y (ncoinlst ( rem x y) (car lst) (cdr lst)))))))


(defun myabs ( a b) ;;;returns absolute value of the operation x/y ;;critic ok
                   
                   (if ( < a b)
                       0
                     (1+ (myabs (- a b ) b ))))
	

(defun fbcs (x lst)
	(if (null lst)
		nil
	    (compare-lists (ncoinlst x (car lst) (cdr lst)) (fbcs x (cdr lst)))))
		


(defun format-list (cnlst lst);;;formats the list ;;;critic ok
                   (cond ( (and (null cnlst) (null lst)) nil)
			 ( (null (cdr cnlst))
                           (if (null lst)
                               nil
                             (cons (list 0 (car lst)) (format-list cnlst (cdr lst)))))
                         ( (= (cadr cnlst) (car lst))
                           (cons (list (car cnlst) (cadr cnlst)) (format-list (cddr cnlst) (cdr lst))))
                         (t (cons (list 0 (car lst))(format-list cnlst (cdr lst))))))


(defun find-best-change (x lst);;main calling function
                   (format-list (fbcs x (sort (rmvzero lst) #'>)) (sort (rmvzero lst) #'>)))