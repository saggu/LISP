;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	SUBMITTED BY - AMANDEEP SINGH
;	Input - a list of the form ( (n m) (( x y) (x2 y2) ... )) 
;	Output - a list of positions  ( (a1 b1) (a2 b2 ... ) depicting the coordinates of knights on the 
;		 grid such that no two knights attack each other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun mmx (x y)	;;;returns the whole x X y matrix;;;critic ok
                  (if (= x 0)
                      nil
                    (append (mm x y) (mmx (1- x) y))))

(defun mm( x y)  ;;;helper function for mmx  ;;critic ok
              (if (= y 0)
                  nil                
                (cons (list x y) (mm x (1- y)))))

(defun canplace (lstx lsty) ;;;;checks if lstx is in lsty (checks for available places for knights to be placed) ;;critic ok except for EQUAL
              (if (null lsty)
                  nil
                ( or (equal lstx (car lsty)) (canplace lstx (cdr lsty)))))


(defun removext (lstx lsty) ;;;;removes lstx from lsty and returns the rest (removext '(1 1) '(( 2 3) (1 1) ( 2 3)))
              (if (null lsty)
                  nil
                (if (equal lstx (car lsty))  
                    (cdr lsty)
                  (cons (car lsty) (removext lstx (cdr lsty))))))

(defun removelsts (lstx lsty);;;;;sample input (removelsts '((1 1) (2 3)) '((2 3) (1 1) (3 4)))  
                   (if (null lstx)
                       lsty
                     (removelsts (cdr lstx) (removext (car lstx) lsty))))

(defun pn ()   ;;;;;;returns the list of positions where, if placed, knights can attack the other knight
                   '((1 2) (1 -2) (-1 2) (-1 -2) (2 1) (2 -1) (-2 1) (-2 -1)))


(defun avpsn (x y pos lstpn)  ;;;;checks if all the contradicting positions for a input are in the search space, enter the ones which are
              (cond ( (null (car lstpn)) nil)
                    ( (or (<= (+ (car pos) (caar lstpn)) 0) (> (+ (car pos) (caar lstpn)) x) (<= (+ (cadr pos) (cadar lstpn)) 0) (> (+ (cadr pos) (cadar lstpn)) y))
                      (avpsn x y pos (cdr lstpn)))
                    (t (cons (list (+ (car pos) (caar lstpn)) (+ (cadr pos) (cadar lstpn))) (avpsn x y pos (cdr lstpn))))))	

(defun mvnxt (pos x y)  ;;;;returns the next available position in the search space
                   (cond ( (<= (+ (cadr pos) 2) y) (list (car pos) (+ (cadr pos) 2)))
			 ( ( and (<= (1+ (car pos)) x) (= (rem (1+ (car pos)) 2) 0)) (list (1+ (car pos)) 2))
                         ( ( and (<= (1+ (car pos)) x) (= (rem (1+ (car pos)) 2) 1)) (list (1+ (car pos)) 1))
                         (t nil)))


(defun pknights (pos sspace x y);;;;;;;;;;;places knights recursively until it runs out of search space, if search space is not null, return the available positions
              (cond ( (and (null pos) (not (null (car sspace)))) sspace)
                    ( (and (null pos) (null (car sspace))) nil) 
                    ( (canplace pos sspace) (cons pos (pknights(mvnxt pos x y) (removelsts (cons pos (avpsn x y pos (pn))) sspace) x y)))
                    (t (pknights (mvnxt pos x y) sspace x y))))

(defun place-knights (l) ;;;;main calling function
	(pknights '(1 1) (removelsts (cadr l) (mmx (caar l) (cadar l))) (caar l) (cadar l)))







