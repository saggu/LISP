;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	SUBMITTED BY - AMANDEEP SINGH
;	Input - (convert-to-CNF '(IF (AND A B C) (OR D E F G))) 
;	Output - sentences in CNF form 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun operator (l)  ;;;;returns the operator of the given input sentence
              (if (listp l)
                   (if (listp (car l))
                       (case (caar l)
                         (TRUE 'true)
                         (FALSE 'false)
                         (NOT 'not)
                         (OR 'or)
                         (AND 'and)
                         (IF 'if)
                         (t (caar l)))
                     (case (car l)
                       (TRUE 'true)
                       (FALSE 'false)
                       (NOT 'not)
                       (OR 'or)
                       (AND 'and)
                       (IF 'if)
                       (t (car l))))
                l))

(defun negation (l)  ;;;;negates the sentence
               (cond ( (null l) nil)
                     ( (atom l) (list 'not l))
                     (t (case (operator l)
                          (TRUE 'false)
                          (FALSE 'TRUE)
                          (NOT (if (listp (car l)) (list (cadar l)) (list (cadr l))))
                          (AND (cons 'or (cons (negation (cadr l)) (negation (cddr l)))))
                          (OR (cons 'and (cons (negation (cadr l)) (negation (cddr l)))))
                          (t (cons (negation (car l)) (negation (cdr l))))))))



(defun remove-imply (l) ;;;;remove any implications
              (cond ( (or  (atom l) (and (listp l) (null (cdr l))))  l)
                    (t (case (operator l)
                         (IF (cons 'or (list (negation (remove-imply (second l))) (remove-imply (third l)))))
                         (IFF (list 'and (cons 'or (list (negation (remove-imply (second l))) (remove-imply (third l)))) (cons 'or (list (negation (remove-imply (third l))) (remove-imply (second l))))))
                         (NOT (if (or (eq (operator (cdr l)) 'and) (eq (operator (cdr l)) 'or)) (negation (unlist (cdr l))) (cons (car l) (remove-imply (cdr l)))))
                         (t (cons (operator l) (cons (remove-imply (cadr l)) (remove-imply (unlist (cddr l))))))))))


(defun flatten (l) ;;;;;;;flattens the list removinf any unnecessary lists
  (cond ((null l) nil)
        ((or (atom (car l)) (member (operator l) '(not))) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))


(defun allors (l) ;;;checks if all operators in the sentence are ors
               (not (member 'and (flatten l))))

(defun allands (l) ;;;checks if all operators in the sentence are ands
               (not (member 'or (flatten l))))


(defun remove-or (l)  ;;;;;;;always pass the arguments to this function after flattening it, add and or or in front of the result for it to make sense
                 (cond ( (null l) nil)
                       ( (and (not (listp (car l))) (eq (car l) 'or)) (remove-or (cdr l)))
                       ( (and (listp (car l)) (not (eq (operator (car l)) 'or))) (cons (car l) (remove-or (cdr l))))
                       (t (cons (car l) (remove-or (cdr l))))))

(defun remove-and (l)  ;;;;;;;always pass the arguments to this function after flattening it, add and or or in front of the result for it to make sense
                 (cond ( (null l) nil)
                       ( (and (not (listp (car l))) (eq (car l) 'and)) (remove-and (cdr l)))
                       ( (and (listp (car l)) (not (eq (operator (car l)) 'and))) (cons (car l) (remove-and (cdr l))))
                       (t (cons (car l) (remove-and (cdr l))))))

(defun equality (lx ly) ;;;checks if two items are equal
               (cond ( (and (listp lx) (listp ly)) (equal lx (car ly)))
                     ( (and (listp lx) (not (listp ly))) (eq (car lx) ly))
                     ( (and (not (listp lx)) (listp ly)) (eq lx (car ly)))
                     ( t (eq lx ly))))


(defun cnf (l) ;;;main function, converts the sentence into cnf
                (cond ( (null l) nil)
                      ( (and (listp (car l)) (null (cdr l))) (cnf (car l)))
                      ( (isl l) l)
                      ( (allors l) (cons 'or (remove-or (flatten l))))
                      ( (allands l) (remove-and (flatten l)))
                      (t (case (operator l)
                           (OR (move-or-in (cnf (cadr l)) (cnf (cddr l))))
                           (AND (list (cnf (cadr l)) (cnf (cddr l))))
                           (NOT (negation (cdar l)))
                           (t l)))))


(defun convert-to-cnf (l) ;;entry point for the program
                (cond ( (allors (remove-imply l)) (cons 'or (remove-or (flatten (remove-imply l)))))
                      ( (allands (remove-imply l)) (cons 'or (remove-and (flatten (remove-imply l)))))
                      (t (cons 'and (cnf (remove-imply l))))))



(defun make-disjunction (lx ly) ;;;returns disjunction of input
               (cons 'or (cons lx (list ly))))

(defun move-or-inh (lx ly) ;;helper function for move-or-in
               (cond ( (null ly) nil)
                     ( (eq (operator ly) 'or) (move-or-inh lx (cdr ly)))
                     ( (equality lx ly) nil)
                     ( t (cons (make-disjunction lx (car ly)) (move-or-inh  lx (cdr ly))))))

(defun move-or-in (lx ly) ;;moves the ors inside
               (cond ( (null lx) nil)
                     ( (eq (operator lx) 'or) (move-or-in (cdr lx) ly))
                     ( t (append (move-or-inh (car lx)  ly) (move-or-in (cdr lx) ly)))))

(defun isl (l)
              (or (isa l) (and (isn l) (isa (arguments (cdr l))))))


(defun isa (l)
              (not (member (operator l) '(and or not if iff))))


(defun arguments (l)
              (cond ((null l) nil)
		    ((listp (car l)) (cons (car l) (arguments (cdr l))))
                    (t (cons (list (car l)) (arguments (cdr l))))))

(defun isn (l)
              (eq (operator l) 'not))

(defun unlist (l)
              (if (and (listp (car l)) (null (cdr l))) (car l) l))








