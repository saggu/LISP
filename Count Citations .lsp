;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	SUBMITTED BY - AMANDEEP SINGH
;	Input - (count-citations n db)
;	Output - Score for the author
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-articles (n db)   ;;;;;critic ok
               (cond ( (null db) nil)
                     ( (member n (caddar db)) (cons (caar db) (get-articles n (cdr db))))
                     (t (get-articles n (cdr db)))))

(defun get-coauthors (n l)  ;;;;;critic ok
               (cond ( (null l) nil)
                     ( (eql n (car l)) (get-coauthors n (cdr l)))
                     (t (cons (car l) (get-coauthors n (cdr l))))))

(defun coauthors (n article db)  ;;;;;critic ok
               (cond ( (null db) nil)
                     ( (and ( is-member n (caddar db)) (eql (caar db) article)) (get-coauthors n (caddar db)))
                     (t (coauthors n article (cdr db)))))


(defun cited-by-ca (x y)  ;;;;;critic ok
               (cond ( (or (null x) (null y)) nil)
                     ( (eql (car x) (car y)) T)
                     (t (or (cited-by-ca (cdr x) y) (cited-by-ca x (cdr y))))))

(defun get-score (n ca article db)  ;;;;critique somewhat ok
               (cond ( (null db) 0)
                     ( (or (eql article (caar db)) (not (is-member article (car (cddddr (car db))))) (is-member n (caddar db))) (get-score n ca article (cdr db)))
                     ( (and (cited-by-ca ca (caddar db)) (>= (cadar db) 1995)) (1+ (get-score n ca article (cdr db))))
                     ( (and (cited-by-ca ca (caddar db)) (< (cadar db) 1995)) (+ 2 (get-score n ca article (cdr db))))
                     ( (and (not (cited-by-ca ca (caddar db))) (is-member article (car (cddddr (car db)))) (>= (cadar db) 1995)) (+ 6 (get-score n ca article (cdr db))))
                     (t (+ 4 (get-score n ca article (cdr db))))))

(defun accumulate-citations (n articles db)  ;;;critique ok
               (if (null articles)
                   0
                 (+ (get-score n (coauthors n (car articles) db) (car articles) db) (accumulate-citations n (cdr articles) db))))

(defun count-citations (n db)  ;;;;critique ok
               (accumulate-citations n (get-articles n db) db))

(defun is-member ( n l)  ;;critique ok
                (cond ( (null l) nil)
                      ( (eql n (car l)) T)
                      (t (is-member n (cdr l)))))

