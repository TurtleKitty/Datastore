#lang racket

(require (planet dvanhorn/fector:1:1))

;(provide create-datastore)

; create datastore
; delete datastore
; create relation name (fields)
; delete relation
; create tuple (x y z)
; read (x y) from relation where (u v)
; update (x y) in relation where (u v)
; delete from relation where (u v)

#|

(define (create-datastore (init #f))
    (define (build-store (rs #f) (is #f))
	(define relations (or rs (hash)))
	(define indexes (or is (hash)))
	(define (datastore cmd . args)
	    (case cmd
		('relations relations)
		('indexes indexes)
		('add_relation
		    (build-store
			(hash-set relations
			    (car args)
			    (create-relation (cadr args)))
			indexes))
		('add_tuples ())
	)))
    (if init
	(build-store (init 'relations) (init 'indexes))
	(build-store)))

|#

(define (list->fector lst)
    (apply fector lst))

(define (fector->list fv)
    (for/list ((i (in-range 0 (fector-length fv)))) 
	(fector-ref fv i)))

(define (fector->vector fv)
    (for/vector ((i (in-range 0 (fector-length fv)))) 
	(fector-ref fv i)))

(define (make-index fields)
    (for/list ((i (in-range (length fields))) (f (in-list fields)))
	(cons f i)))

(define (make-lookup index)
    (λ (f) (cdr (assoc f index))))

(define (tuple lookup seq)
    (define fvec
	(if (list? seq)
	    (list->fector seq)
	    seq))
    (define (get f)
	(fector-ref fvec (lookup f)))
    (define (put f val)
	(tuple lookup (fector-set fvec (lookup f) val)))
    (λ ((key #f) (val #f))
	(cond
	    (val (put key val))
	    (key (get key))
	    (else (fector->list fvec)))))

(define (tuple-factory fields)
    (define index (make-index fields))
    (define lookup (make-lookup index))
    (λ (fv) (tuple lookup fv)))

(define (relation fields (tuples empty))
    (define index (make-index fields))
    (define lookup (make-lookup index))
    (define (convert lss) (map list->fector lss))
    (define (get t f)
	(fector-ref t (lookup f)))
    (define (wrapper fv) (tuple lookup fv))
    (define (caller where)
	(λ (t) (let ((tup (wrapper t))) 
	    (where tup))))
    (define (mk-matcher where)
	(if (procedure? where)
	    (caller where)
	    (λ (t) (andmap
		(λ (x) (eq? (get t (car x)) (cadr x)))
		where))))
    (define (mk-relation ts)
	(define (create tups)
	    (mk-relation (append ts (convert tups))))
	(define (read where wanted)
	    (define is-match? (mk-matcher where))
	    (define t-fact (tuple-factory wanted))
	    (define (grab v)
		(t-fact
		    (apply fector
			(map (λ (f) (get v f)) wanted))))
	    (map grab (filter is-match? ts)))
	(define (read-row where xs) (car (read where xs)))
	(define (read-val where xs) ((read-row where xs) (car xs)))
	(define (update where put)
	    (define is-match? (mk-matcher where))
	    (define updater
		(if (procedure? put)
		    (caller put)
		    (λ (row)
			(for/list ((f (in-list fields)))
			    (let ((key (assoc f put)))
				(if key
				    (cadr key)
				    (get row f)))))))
	    (define (do-write row)
		(if (is-match? row)
		    (apply fector (updater row))
		    row))
	    (mk-relation (map do-write ts)))
	(define (delete where)
	    (define is-match? (mk-matcher where))
	    (mk-relation (filter-not is-match? ts)))
	(define (serialize)
	    (hash 'fields fields 'tuples (map fector->vector ts)))
	(define dispatch
	    (case-lambda
		(() (serialize))
		((cmd args)
		    (case cmd
			('create (create args))
			('delete (delete args))))
		((cmd where xs)
		    (case cmd
			('read (read where xs))
			('read-row (read-row where xs))
			('read-val (read-val where xs))
			('update (update where xs))))))
	dispatch)
    (mk-relation (convert tuples)))

(define fields '(x y z p q))
(define tf (tuple-factory fields))
(define xyzpq (tf '(1 1 2 3 5)))

(newline)

(xyzpq)
(xyzpq 'z)
((xyzpq 'q 8))

(newline)

(define ts
    '((1 1 2 3 5)
      (8 13 21 34 55)
      (0 0 2 1 0)  
      (1 1 1 1 1)
      (0 0 0 0 0)))

(define table (relation fields ts))

(define (unroll xs)
    (map (λ (tup) (tup)) xs))


(unroll (table 'read '((x 0) (q 0)) '(y z p)))
((table 'read-row '((p 3)) '(x y z)))
(table 'read-val '((z 21)) '(q))

(unroll
    (table 'read
	(λ (tup)
	    (let ((total (apply + (map tup fields))))
		(or (= (tup 'z) 21) (and (> total 4) (< total 16)))))
	'(x y z p q)))

(newline)

(table)

(define table2 (table 'create '((8 6 7 5 3) (0 9 8 6 7) (5 3 0 9 9))))

(table2)

(define table3 (table2 'update '((x 0)) '((y 3))))

(table3)

(define table4 (table3 'update
    (λ (t) (> (apply + (map t fields)) 10))
    (λ (t) (let ((xyz (map t '(x y z))))
	 (append xyz (list (apply + xyz) (apply * xyz)))))))

(table4)

(define table5 (table4 'delete (λ (t) (> (apply + (map t fields)) 100))))

(table5)

(define table6 (table5 'delete '((x 1))))

(table6)




