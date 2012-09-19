#lang racket

(require (planet dvanhorn/fector:1:1))

(provide create-datastore)


(define (create-datastore (rs #f) (is #f))
    (define relations (or rs (hash)))
    (define indexes (or is (hash)))
    (define (unroll-relations)
	(make-immutable-hash
	    (map (λ (k) (cons k ((hash-ref relations k))))
		 (hash-keys relations))))
    (define (serialize)
	(hash
	    'relations (unroll-relations)
	    'indexes indexes))
    (define (relvar name)
	(hash-ref relations name))
    (define (rebuild name noob)
	(create-datastore
	    (hash-set relations name noob) indexes))
    (define (add-relation name fields (tuples empty))
	(rebuild name (relation name fields tuples)))
    (define (add-tuples relname tupz)
	(rebuild relname ((relvar relname) 'create tupz)))
    (define (rm-tuples relname where)
	(rebuild relname ((relvar relname) 'delete where)))
    (define (update-tuples relname where put)
	(rebuild relname ((relvar relname) 'update where put)))
    (define (query result-type relname where want)
	((relvar relname) result-type where want))
    (case-lambda
	(() (serialize))
	((cmd)
	    (case cmd
		('relations (unroll-relations))
		('indexes indexes)))
	((cmd x)
	    (case cmd
		('get_relation (hash-ref relations x))))
	((cmd x y)
	    (case cmd
		('add_relation (add-relation x y))
		('create (add-tuples x y))
		('delete (rm-tuples x y))))
	((cmd x y z)
	    (case cmd
		('add_relation (add-relation x y z))
		((read read-row read-col read-val) (query cmd x y z))
		('update (update-tuples x y z))))))

(define (list->fector lst)
    (apply fector lst))

(define (fector->list fv)
    (for/list ((i (in-range 0 (fector-length fv)))) 
	(fector-ref fv i)))

(define (fector->vector fv)
    (for/vector ((i (in-range 0 (fector-length fv)))) 
	(fector-ref fv i)))

(define (make-lookup fields)
    (define index
	(for/list ((i (in-range (length fields))) (f (in-list fields)))
	    (cons f i)))
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
    (define lookup (make-lookup fields))
    (λ (fv) (tuple lookup fv)))

(define (relation name fields (tuples empty))
    (define lookup (make-lookup fields))
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
	(define (read-col where xs)
	    (define colname (car xs))
	    (for/list ((tup (in-list (read where xs))))
		(tup colname)))
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
	    (hash
		'name name
		'fields fields
		'tuples (map fector->vector ts)))
	(case-lambda
	    (() (serialize))
	    ((cmd)
		(case cmd
		    ('name name)
		    ('fields fields)))
	    ((cmd args)
		(case cmd
		    ('create (create args))
		    ('delete (delete args))))
	    ((cmd where xs)
		(case cmd
		    ('read (read where xs))
		    ('read-row (read-row where xs))
		    ('read-col (read-col where xs))
		    ('read-val (read-val where xs))
		    ('update (update where xs))))))
    (mk-relation (convert tuples)))

