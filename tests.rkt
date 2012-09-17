#lang racket

(require "main.rkt")

; create datastore
; delete datastore
; create relation name (fields)
; delete relation
; create tuple (x y z)
; read (x y) from relation where (u v)
; update (x y) in relation where (u v)
; delete from relation where (u v)

(define world (create-datastore))

(world 'display)

(define world2
    (world 'add_relation
	'brawlers
	'(id name brawn brains moves cool karma)))

(world2 'display)

(define world3
    (world 'add_tuples
	'brawlers
	'(1 "Jack Damage" 20 0 2 10 100)
	'(2 "Dark Okie" 18 3 14 7 100)
	'(3 "Grey Hound" 10 0 17 0 100)
	'(4 "Billy Gunn" 0 3 4 0 100)
	'(5 "Doctor Braino" 0 10 0 10 100)))

(world3 'display)

(define world4
    (world 'update
	'brawlers
	'((brawn 21) (brains 1))
	'((id 1))))

(world4 'display)

(define brawlers
    (world 'get_relation 'brawlers))

(brawlers 'display)

(define jack
    (brawlers 'get_tuple '(('id 1)))

(jack 'display)

(define new-world (create-datastore (ds init)))


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

(define table (relation 'numbers fields ts))

(define (unroll xs)
    (map (λ (tup) (tup)) xs))

(table 'name)
(table 'fields)

(unroll (table 'read '((x 0) (q 0)) '(y z p)))
((table 'read-row '((p 3)) '(x y z)))
(table 'read-val '((z 21)) '(q))

(unroll
    (table 'read
	(λ (tup)
	    (let ((total (apply + (map tup (table 'fields)))))
		(or (= (tup 'z) 21) (and (> total 4) (< total 16)))))
	'(x y z p q)))

(newline)

(table)

(define table2 (table 'create '((8 6 7 5 3) (0 9 8 6 7) (5 3 0 9 9))))

(table2)

(define table3 (table2 'update '((x 0)) '((y 3))))

(table3)

(define table4 (table3 'update
    (λ (t) (> (apply + (map t (table3 'fields))) 10))
    (λ (t) (let ((xyz (map t '(x y z))))
	 (append xyz (list (apply + xyz) (apply * xyz)))))))

(table4)

(define table5 (table4 'delete (λ (t) (> (apply + (map t (table4 'fields))) 100))))

(table5)

(define table6 (table5 'delete '((x 1))))

(table6)






