#lang racket

(require "main.rkt")


(define world (create-datastore))

(world)

(define bfields '(id name brawn brains moves cool karma))

(define world2
    (world 'add_relation 'brawlers bfields))

(world2)

(define world3
    (world2 'create 'brawlers
	'((1 "Jack Damage" 20 0 2 10 100)
	  (2 "Dark Okie" 18 3 14 7 100)
	  (3 "Grey Hound" 10 0 17 0 100)
	  (4 "Billy Gunn" 0 3 4 0 100)
	  (5 "Doctor Braino" 0 10 0 10 100))))

(world3)

(define world4
    (world3 'update 'brawlers
	'((id 1))
	'((brawn 21) (brains 1))))

(world4)

(define brawlers
    (world4 'get_relation 'brawlers))

(brawlers)

(define jack (car (brawlers 'read '((id 1)) '(name brawn brains moves cool karma))))

(displayln (jack 'cool))

(define (unroll xs)
    (map (λ (tup) (tup)) xs))

(unroll
    (world4 'read 'brawlers
	(λ (tup) (> (tup 'brawn) 10))
	'(id name)))

(define world5
    (world4 'update 'brawlers
	(λ (t) (> (apply + (map t '(brawn brains moves cool))) 12))
	(λ (t) (map (t 'cool 37) bfields))))

(world5)

(define world6
    (world5 'delete 'brawlers
	(λ (t) (< (apply + (map t '(brawn moves))) 8))))

(world6)

(world6 'relations)

(world6 'indexes)

(define nfields '(x y z i j k m n q))

(define w7 (world6 'add_relation 'numbers nfields))

(w7)

(current-memory-use)

(define (rand09 (x #f)) (random 10))
(define (rando xs) (map rand09 xs))

(define w8
    (let ((tuples (for/list ((i (in-range 0 12000))) (rando nfields))))
	(w7 'create 'numbers tuples)))

(current-memory-use)

(define (rand-update)
    (define (rand-fs)
	(take (shuffle nfields) 3))
    (define (rand-vs)
	(map rand09 '(1 1 1)))
    (define (rander)
	(map list (rand-fs) (rand-vs)))
    (define where (rander))
    (define put (rander))
    (cons where put))

(define upgrayydz
    (for/list ((i (in-range 0 1e3)))
	(rand-update)))

(time
    (for/fold ((relvar w8)) ((upg (in-list upgrayydz)))
	(relvar 'update 'numbers (car upg) (cdr upg))))

(current-memory-use)

(read)




