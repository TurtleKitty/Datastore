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




