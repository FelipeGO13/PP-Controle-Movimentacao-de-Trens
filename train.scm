(load "concurrent.scm")
(import concurrent)

(use srfi-18)

; a simple account object

(define make-train
  (lambda (id time)
    (define synchronizer (make-synchronizer))
    (define (set-speed)
      ())
    (define (calc-metrics)
      ())
    (define (get-synchronizer) synchronizer)
    (lambda (m) ; dispatcher
      (cond ((eq? m 'set-speed) set-speed)
            ((eq? m 'calc-metrics) calc-metrics)
            (else (error "Unknown method"))))))

(define a (make-train 1 0))
(define b (make-train 2 0))

(define (move id)
	(

(define (synchronized-move id)
      (begin
        (define synchronizer ((id 'get-synchronizer)))    
  ((synchronizer1 (synchronizer2 transfer)) from to amount))

(define trains
  (list
    (make-thread (lambda () (print 'a: (synchronized-transfer a b 200))) 'p1)
    (make-thread (lambda () (print 'b: (synchronized-transfer b a 300))) 'p2)))

(map thread-join! 
  (map thread-start! trains))

