(load "mapa-trem.scm")
(import mapa)

(use srfi-18)

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

(define (move-train id)
	())

(define (synchronized-move-train id)
      (begin
        (define synchronizer ((id 'get-synchronizer)))    
  ((synchronizer move-train)) id))

(define trains
  (list
    (make-thread (lambda () (print 'a: (synchronized-move-train a))) 'p1)
    (make-thread (lambda () (print 'b: (synchronized-move-train b))) 'p2)))

(map thread-join! 
  (map thread-start! trains))

