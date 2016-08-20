;; importa lista que representa o trajeto a ser percorrido pelos trens
(load "mapa-trem.scm")
(import mapa)

(load "concurrent.scm")
(import concurrent)

(use srfi-18)
(use srfi-1)

;; objeto trem: este objeto simula a movimentação de um trem ao longo da um mapa/trajeto
(define make-train
  (lambda (id time)
	(define synchronizer (make-synchronizer))
    (define (set-speed) ; método que define a velocidade de um trem em função da quantidade de trens nos próximos 3 trechos a frente
      (set! time 5))
    (define (get-time) ; método que define a velocidade de um trem em função da quantidade de trens nos próximos 3 trechos a frente
      time)
  (define (get-id) ; método que define a velocidade de um trem em função da quantidade de trens nos próximos 3 trechos a frente
      id)
    (define (calc-metrics) ; método que calcula métricas como, por exemplo, tempo médio de parada em estações
      (set! time 5))
    (define (get-synchronizer) synchronizer)
    (lambda (m) ; dispatcher
      (cond 
			((eq? m 'set-speed) set-speed)
			((eq? m 'get-time) get-time)
			((eq? m 'get-id) get-id)
			((eq? m 'calc-metrics) calc-metrics)
			((eq? m 'get-synchronizer) get-synchronizer)
            (else (error "Unknown method"))))))

;; criação das threads/trens da linha de trem
(define a (make-train 1 0))
(define b (make-train 2 0))
(define c (make-train 3 0))

(define mapa mapa-trem)

;; método responsável por atualizar a posição do trem na lista mapa (utiliza os métodos 'set-speed e 'calc-metrics)
(define (move-train id)
(print "teste de execucao de threads")
((id 'get-id))) 

;;Necessario implementar loop para setar posicao inicial dos trens
(define (set-trains)
	(set-car! (cdr (assq 'e0 mapa)) 1)
	(set-car! (cdr (assq 'e1 mapa)) 2)
	(set-car! (cdr (assq 'e2 mapa)) 3))

(set-trains)

;; definição do trecho crítico
(define (synchronized-move-train id)
      (begin
        (define synchronizer ((id 'get-synchronizer))))    
  ((synchronizer move-train) id))


(define trains
  (list
    (make-thread (lambda () (print 'a: (synchronized-move-train a))) 'p1)
    (make-thread (lambda () (print 'b: (synchronized-move-train b))) 'p2)
	(make-thread (lambda () (print 'c: (synchronized-move-train c))) 'p3)))

;; inicialização das threads definidas na lista trains
(map thread-join! 
	(map thread-start! trains)(list 2 2 2))


