;; importa lista que representa o trajeto a ser percorrido pelos trens
(load "mapa-trem.scm")
(import mapa)

(use srfi-18)

;; objeto trem: este objeto simula a movimentação de um trem ao longo da um mapa/trajeto
(define make-train
  (lambda (id time)
    (define (set-speed) ; método que define a velocidade de um trem em função da quantidade de trens nos próximos 3 trechos a frente
      ())
    (define (calc-metrics) ; método que calcula métricas como, por exemplo, tempo médio de parada em estações
      ())
    (define (get-synchronizer) synchronizer)
    (lambda (m) ; dispatcher
      (cond ((eq? m 'set-speed) set-speed)
            ((eq? m 'calc-metrics) calc-metrics)
            (else (error "Unknown method"))))))

;; criação das threads/trens da linha de trem
(define a (make-train 1 0))
(define b (make-train 2 0))

;; método responsável por atualizar a posição do trem na lista mapa (utiliza os métodos 'set-speed e 'calc-metrics)
(define (move-train id)
	())

;; definição do trecho crítico
(define (synchronized-move-train id)
      (begin
        (define synchronizer ((id 'get-synchronizer)))    
  ((synchronizer move-train)) id))


(define trains
  (list
    (make-thread (lambda () (print 'a: (synchronized-move-train a))) 'p1)
    (make-thread (lambda () (print 'b: (synchronized-move-train b))) 'p2)))

;; inicialização das threads definidas na lista trains
(map thread-join! (map thread-start! trains) (list 5 5))

