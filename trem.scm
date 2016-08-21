;; importa lista que representa o trajeto a ser percorrido pelos trens
(load "mapa-trem.scm")
(import mapa)

(load "concurrent.scm")
(import concurrent)

(use srfi-18)
(use srfi-1)
(use extras)

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
    (define (calc-metrics time-move) ; método que calcula métricas como, por exemplo, tempo médio de parada em estações
      (set! time (+ time time-move)))
	  
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
(print "Insira o tempo desejado de simulação: ")
(define runtime (string->number (read-line)))

;;Lista que sera utilizada para os metodos do relatorio. (id tempo)
(define lista-tempo '((1 0)(2 0)(3 0))) ;;Trem 1, 2 e 3

;;Metodos para geracao dos dados do relatorio
(define faz-total
	(lambda (x)
		(let total-i ((ls x)(acc 0))
			(if (null? ls)
				acc
			(total-i (cdr ls)(+ acc (cadar ls)))))))
			
(define faz-media
	(lambda (x)
		(/ x 3)))

(define update-tempo
  (lambda (id tempo)
    (set-car! (cdr (assq id lista-tempo)) tempo)))
	
(define (report)
	(call-with-output-file "relatorio.txt"  
		(lambda (output-port)
			(display "Relatório de Simulação" output-port)
			(newline output-port)
			(display "ID|Tempo" output-port)
				(newline output-port)
					(let f ((ls lista-tempo))
						(if (not (null? ls))
							(begin
								(display (car ls) output-port)
								(newline output-port)
								(f (cdr ls)))))
				(newline output-port)
				(define y (faz-total lista-tempo))
				(display "Tempo Total: " output-port)
				(display y output-port)
				(newline output-port)
				(display "Tempo medio: " output-port)
				(display (exact->inexact (faz-media y)) output-port)
				(newline output-port)
				(display "Mapa da linha" output-port)
				(newline output-port)
				(display mapa output-port))))

;; método responsável por atualizar a posição do trem na lista mapa (utiliza os métodos 'set-speed e 'calc-metrics)
(define (move-train id)
	(let move-i ((l mapa))
		(cond 
			((> (time->seconds (current-time)) runtime) 
				(begin
				(print "Gerando relatório de simulação...")
				(report)
				(thread-terminate! (current-thread))))
			((null? l) (print "Erro ao movimentar trem"))
			((= ((id 'get-id)) (cadar l)) (verify-move (cdr l) (car l) id))
			(else (move-i (cdr l)))))
			(print mapa)
			(print "Tempo: "((id 'get-time)))
			(print "Tempo de execução: "(time->seconds (current-time)))
			(update-tempo ((id 'get-id)) ((id 'get-time)))
			(move-train id))

(define verify-move
	(lambda (lista local id)
		(let ver-i ((l lista)(n 3))
			(cond 
				((null? l) 
					(begin
						(let loop-i ((lp mapa))
							(cond 
								((null? lp) (print "Erro ao movimentar trem"))
								((= ((id 'get-id)) (cadar lp))
									(begin
										(set! l (append lp (take mapa 29)))))
								(else (loop-i (cdr lp)))))
						(ver-i l 3)))
				((and (eq? n 0) (eq? 0 (cadar l)))
					(begin 
						(thread-sleep! 1.0)
						((id 'calc-metrics) 1)
						(move-change id 1 l)))
				((and (eq? n 1) (not (eq? 0 (cadar l))))
					(begin 
						((id 'calc-metrics) 2)
						(move-change id 1 l)))
				((and (eq? n 2) (not (eq? 0 (cadar l))))
					(begin 
						(thread-sleep! 3.0)
						((id 'calc-metrics) 3)
						(move-change id 1 l)))
				((and (eq? n 3) (not (eq? 0 (cadar l))))
					(begin 
						(thread-sleep! 4.0)
						((id 'calc-metrics) 4)
						(move-change id 1 l)))
				((= (cadar l) 0) (ver-i (cdr l) (- n 1)))
				(else (thread-sleep! 1.0) (ver-i l n))))))

(define (move-change id posicao lp)
	(let move-change-i ((l mapa))
			(cond 
				((= 1 (length l)) 
					(begin
						(set! l lp)
						(move-change-i l)))
				((= ((id 'get-id)) (cadar l))
					(begin 
						(print "Movimentando trem " ((id 'get-id)))
						(set-car! (cdr (assq (caar l) mapa)) 0)
						(let loop-i ((lista l) (n posicao))
							(if (= n 0)
								(set-car! (cdr (assq (caar lista) mapa)) ((id 'get-id)))
								(loop-i (cdr lista) (- n 1))))))
				(else (move-change-i (cdr l))))))	


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
	(map thread-start! trains)(list (+ runtime 1) (+ runtime 1) (+ runtime 1)))

