(load "ler_arquivo_usuarios.scm")
(import leitura)

;; Função auxiliar para separar as Strings a partir de um caractere passado como parâmetro
(define str-split
		(lambda (str ch)
		(let ((len (string-length str)))
		(letrec
		((split
			(lambda (a b)
			(cond
				((>= b len) (if (= a b) '() (cons (substring str a b) '())))
				((char=? ch (string-ref str b)) (if (= a b)
					(split (+ 1 a) (+ 1 b))
					(cons (substring str a b) (split b b))))
                (	else (split a (+ 1 b)))))))
	(split 0 0)))))
	
;; Função para autentica a entrada de um usuário no sistema, verificando login e senha	
;; ainda é necessário concluir a autenticação, porém leitura dos registros já ocorre.
(define autenticacao
	(lambda (login senha)	
		(define lista-login (ler-arquivo))
		(define get-login 
			(let get-login-i ((logins '()) (l lista-login))
				(if (null? l) 
					(reverse logins)
					(get-login-i (cons (car (str-split (car l) #\:)) logins) (cdr l)))))
		(define get-senha 
			(let get-senha-i ((senhas '()) (l lista-login))
				(if (null? l) 
					(reverse senhas)
					(get-senha-i (cons (cadr (str-split (car l) #\:)) senhas) (cdr l)))))
		(define (get-lista) lista-login)
			(lambda (m) ; dispatcher
				(cond ((eq? m 'test) test)
				((eq? m 'get-lista) lista-login)
				((eq? m 'get-login) get-login)
				((eq? m 'get-senha) get-senha)
				(else (error "Unknown method"))))))
		 
;; Teste para verificar funcionamento da função de autenticação
(define teste (autenticacao 'joao.silva '12345))
(print (teste 'get-lista))
(print 'logins: (teste 'get-login))
(print 'senhas: (teste 'get-senha))


