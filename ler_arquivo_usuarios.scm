;;Módulo para a leitura do arquivo de registros de login e senha em formato .txt para autenticação de usuário

(module leitura (ler-arquivo-linhas ler-arquivo-helper ler-arquivo)
  	(import chicken scheme)
	(use extras)
	
	;;Realiza a leitura das linhas do arquivo e as retorna em uma lista
	(define ler-arquivo-linhas
        (lambda (file-input-port)
                (let* ((line (read-line file-input-port))
                      )
                      (if (eof-object? line)
                          (list)
                          (cons line (ler-arquivo-linhas file-input-port))))))

	
	;;Função auxiliar verifica se a porta de entrada existe para realizar a leitura do arquivo, caso exista chaam função para 
	;;leitura das linhas 
	(define ler-arquivo-helper
        (lambda (file-port)
                (if (input-port? file-port)
                    (ler-arquivo-linhas file-port)
                    (list))))

	;;Cria uma porta de entrada para leitura do arquivo de com o registro dos usuários e passa esta porta como parâmetro
	;;para a função auxiliar
	(define (ler-arquivo)
            (call-with-input-file "usuarios.txt" ler-arquivo-helper)
	)

	
	
)





