; a module containing several concurrency helpers
(module concurrent (wait make-synchronizer)
  (import chicken scheme)
  (use srfi-18)
  ; helper procedure to make a thread wait
  (define (wait name dur)
    (if (eq? (thread-name (current-thread)) name)
        (thread-sleep! dur)))

  ; a factory of synchronizers
  ; a synchronizer is an operation that wraps a procedure as a critical
  ; section; all procedures wrapped by the synchronizer will use the same
  ; lock, consequently, all  those procedures will be mutually exclusive between
  ; each other
  (define (make-synchronizer)
    (let ((mx (make-mutex)))
      (lambda (f)
        (define (synchronized-f . args)
          (mutex-lock! mx)
          (let ((result (apply f args)))
            (mutex-unlock! mx)
            result))
        synchronized-f)))  )
