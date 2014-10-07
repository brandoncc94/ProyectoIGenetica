;Leer el archivo
(define (leerArchivo dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))

(define info
  (lambda(nombreArchivo) 
    (foldr (lambda (x y) (string-append (string x) y)) "" (leerArchivo nombreArchivo))))

(define genetica
  (lambda(nombreArchivo)
    (info nombreArchivo)))



(define arb
  '(+
    (* ()())
    (/ 
     (- () (* () ()))
     (+ () ()))))

(define evaluacion
  (lambda (X Y arb)
    (eval (cambioHoja X Y 1 arb))))

(define cambioHoja
  (lambda (X Y hijo arb)
    (cond ((null? arb)
           (cond ((eq? hijo 1) X)
                 (else Y)))
          (else
           (list (car arb) (cambioHoja X Y 1 (cadr arb))
              (cambioHoja X Y -1 (caddr arb)))))))
