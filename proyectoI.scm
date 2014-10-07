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