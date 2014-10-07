;Nombre del archivo
(define nombreArchivo "info.txt")

;Leer el archivo
(define (leerArchivo dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))

(define info(
   lambda() (foldr (lambda (x y) (string-append (string x) y)) "" (leerArchivo nombreArchivo))))