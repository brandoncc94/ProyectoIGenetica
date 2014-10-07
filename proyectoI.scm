;Read a file to a list of chars
(define (leerArchivo dir)
 (call-with-input-file dir
   (lambda (input-port)
     (let loop ((x (read-char input-port)))
       (cond 
        ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char input-port))))))))))

(foldr (lambda (x y) (string-append (string x) y)) "" (leerArchivo "info.txt"))