;------------------
     ;ARCHIVOS
;------------------
;Leer el archivo
(define (leerArchivo dir)
 (call-with-input-file dir
   (lambda (p)
     (let loop ((x (read-char p)))
       (cond ((eof-object? x) '())
        (#t (begin (cons x (loop (read-char p))))))))))

;Acumula los datos del archivo, que es lo que hace foldr
(define info
  (lambda(nombreArchivo) 
    (foldr (lambda (x y) (string-append (string x) y)) "" (leerArchivo nombreArchivo))))

;--------------------------------
;LISTA CON LOS DATOS DEL TXT
;--------------------------------
;Darle formato requerido a la lista
(define formatoLista
  (lambda(datos)
    (map string->number (regexp-split " " (regexp-replace* "\n" datos " ")))))

;Generar la lista de los datos leídos
(define generarLista
  (lambda(datos)
    (cond((eqv? "" datos) (display "Error: No existen datos de entrada para ser evaludos."))
         (else(agrupar-por-n (remove #f (formatoLista datos)) 3)))))

;Agrupamos en lista de listas
(define agrupar-por-n
  (lambda(lista n)
    (cond((null? lista) '())
         (else(cons (segmentarLista lista n) (agrupar-por-n (cdddr lista) n))))))

;Obtenemos los primeros N elementos de la lista
(define segmentarLista
  (lambda(pLista n)
    (cond((= 0 n) '())
         (else(cons (car pLista) (segmentarLista (cdr pLista) (- n 1)))))))

;------------------
;OPERACIONES BINARIAS
;------------------
;(define (xor a b) (and (not (and a b)) (or a b)))

;(and 1100111 1011001)
;(or 1100111 1011001)
;(xor 1100111 1011001)

;------------------------------------
          ;ALGORITMO GENÉTICO
;------------------------------------
;------------------
;POBLACIÓN INICIAL
;------------------
(define poblacionInicial
  (lambda(lista)
    (lista)))
;------------------
;FUNCION PRINCIPAL
;------------------
(define genetica
  (lambda(nombreArchivo)
    (generarLista (info nombreArchivo))))

(define arb
  '(+
    (* ()())
    (/ 
     (- () (* () ()))
     (+ () ()))))

(define evaluacion
  (lambda (X Y arb)
    (eval (cambioHoja X Y 1 arb))))

(define crearArb
  (lambda (func)
    (list func () ())))

(define cambioHoja
  (lambda (X Y hijo arb)
    (cond ((null? arb)
           (cond ((eq? hijo 1) X)
                 (else Y)))
          (else
           (list (car arb) (cambioHoja X Y 1 (cadr arb))
              (cambioHoja X Y -1 (caddr arb)))))))

