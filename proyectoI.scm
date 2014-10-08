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
;Función para obtener un número random en un rango específico
(define rand
  (lambda(min max)
    (+ min (random(+ 1(- max min))))))

;Crear la población inicial
(define poblacionInicial
  (lambda(lista)
    (display "Lista: ") (display lista) (newline)
    (funciones-por-arbol (rand 5 15)  ))) 

;Definir cuantas funciones van a haber por cada árbol
(define funciones-por-arbol
  (lambda(n)
    (cond((zero? n) '())
         ((cons (generar-arbol (obtener-funciones (rand 1 5)) '())
          (funciones-por-arbol (- n 1)))))))

;Lista de funciones
(define lista-funciones '(+ - * / expt and or))

;Definir cuales funciones van a haber por cada árbol
(define obtener-funciones
  (lambda(n)
    (cond((zero? n) '())
    (else(cons (list-ref lista-funciones (rand 0 6)) (obtener-funciones (- n 1)))))))

;Generar el árbol para cada función
(define generar-arbol
  (lambda(lista arb)
    (cond((null? lista) arb)
       (else(generar-arbol (cdr lista) (insertar-arbol arb (car lista)))))))

;Insertar en el árbol, o crearlo en caso de estar vacío
(define insertar-arbol
  (lambda (arbol value)
    (cond
      ((null? arbol) (list value (append '()) (append '()))) 
      ((= (rand 0 1) 1)  
       (list (car arbol)  
                   (insertar-arbol (cadr arbol) value)
                   (caddr arbol)))
      ((list (car arbol)
                   (cadr arbol)
                   (insertar-arbol (caddr arbol) value))))))
;------------------
;FUNCION DE FITNESS
;------------------

;;AQUIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII


;------------------
;FUNCION PRINCIPAL
;------------------
(define genetica
  (lambda(nombreArchivo)
    (poblacionInicial (generarLista (info nombreArchivo)))))


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

