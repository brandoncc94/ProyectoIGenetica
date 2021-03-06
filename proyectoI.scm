;------------------
     ;ARCHIVOS
;------------------
;Leer el archivo
(define leerArchivo
 (lambda(dir)
   (call-with-input-file dir
     (lambda (p)
       (let loop ((x (read-char p)))
         (cond ((eof-object? x) '())
               (#t (begin (cons x (loop (read-char p)))))))))))

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
    (+ min (random (+ 1 (- max min))))))

(define rand2
  (lambda(a b)
    (cond((zero? (random 2)) a) (else b))))

;Crear la población inicial
(define poblacionInicial
  (lambda(lista)
    (display "Lista: ") (display lista) (newline)
    (funciones-por-arbol 50))) 

;Definir cuantas funciones van a haber por cada árbol
(define funciones-por-arbol
  (lambda(n)
    (cond((zero? n) '())
         ((cons (generar-arbol (obtener-funciones (rand 5 15) ) '())
          (funciones-por-arbol (- n 1)))))))

;Lista de funciones
(define lista-funciones1 '(+ - * / expt bitwise-and bitwise-ior bitwise-xor)) ;PONER XOR
(define lista-funciones '(+ - * / bit-xor bit-and bit-or bit-xor)) ;PONER XOR

(define bit-or
  (lambda (x y)
    (bitwise-ior (inexact->exact (round x)) (inexact->exact (round y)))))

(define bit-and
  (lambda (x y)
    (bitwise-and (inexact->exact (round x)) (inexact->exact (round y)))))

(define bit-xor
  (lambda (x y)
    (bitwise-xor (inexact->exact (round x)) (inexact->exact (round y)))))


(define frecuencia '(12 12 12 12 10 14 14 14)) 

;Verificar valor en rango
(define freq
  (lambda(index)
    (cond((< index 5) (round(* (/ (list-ref frecuencia index) 100.0) 255)))
         (else(floor(* (/ (list-ref frecuencia index) 100.0) 255))))))

(define in-range
  (lambda(valor)
    (in-range-aux valor 0 0 (freq 0) )))

(define in-range-aux
  (lambda(valor index min max)
    (cond((> valor 255) -1)
         ((and (>= valor min) (<= valor max)) index)
         (else(in-range-aux valor (+ index 1) max (+ max (freq (+ index 1))))))))

;Definir cuales funciones van a haber por cada árbol
(define obtener-funciones
  (lambda(n)
    (cond((zero? n) '())
    (else(cons (list-ref lista-funciones (in-range (rand 0 255))) (obtener-funciones (- n 1)))))))

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
      ((= (rand2 0 1) 1)  
       (list (car arbol)  
                   (insertar-arbol (cadr arbol) value)
                   (caddr arbol)))
      ((list (car arbol)
                   (cadr arbol)
                   (insertar-arbol (caddr arbol) value))))))

;------------------
;MUTACIÓN
;------------------
(define mutacion
  (lambda(arbol)
    (cond((> (rand 0 100) 90) 
          (mut-aux arbol (list-ref lista-funciones (in-range (rand 0 255)))))
         (else arbol ))))

(define muta
  (lambda(arbol)
    (cond((> (rand 0 100) 90) 
          (mut1 arbol (list-ref lista-funciones (in-range (rand 0 255))) (rand 0 (altura arbol))))
         (else arbol ))))

;;MUTACION COMO LA DEL PROFE pero sin implementar en el programa
(define mut1
  (lambda (arbol fun n)
    (cond ((null? arbol) (list fun '() '()))
          ((= 0 n) (cond ((= 0 (rand2 0 1)) (list fun arbol '()))
                         (else (list fun '() arbol))))
          (else (cond ((= 0 (rand2 0 1)) (list (car arbol) (mut1 (cadr arbol) fun (- n 1)) (caddr arbol)))
                      (else (list (car arbol) (cadr arbol) (mut1 (caddr arbol) fun (- n 1)))))))))

(define mut-aux 
  (lambda (arb fun)
    (cond ((null? arb) (list fun '() '()))
          (else
           (cond ((= (rand2 0 1) 0) (list (car arb) (mut-aux (cadr arb) fun) (caddr arb)))
                 (else (list (car arb)  (cadr arb) (mut-aux (caddr arb) fun))))))))
              


(define altura
  (lambda (arb)
    (cond ((null? arb) -1)
          (else
           (+ 1 (max (altura (cadr arb))
                     (altura (caddr arb))))))))

;------------------
;FUNCION DE FITNESS
;------------------

;------------------
;Funcion de Generacion de Poblacion
;------------------
(define genPob
  (lambda (apt larg gen)
    (cond ((= larg 1) gen)
          (else (genPob apt (-  larg 1) (cons (genHijo apt) gen))))))

          
(define genHijo
  (lambda (padres)
    (muta (cruce1 (list-ref padres (rand 0 (- (length padres) 1) )) (list-ref padres (rand 0 (- (length padres) 1)))))))

(define genPob1
  (lambda (apt)
    (cond ((= 1 (length apt)) '())
          (else (append (g-aux (car apt) (cdr apt)) (genPob1 (cdr apt)))))))

(define g-aux
  (lambda (p1 p2)
    (cond ((null? p2) p2)
          (else (cons (cruce1 p1 (car p2)) (g-aux p1 (cdr p2)))))))
    

;------------------
;Funcion de Adaptabilidad
;------------------
;Funcion obtener el valor de la funcion
(define aptos
  (lambda (l funciones apto)
    (cond ((null? funciones) funciones)
          ((<= (evaluarFuncion l (car funciones)) apto) (cons (car funciones) (aptos l (cdr funciones) apto)))
          (else (aptos l (cdr funciones) apto)))))

(define prueba
  (lambda (l)
    (list-ref (sort l <) (inexact->exact (floor (+ 0.5 (/ (length l) 2)))))))

(define prueba1
  (lambda (l cant)
    (list-ref (sort l <) cant )))

(define promedioFitness
  (lambda (l funciones)
    (/ (suma (map (evalu l) funciones)) (length funciones)))) 

(define elitismo
  (lambda (pob l)    
    (list-ref pob (select-opt (cdr (map (evalu l) pob)) (car (map (evalu l) pob)) 0 1))))
    
;------------------
;Funcion de Eleccion de Optimo
;------------------
(define optimo?
  (lambda (pob l)
    (cond ((ormap (lambda(x) (eqv? x 0.0)) (map (evalu l) pob) )
           #true)
          (else
           #false))))

(define select-opt
  (lambda (l minimo pos cont)
    (cond ((null? l)  pos)
          ((< (car l) minimo) (select-opt (cdr l) (car l) cont (+ 1 cont)))
          (else (select-opt (cdr l) minimo pos (+ 1 cont))))))

(define (minim lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (minim (cdr lst))) (car lst))
          (else (minim (cdr lst)))))

;------------------
;Funcion de Cruce
;------------------
(define cruce
  (lambda (arb1 arb2)
    (cond ((= (rand 0 1) 0)
           (cond ((= (rand 0 1) 0)
                  (list (car arb1) (cadr arb1) (caddr arb2)))
                 (else (list (car arb1) (cadr arb2) (caddr arb1)))))
          (else 
           (cond ((= (rand 0 1) 0)
                  (list (car arb2) (cadr arb2) (caddr arb1)))
                 (else (list (car arb2) (cadr arb1) (caddr arb2))))))))

(define cruce1
  (lambda (arb1 arb2)
    (cond 
      ((null? arb1) arb2)
      ((null? arb2) arb1)
      ((= (rand2 0 1) 0) (list (car arb1) 
                              (cruce1 (cadr arb1) (cadr arb2))
                              (cruce1 (caddr arb1) (caddr arb2))))
      (else (list (car arb2) 
                              (cruce1 (cadr arb1) (cadr arb2))
                              (cruce1 (caddr arb1) (caddr arb2)))))))


;------------------
;Funcion de Fitness
;------------------
(define Fitness
  (lambda (l1 l2)
    (cond ((null? l1) l1 )
          (else (cons (convertDecimal (car l1) (car l2)) (Fitness (cdr l1) (cdr l2)))))))


(define convertDecimal 
  (lambda (x y)
    (abs (- 1.0 (/ x y))))) 
    

(define evaluacion
  (lambda (X Y arb)
    (eval (cambioHoja X Y 1 arb))))


(define evalu
  (lambda (l)
    (lambda (func) (evaluarFuncion l func))))

(define getResult
  (lambda (l)
    (caddr l)))
    
(define result-l
  (lambda (l)
    (suma (map getResult l))))


(define evaluarFuncion
 (lambda (l fun)
   (with-handlers ([exn:fail? (lambda (v) #f)])
    (* 100 (suma (Fitness (map (funcion fun) l) (map getResult l)))))))

(define filtrar
  (lambda (l pob flag pob1)
    (cond ((null? pob) (cond ((= flag 1) (filtrar l pob1 0 '()))
                             (else pob1)))
          ((eq? #f (evaluarFuncion l (car pob)))  (filtrar l (cdr pob) 1 (cons (generar-arbol (obtener-funciones 5) '()) pob1)))
          (else (filtrar l (cdr pob) flag (cons (car pob) pob1))))))

(define suma 
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ (car l) (suma (cdr l)))))))

(define funcion
  (lambda (fun)
    (lambda (l) (evaluacion (car l) (cadr l) fun))))

;------------------
;FUNCION PRINCIPAL
;------------------
(define genetica
  (lambda(nombreArchivo)
    (poblacionInicial (generarLista (info nombreArchivo)))))

(define genetica1
  (lambda (nombreArchivo)
    (gen-aux (filtrar (generarLista (info nombreArchivo)) (poblacionInicial (generarLista (info nombreArchivo))) 0 '()) 
             (generarLista (info nombreArchivo)) 0 50)))


(define gen-aux
  (lambda (pob lis countGen maxGen)
    (cond ((optimo? pob lis) (display 'optimo) (list-ref pob (select-opt (cdr (map (evalu lis) pob)) (car (map (evalu lis) pob)) 0 1)))
          ((= countGen maxGen) (display 'maxGen) (list-ref pob (select-opt (cdr (map (evalu lis) pob)) (car (map (evalu lis) pob)) 0 1)))
          (else 
           (newline)
           (display countGen)
           (newline)
           (display (prueba (map (evalu lis) pob)))
           (newline)
                 (gen-aux (cons (elitismo pob lis) (filtrar lis (genPob (aptos lis pob (prueba (map (evalu lis) pob))) (length pob) '()) 0 '()))
                                                      lis (+ 1 countGen) maxGen)))))


;-----------------------------------------------------------
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

;-----------------------------------------------------------
;-----------------------------------------------------------


(define funcionFinal 
  (lambda (fun)
    (lambda (X Y) (evaluacion X Y fun))))

;(define arb '(+ (* ()()) (/ (- () (* () ())) (+ () ()))))
;(define arb1 '(+ (* (/ () ()) (- () ())) (+ (expt (/ () ()) (* (+ () () ) (- () ()))) ())) )
(define l1 (generarLista (info "info.txt")))
;(define pobla (genetica "info.txt"))