#lang racket
;Proyecto
(define display-p
  (lambda(L)
    (if (null? L) null
        (d-p L 0))))
(define d-p
  (lambda(L n)
    (cond
      ((null? L) null)
      ((= (car L) 0) (d-p (cdr L) (+ n 1)))
      ((= n 0)
       (if(null? (cdr L))
          (car L)
          (append(list (car L)) '(+) (d-p (cdr L) (+ n 1)))))
      ((= n 1)
       (if (null? (cdr L))
           (list (car L) 'x)
           (append(list (car L)'x ) '(+) (d-p (cdr L) (+ n 1)))))
      ((= (length L) 1) (list(car L) 'x '^ n))
          (else (append
                 (list(car L) 'x '^ n) '(+) (d-p (cdr L) (+ n 1)))))))

;Metodo cuenta cuantos polinomios tiene una lista, metodos para ayudar.
(define cuantos-p
  (lambda (polinomios)
    (if (null? (car polinomios)) 0
        (c-p polinomios 1))))

(define c-p
  (lambda (polinomios n)
    (if (null? (cdr polinomios)) n
        (c-p (cdr polinomios) (+ n 1)))))



;Suma de polinomios---------------------------------------------------------------------------
(define suma-p
  (lambda (polinomios)
    (cond ((=(cuantos-p polinomios) 1) polinomios)
          ((=(cuantos-p polinomios) 2) (suma-doble (car polinomios) (cadr polinomios)))
          ((=(cuantos-p polinomios) 3) (suma-doble (suma-doble (car polinomios) (cadr polinomios)) (caddr polinomios)))
          (else 
          (suma-p (cons (suma-doble (car polinomios) (cadr polinomios)) (cddr polinomios))))
          )
    )
  )

(define suma-doble
  (lambda (p1 p2)
    (cond ((and(null? p1) (null? p2)) '())
          ((null? p1) p2)
          ((null? p2)  p1)
          ( else ( cons (+ (car p1) (car p2)) (suma-doble (cdr p1) (cdr p2))))
          )
    )
  )

;Resta de polinomios-----------------------------------------------------------------
(define negar-lista
  (lambda (lista)
    (cond ((null? lista) '())
          (else
          (cons (- (car lista)) (negar-lista (cdr lista)))))))
    

    
(define resta-doble
  (lambda (p1 p2)
    (cond ((and(null? p1) (null? p2)) '())
          ((null? p1) (negar-lista p2))
          ((null? p2)  p1)
          ( else ( cons (- (car p1) (car p2)) (resta-doble (cdr p1) (cdr p2))))
          )
    )
  )

(define resta-p
  (lambda (polinomios)
    (cond ((=(cuantos-p polinomios) 1) polinomios)
          ((=(cuantos-p polinomios) 2) (resta-doble (car polinomios) (cadr polinomios)))
          ((=(cuantos-p polinomios) 3) (resta-doble (resta-doble (car polinomios) (cadr polinomios)) (caddr polinomios)))
          (else 
          (resta-p (cons (resta-doble (car polinomios) (cadr polinomios)) (cddr polinomios))))
          )
    )
  )

;Multiplicacion de polinomios----------------------------------------------------
(define lista-pos ;Crea una lista con un numero x elevado a la y
  (lambda (x y)
    (if (= y 0) (list x)
        (append (list 0) (lista-pos x (- y 1))))
        )
    )
(define mult1
  (lambda (mon posMon L)
    (if (null? L) '(0)
        (suma-p (mul-mon-p mon posMon L 0)))))

(define mul-mon-p ;Multiplica un monomio por un polinomio.
  (lambda (mon posMon L posCar)
    (if (null? L) '()
          ( cons(lista-pos (* mon (car L)) (+ posMon posCar)) (mul-mon-p mon posMon (cdr L) (+ posCar 1))))))
 
(define multi-doble ;Multiplica 2 polinomios
  (lambda(L1 L2)
    (if (null? L1) '()
     (suma-p(m-doble L1 0 L2)))))

(define m-doble
  (lambda (L1 posCarL1 L2)
    (if(null? L1) '()
       (cons (mult1 (car L1) posCarL1 L2) (m-doble (cdr L1) (+ posCarL1 1) L2) ))))

(define multi-p
  (lambda(polinomios)
     (cond ((=(cuantos-p polinomios) 1) polinomios)
          ((=(cuantos-p polinomios) 2) (multi-doble (car polinomios) (cadr polinomios)))
          ((=(cuantos-p polinomios) 3) (multi-doble (multi-doble (car polinomios) (cadr polinomios)) (caddr polinomios)))
          (else 
          (multi-p (cons (multi-doble (car polinomios) (cadr polinomios)) (cddr polinomios))))
          )
    )
  )


;Division-----------------------------------------------------------------------------------
(define elimina0Final
  (lambda (L)
    (if(null? L) '()
       (if (and (=(length L) 1) (=(car L) 0)) '()
           (cons (car L) (elimina0Final (cdr L)))))))
    
(define ultimoLista
  (lambda (L) ;devuelve el ultimo elemento de la lista
    (if (null? L) 0
    (if (null? (cdr L)) (car L)
        (ultimoLista (cdr L))))))
(define posUltimoLista ;Devuelve la posicion del ultimo elemento de la lista.
  (lambda (L)
    (if(null? L) -1;Lista vacia
       (pos-Ult L 0))))
(define pos-Ult
  (lambda (L val)
     (if (null? (cdr L)) val
        (pos-Ult (cdr L) (+ val 1)))))

(define primerCociente ;Devuelve el primer cociente. No se pueden recibir listass vacias.
  (lambda (L)
    (if (null? L) -1
         (if (=(car L)0) (primerCociente (cdr L)) 
             (car L)))))
(define posPrimerCociente ;Da la posicion del primer cociente.
  (lambda (L)
    (if (null? L) 0
        (pos-Pco L 0))))
(define pos-Pco
  (lambda (L pos)
    (if (= (car L) 0) (pos-Pco (cdr L) (+ pos 1))
        pos)))

(define susDiv1
  (lambda (dividiendo cociente divisor) ; Hace el nuevo dividiendo , dividiendo -(cociente * divisor) ;Primero se hace susDiv2
  (resta-doble dividiendo (mult1 (primerCociente cociente) (posPrimerCociente cociente) divisor))))   ;Practicamente es el residuo.

(define susDiv2 ;Devuelve el resultado de division , ultimoDividiendo / ultimodivisor. Cociente
  (lambda (dividiendo divisor); Para ese metedo tiene que validar antes que (pos1 -pos2 ) >= 0
    (lista-pos (/(ultimoLista dividiendo) (ultimoLista divisor)) (-(posUltimoLista dividiendo) (posUltimoLista divisor)))))
(define calculaCociente
  (lambda (dividiendo divisor cociente)
    (suma-doble cociente (susDiv2 dividiendo divisor))))

 ;Funciones devuelven el cociente de la division (respuesta)  
(define divi-p
  (lambda (dividiendo divisor)
    (if (<=(-(posUltimoLista dividiendo) (posUltimoLista divisor)) 0) '()
        (dvi-p dividiendo divisor '() )
        )))
(define dvi-p
  (lambda (dividiendo divisor cociente)
     (if (<(-(posUltimoLista dividiendo) (posUltimoLista divisor)) 0) cociente ;Fin de recursividad, retorna respuesta
         (dvi-p (elimina0Final(susDiv1 dividiendo (calculaCociente dividiendo divisor cociente) divisor)) divisor (calculaCociente dividiendo divisor cociente)))))   
;Resiudo de la division
(define divi-p-residuo
  (lambda (dividiendo divisor)
    (if (<=(-(posUltimoLista dividiendo) (posUltimoLista divisor)) 0) '()
        (dvi-p-residuo dividiendo divisor '() )
        )))
(define dvi-p-residuo
  (lambda (dividiendo divisor cociente)
     (if (<(-(posUltimoLista dividiendo) (posUltimoLista divisor)) 0) dividiendo
         (dvi-p-residuo (elimina0Final(susDiv1 dividiendo (calculaCociente dividiendo divisor cociente) divisor)) divisor (calculaCociente dividiendo divisor cociente)))))      
;Division compelte (Cociente y residuo)         
(define divi-total
  (lambda (dividiendo divisor)
    (list (divi-p dividiendo divisor) (divi-p-residuo dividiendo divisor))))

;-------------------------Derivacion----------------------------------
(define deriva-n ;Crea un lista nueva segun la derivacion de n
 (lambda (n pos)
   (if (= pos 0) '(0)
       (lista-pos (* n pos) (- pos 1)))))

(define deriva-p ;Deriva un solo polinomio
 (lambda (polinomio)
   (if (null? polinomio) '()
       (deri-p polinomio 0))))
(define deri-p
 (lambda (polinomio pos)
   (if (=(length polinomio) 1) (deriva-n (car polinomio) pos)
       (suma-doble (deriva-n (car polinomio) pos) (deri-p (cdr polinomio) (+ pos 1))))))
              
;Deriva una lista de polinomios
(define deriva-polinomios
  (lambda (polinomios)
    (map deriva-p polinomios)))

;-----------------------------Evaluacion--------------------------------------------------
(define exponente ;Saca el exponente de un n elevado a la x
  (lambda (n x)
    (cond ((= n 0) 0)
          ((= x 1) n)
          (
           else(* n(exponente n (- x 1))))))) 

(define listaExponentes ;Crea una nueva lista del tamaño de "lista" pero elevando cada posicion y no numero.
  (lambda (lista x)
    (if (<=(length lista) 1) lista
        (cons 1 (lista-e (cdr lista) x 1)))))
(define lista-e ;
  (lambda (lista x pos)
    (if (null? lista) '()
        (cons (exponente x pos) (lista-e (cdr lista) x (+ pos 1))))))
(define suma-lista
  (lambda (lista)
    (if(null? lista) 0
       (+ (car lista) (suma-lista (cdr lista))))))
       
(define evalua; Multiplica 2 listas (La lista del polinomio y otra lista que se crea en base al tamaño del polinomio la cual contiene los exponentes de x)
  (lambda (lista x) ;Luego suma la lista resultante.
    (suma-lista (map * lista (listaExponentes lista x)))))

;Factorizacion--------------------------------
;Grado 2.----------------------------
(define discriminante
  (lambda (lista)
    (- (exponente (cadr lista) 2) (* 4 (car lista) (caddr lista)))))

(define formula
  (lambda (lista)
    (/(+(-(cadr lista)) (sqrt(discriminante lista))) (* 2(caddr lista)))
  ))
(define formula2
  (lambda (lista)
    (/(-(-(cadr lista)) (sqrt(discriminante lista))) (* 2(caddr lista)))
    ))

(define grado2 ;Tiene que ser una lista de tam 3
  (lambda (lista)
    (if (and (=(length lista) 3) (not(eq? (caddr lista) 0)))
    (list (list (-(formula lista)) 1) (list (*(caddr lista) (-(formula2 lista))) (*(caddr lista)1)))
    '()
    )))
;GRADO 3--------------------------------------------
(define reverse1 ;Invierte una lista
  (lambda (l)
  (if (null? l)
     '()
     (append (reverse1 (cdr l)) (list (car l)))
  )))
(define uneListas ;Une 2 listas ( del mismo tamaño)
  (lambda (lista1 lista2)
    (if(null? lista1) '()
       (cons (car lista1) (cons (car lista2) (uneListas (cdr lista1) (cdr lista2)))))))

(define ordena-raices ;REcibe las posibles raices de un polinomio y las ordena para usar en division sintetica.
  (lambda (positivos negativos)
    (uneListas (reverse1 positivos) (reverse1 negativos))))

(define divisores-positivos
 (lambda (n)
   (if (>= n 1)
       (divisores n n)
       (divisores (* -1 n) (* -1 n)))))

(define divisores-negativos
 (lambda(n)
   (if (>= n 1)
       (map - (divisores n n))
       (map - (divisores (* -1 n) (* -1 n))))))
(define divisores-PN ;Esto une los divisores negativos y positivos del elemento del ultimo elemento del polininomio (El que no tiene x)
  (lambda (n)
    (ordena-raices (divisores-positivos n) (divisores-negativos n)))) ;Acomodar estos.....

(define divisores-ultimo ;REtorna los divisores del ultimo de la lista. El x de mayor grado
  (lambda (n)
    (reverse1 (divisores-positivos n))))

(define divisores
  (lambda (n div)
    (if (> div 0)
        (if (integer? (/ n div)) (cons div (divisores n (- div 1)))
            (divisores n (- div 1)))
        '())))
;Dividir los divisores del ultimo elemento contra los divisores del primero. (Del primero solo ocupo los positivos.
(define divide-N-Lista
  (lambda (lista n)
    (if (null? lista) '()
        (cons (/ (car lista) n) (divide-N-Lista (cdr lista) n)))))

(define posibles-raices
  (lambda (lista)
    (if (null? lista) '()
        (posibles-raices2  (divisores-PN (car lista)) (divisores-ultimo (ultimoLista lista))))))

(define posibles-raices2
  (lambda (listaA listaB) ;Dividir Cada car de listaB por todos los de lista A
    (if(null? listaB) '()
       (append (divide-N-Lista listaA (car listaB)) (posibles-raices2 listaA (cdr listaB))))))
;------------Invertir la lista para poder hacer la division sintetica------------------
(define concatenar
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          (else (cons (car l1)
                      (concatenar (cdr l1) l2))))))
;; Funcion invertir
;; invierte una lista
(define invertir
  (lambda (l)
    (cond ((null? l) l)
          (else (concatenar (invertir (cdr l))
                            (list (car l)))))))
;------------DIVISION SINTETICA---------
(define div-s
  (lambda (lista)
    (div-s2 (invertir lista) (posibles-raices lista)))) ; HAY que invertir la lista para poder aplicar la division sintetica.

(define div-s2
  (lambda (listaInvertida raices)
    (if(null? raices)  (display '(El polinomio no tiene raices reales))
    (if (null? (div-s3 (cdr listaInvertida) (car listaInvertida) (car raices)))
        (div-s2 listaInvertida (cdr raices))
        (car raices)))))

(define div-s3
  (lambda (lista primero x) ;Este metodo recibe: (cdr de lista) (car lista) y X que es un elemento de la raiz.
    (if (>(length lista) 1) ; Osea todavia no estamos en el ultimo elemento.
        (div-s3 (cdr lista)(+(* primero x) (car lista)) x)
        (if (=(+ (car lista) (* x primero)) 0) x ; Esta es la raiz :D
            '()))))
(define eliminaFinal
  (lambda (L)
    (if(null? L) '()
       (if (=(length L) 1)  '()
           (cons (car L) (eliminaFinal (cdr L)))))))

(define div-s11 ;Crea el polinomio de grado 2, que da como resultado de la division sintetica.
  (lambda (lista x) ;Ya va a recibir la cola de lista, el numero y el X
    (cons (car (invertir lista))(div-s22 (cdr(invertir lista)) (car (invertir lista)) x)))) ; HAY que invertir la lista para poder aplicar la division sintetica.

(define div-s22 
  (lambda (lista primero x)
    (if(>(length lista) 1)
       (cons (+ (* primero x) (car lista)) (div-s22 (cdr lista) (+ (* primero x) (car lista)) x))
       '())))

(define grado3 ;Tiene que ser una lista de tam 4
  (lambda (lista)
    (if (and (=(length lista) 4) (not(eq? (cadddr lista) 0)))
        (if (integer? (div-s lista))
            (cons (list (-(div-s lista)) 1 ) (grado2 (invertir(div-s11 lista (div-s lista)))))
             (display '(El polinomio no tiene raices reales)))
         (display '(El polinomio no cumple con los requisitos)))))

;GRADOS SUPERIORES-----------------------------------------
(define gradoN
  (lambda (lista)
    (cond ((=(length lista) 3) (grado2 lista))
          ((=(length lista) 4) (grado3 lista))
          (else (cons (list (-(div-s lista)) 1 ) (gradoN (invertir(div-s11 lista (div-s lista)))))))))
