#lang racket

;; ============================================
;; LÓGICA FUNCIONAL DEL BUSCA MINAS
;; Paradigma funcional puro (solo recursión, sin variables ni let/map/apply)
;; ============================================

;; -------------------------
;; (make-row n)
;; Crea una fila de longitud n llena de ceros. 
;; Esta es una funcion auxiliar para make-grid
;; make-grid hace cada fila, y make-row hace cada elemento (columna) de la fila)

;; Entrada: n (entero >= 0)
;; Salida: lista de enteros
;; Ejemplo: (make-row 3) => '(0 0 0)
;; -------------------------

(define (make-row n)
  (cond [(= n 0) '()] ; caso base, devuelve lista vacia si ya no hacen falta mas filas
        [else (cons 0 (make-row (- n 1)))])) ; caso recursivo, crea una columna con un 0 
        ;y llama con n-1, donde n son las columnas 

;; -------------------------
;; (make-grid rows cols)
;; Crea una cuadrícula de tamaño rows × cols llena de ceros.
;; Entrada: rows, cols (enteros >= 0)
;; Salida: lista de listas
;; Ejemplo: (make-grid 2 3) => '((0 0 0) (0 0 0))
;; -------------------------

(define (make-grid rows cols)
  (cond [(= rows 0) '()] ; caso base, igual a make-row
        [else (cons (make-row cols) ; caso recursivo, cons agrega una fila (hecha con make-row)
                    (make-grid (- rows 1) cols))])) ;cuando make-row termina, sigue recursivamente

;; -------------------------
;; (grid-ref grid row col)
;; Devuelve el valor en la posición (row, col).
;; Entrada: grid (lista de listas), row, col (enteros >= 0)
;; Salida: número
;; Ejemplo: (grid-ref '((1 2)(3 4)) 1 0) => 3
;; -------------------------

(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col)) ; list-ref obtiene el elemento en la posicion dada
  ; primero obtiene la fila y luego la columna

;; -------------------------
;; (replace-nth lst idx new-val)
;; Reemplaza el elemento en la posición idx de una lista.
;; Entrada: lista lst, entero idx, valor new-val
;; Salida: nueva lista con el cambio aplicado
;; Ejemplo: (replace-nth '(a b c) 1 'x) => '(a x c)
;; -------------------------

(define (replace-nth lst idx new-val)
  (cond [(null? lst) '()]
        [(zero? idx) (cons new-val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (- idx 1) new-val))]))

;; -------------------------
;; (grid-set grid row col val)
;; Devuelve una nueva cuadrícula igual a grid pero con la celda (row,col) cambiada.
;; Entrada: grid, row, col, val
;; Salida: nueva cuadrícula (lista de listas)
;; Ejemplo: (grid-set '((0 0)(0 0)) 1 1 9) => '((0 0)(0 9))
;; -------------------------

(define (grid-set grid row col val)
  (replace-nth grid row
               (replace-nth (list-ref grid row) col val)))

;; Se exportan las funciones para que la GUI la pueda usar
(provide make-row make-grid grid-ref grid-set)


