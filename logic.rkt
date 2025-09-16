#lang racket
;; ============================================
;; LÓGICA FUNCIONAL DEL BUSCA MINAS
;; ============================================

;; Crea una fila de n ceros
(define (make-row n)
  (cond [(= n 0) '()]
        [else (cons 0 (make-row (- n 1)))]))

;; Crea una grilla de rows x cols
(define (make-grid rows cols)
  (cond [(= rows 0) '()]
        [else (cons (make-row cols)
                    (make-grid (- rows 1) cols))]))

;; Devuelve el valor en (row, col)
(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col))

;; Reemplaza el índice idx por new-val en una lista
(define (replace-nth lst idx new-val)
  (cond [(null? lst) '()]
        [(zero? idx) (cons new-val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (- idx 1) new-val))]))

;; Reemplaza el valor en la grilla en (row, col)
(define (grid-set grid row col val)
  (replace-nth grid row
               (replace-nth (list-ref grid row) col val)))

;; ============================================
;; Bombas aleatorias
;; ============================================

;; genera coordenada aleatoria (fila . col)
(define (random-pos rows cols)
  (cons (random rows) (random cols)))

;; verifica si pos ya está en la lista
(define (pos-member? pos lst)
  (cond [(null? lst) #f]
        [(equal? pos (car lst)) #t]
        [else (pos-member? pos (cdr lst))]))

;; genera n posiciones únicas recursivamente SIN let
(define (unique-random-positions n rows cols)
  (define (loop acc)
    (cond [(= (length acc) n) acc]
          [else
           (loop (if (pos-member? (random-pos rows cols) acc)
                     acc
                     (cons (random-pos rows cols) acc)))]))
  (loop '()))

;; coloca bombas recursivamente en la grilla
(define (place-bombs grid positions)
  (cond [(null? positions) grid]
        [else
         (grid-set
          (place-bombs grid (cdr positions))
          (car (car positions))
          (cdr (car positions))
          1)]))

;; crea la grilla con num-bombs bombas
(define (make-bomb-grid rows cols num-bombs)
  (place-bombs (make-grid rows cols)
               (unique-random-positions num-bombs rows cols)))

;; ============================================
;; Export
;; ============================================
(provide make-row make-grid grid-ref grid-set make-bomb-grid)
