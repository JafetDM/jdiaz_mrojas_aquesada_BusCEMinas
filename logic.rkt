#lang racket

(require racket/random) ; para random

;; ============================================
;; LÓGICA FUNCIONAL DEL BUSCA MINAS
;; ============================================

(define (make-row n)
  (cond [(= n 0) '()]
        [else (cons 0 (make-row (- n 1)))]))

(define (make-grid rows cols)
  (cond [(= rows 0) '()]
        [else (cons (make-row cols)
                    (make-grid (- rows 1) cols))]))

(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col))

(define (replace-nth lst idx new-val)
  (cond [(null? lst) '()]
        [(zero? idx) (cons new-val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (- idx 1) new-val))]))

(define (grid-set grid row col val)
  (replace-nth grid row
               (replace-nth (list-ref grid row) col val)))

;; ============================================
;; COLOCACIÓN DE BOMBAS ALEATORIAS
;; ============================================

;; genera una coordenada aleatoria (row col)
(define (random-pos rows cols)
  (list (random rows) (random cols)))

;; verifica si una coordenada ya está en la lista
(define (pos-member? pos lst)
  (cond [(null? lst) #f]
        [(equal? pos (car lst)) #t]
        [else (pos-member? pos (cdr lst))]))

;; genera n posiciones únicas
(define (unique-random-positions n rows cols)
  (define (loop acc)
    (cond [(= (length acc) n) acc]
          [else
           (let ([p (random-pos rows cols)])
             (if (pos-member? p acc)
                 (loop acc) ; ya estaba, seguir
                 (loop (cons p acc))))]))
  (loop '()))

;; coloca bombas en la grilla
(define (place-bombs grid positions)
  (cond [(null? positions) grid]
        [else
         (let* ([row (first (car positions))]
                [col (second (car positions))]
                [new-grid (grid-set grid row col 1)])
           (place-bombs new-grid (cdr positions)))]))

;; función principal: crea grilla con bombas
(define (make-bomb-grid rows cols num-bombs)
  (let* ([grid (make-grid rows cols)]
         [bomb-positions (unique-random-positions num-bombs rows cols)])
    (place-bombs grid bomb-positions)))

;; ============================================
;; EXPORT
;; ============================================
(provide make-row make-grid grid-ref grid-set
         make-bomb-grid)



