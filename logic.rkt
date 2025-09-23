#lang racket
;; =========================================================
;; LÓGICA FUNCIONAL DEL BUSCAMINAS
;; =========================================================

;; ---------- grilla base ----------
(define (make-row n)
  (cond [(= n 0) '()]
        [else (cons 0 (make-row (- n 1)))]))

(define (make-grid rows cols)
  (cond [(= rows 0) '()]
        [else (cons (make-row cols)
                    (make-grid (- rows 1) cols))]))

;; NUEVA: grilla con valor arbitrario (p.ej. #f)
(define (make-row-val n v)
  (cond [(= n 0) '()]
        [else (cons v (make-row-val (- n 1) v))]))

(define (make-grid-val rows cols v)
  (cond [(= rows 0) '()]
        [else (cons (make-row-val cols v)
                    (make-grid-val (- rows 1) cols v))]))

(define (list-ref-safe lst i)
  (cond [(zero? i) (car lst)]
        [else (list-ref-safe (cdr lst) (- i 1))]))

(define (grid-ref grid row col)
  (list-ref-safe (list-ref-safe grid row) col))

(define (replace-nth lst idx new-val)
  (cond [(null? lst) '()]
        [(zero? idx) (cons new-val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (- idx 1) new-val))]))

(define (grid-set grid row col val)
  (replace-nth grid row
               (replace-nth (list-ref-safe grid row) col val)))

;; ---------- utilidades ----------
(define (list-length xs)
  (cond [(null? xs) 0]
        [else (+ 1 (list-length (cdr xs)))]))

(define (grid-rows g) (list-length g))
(define (grid-cols g) (if (null? g) 0 (list-length (car g))))

(define (reverse-list xs)
  (define (go x acc)
    (if (null? x) acc (go (cdr x) (cons (car x) acc))))
  (go xs '()))

(define (in-bounds? rows cols r c)
  (and (<= 0 r) (< r rows) (<= 0 c) (< c cols)))

;; vecinos (8 direcciones)
(define (neighbors rows cols r c)
  (define (step k acc)
    (cond
      [(= k 8) acc]
      [else
       (define dr (list-ref-safe '(-1 -1 -1  0 0  1 1 1) k))
       (define dc (list-ref-safe '(-1  0  1 -1 1 -1 0 1) k))
       (define rr (+ r dr))
       (define cc (+ c dc))
       (step (+ k 1)
             (if (in-bounds? rows cols rr cc)
                 (cons (cons rr cc) acc)
                 acc))]))
  (step 0 '()))

;; ---------- bombas aleatorias ----------
(define (random-pos rows cols)
  (cons (random rows) (random cols)))

(define (pos-member? pos lst)
  (cond [(null? lst) #f]
        [(equal? pos (car lst)) #t]
        [else (pos-member? pos (cdr lst))]))

(define (unique-random-positions n rows cols)
  (define (loop acc)
    (cond [(= (list-length acc) n) acc]
          [else
           (define p (random-pos rows cols))
           (loop (if (pos-member? p acc) acc (cons p acc)))]))
  (loop '()))

(define (place-bombs grid positions)
  (cond [(null? positions) grid]
        [else
         (define pr (car (car positions)))
         (define pc (cdr (car positions)))
         (grid-set (place-bombs grid (cdr positions)) pr pc 1)]))

(define (make-bomb-grid rows cols num-bombs)
  (place-bombs (make-grid rows cols)
               (unique-random-positions num-bombs rows cols)))

;; ---------- conteos adyacentes ----------
(define (count-bombs-around bombs r c)
  (define rows (grid-rows bombs))
  (define cols (grid-cols bombs))
  (define ns (neighbors rows cols r c))
  (define (sum lst s)
    (cond [(null? lst) s]
          [else
           (define rr (car (car lst)))
           (define cc (cdr (car lst)))
           (sum (cdr lst) (+ s (grid-ref bombs rr cc)))]))
  (sum ns 0))

(define (counts-from-bombs bombs)
  (define rows (grid-rows bombs))
  (define cols (grid-cols bombs))
  (define (build-row r c acc)
    (cond [(= c cols) (reverse-list acc)]
          [else
           (define v (grid-ref bombs r c))
           (define cell (if (= v 1) 9 (count-bombs-around bombs r c)))
           (build-row r (+ c 1) (cons cell acc))]))
  (define (build-grid r acc)
    (cond [(= r rows) (reverse-list acc)]
          [else (build-grid (+ r 1) (cons (build-row r 0 '()) acc))]))
  (build-grid 0 '()))

;; ---------- board ----------
(define (make-board* bombs counts revealed marked)
  (list bombs counts revealed marked))

(define (make-board rows cols num-bombs)
  (define bombs (make-bomb-grid rows cols num-bombs))
  (define counts (counts-from-bombs bombs))
  (define revealed (make-grid-val rows cols #f))  
  (define marked  (make-grid-val rows cols #f)) 
  (make-board* bombs counts revealed marked))

(define (board-bombs   board) (list-ref-safe board 0))
(define (board-counts  board) (list-ref-safe board 1))
(define (board-revealed board) (list-ref-safe board 2))
(define (board-marked   board) (list-ref-safe board 3))

(define (bomb-at? board r c)
  (= (grid-ref (board-bombs board) r c) 1))

(define (count-at board r c)
  (grid-ref (board-counts board) r c))

(define (revealed? board r c)
  (grid-ref (board-revealed board) r c))

(define (marked? board r c)
  (grid-ref (board-marked board) r c))

(define (set-revealed board r c val)
  (make-board* (board-bombs board)
               (board-counts board)
               (grid-set (board-revealed board) r c val)
               (board-marked board)))

(define (set-marked board r c val)
  (make-board* (board-bombs board)
               (board-counts board)
               (board-revealed board)
               (grid-set (board-marked board) r c val)))

;; ---------- flood-fill ----------
(define (reveal-at board r c)
  (define rows (grid-rows (board-bombs board)))
  (define cols (grid-cols (board-bombs board)))
  (cond
    [(not (in-bounds? rows cols r c)) board]
    [(marked? board r c) board]
    [(revealed? board r c) board]
    [(bomb-at? board r c) board] ; derrota la decide la GUI
    [else
     (define b1 (set-revealed board r c #t))
     (define cnt (count-at b1 r c))
     (if (> cnt 0)
         b1
         (reveal-dfs-list
          b1
          (neighbors rows cols r c)
          (grid-set (make-grid-val rows cols #f) r c #t)))])) 

(define (reveal-dfs board r c visit)
  (define rows (grid-rows (board-bombs board)))
  (define cols (grid-cols (board-bombs board)))
  (cond
    [(not (in-bounds? rows cols r c)) board]
    [(grid-ref visit r c) board]
    [(marked? board r c) board]
    [(revealed? board r c) board]
    [(bomb-at? board r c) board]
    [else
     (define visit2 (grid-set visit r c #t))
     (define b1 (set-revealed board r c #t))
     (define cnt (count-at b1 r c))
     (if (= cnt 0)
         (reveal-dfs-list b1 (neighbors rows cols r c) visit2)
         b1)]))

(define (reveal-dfs-list board lst visit)
  (cond [(null? lst) board]
        [else
         (define rr (car (car lst)))
         (define cc (cdr (car lst)))
         (reveal-dfs-list (reveal-dfs board rr cc visit) (cdr lst) visit)]))

;; ---------- victoria ----------
(define (victory? board)
  (define bombs (board-bombs board))
  (define rev   (board-revealed board))
  (define (row-ok? r c)
    (cond [(= c (grid-cols bombs)) #t]
          [else
           (define is-bomb (= (grid-ref bombs r c) 1))
           (define is-rev (grid-ref rev r c))
           (if is-bomb (row-ok? r (+ c 1)) (and is-rev (row-ok? r (+ c 1))))]))
  (define (all-ok? r)
    (cond [(= r (grid-rows bombs)) #t]
          [else (and (row-ok? r 0) (all-ok? (+ r 1)))]))
  (all-ok? 0))

;; ---------- primer clic seguro (garantiza 0) ----------
(define (make-board-safe-first rows cols num-bombs r c)
  (define (try-build)
    (define b (make-board rows cols num-bombs))
    (cond
      [(bomb-at? b r c) (try-build)]
      [(> (count-at b r c) 0) (try-build)]
      [else b]))
  (try-build))

;; ---------- revelar todas las bombas (derrota) ----------
(define (reveal-all-bombs board)
  (define rows (grid-rows (board-bombs board)))
  (define cols (grid-cols (board-bombs board)))
  (define (loop r c b)
    (cond
      [(= r rows) b]
      [(= c cols) (loop (+ r 1) 0 b)]
      [else
       (define b2 (if (bomb-at? b r c) (set-revealed b r c #t) b))
       (loop r (+ c 1) b2)]))
  (loop 0 0 board))

;; ---------- export ----------
(provide
  ;; construcción
  make-bomb-grid counts-from-bombs
  make-board make-board-safe-first
  ;; consultas
  bomb-at? count-at revealed? marked? victory?
  ;; acciones
  reveal-at set-revealed set-marked
  reveal-all-bombs)
