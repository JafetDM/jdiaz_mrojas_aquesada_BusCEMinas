;; LÓGICA FUNCIONAL DEL BUSCAMINAS
#lang racket

;;                         MATRIZ BASE

;; Construye una fila de n ceros.
(define (make-row n)
  (cond [(= n 0) '()]
        [else (cons 0 (make-row (- n 1)))]))

;; Construye una matriz rows x cols de ceros.
(define (make-grid rows cols)
  (cond [(= rows 0) '()]
        [else (cons (make-row cols)
                    (make-grid (- rows 1) cols))]))

;; Matriz con un valor arbitrario v.
(define (make-row-val n v)
  (cond [(= n 0) '()]
        [else (cons v (make-row-val (- n 1) v))]))

(define (make-grid-val rows cols v)
  (cond [(= rows 0) '()]
        [else (cons (make-row-val cols v)
                    (make-grid-val (- rows 1) cols v))]))

;; Acceso "seguro" por índice sobre listas usando recursión.
;; (Aquí asumimos índices válidos, como en el resto del código.)
(define (list-ref-safe lst i)
  (cond [(zero? i) (car lst)]
        [else (list-ref-safe (cdr lst) (- i 1))]))

;; Acceso a una celda (r,c) de una matriz (lista de filas).
(define (grid-ref grid row col)
  (list-ref-safe (list-ref-safe grid row) col))

;; Reemplaza el elemento idx por new-val en una lista (devuelve nueva lista).
(define (replace-nth lst idx new-val)
  (cond [(null? lst) '()]                 
        [(zero? idx) (cons new-val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (- idx 1) new-val))]))

;; Reemplaza la celda (row,col) por val en la matriz (copia estructural).
(define (grid-set grid row col val)
  (replace-nth grid row
               (replace-nth (list-ref-safe grid row) col val)))

;;                       UTILIDADES

;; Longitud de lista.
(define (list-length xs)
  (cond [(null? xs) 0]
        [else (+ 1 (list-length (cdr xs)))]))

;; Filas/columnas de una matriz.
(define (grid-rows g) (list-length g))
(define (grid-cols g) (if (null? g) 0 (list-length (car g))))

;; Reverse funcional (acumulador).
(define (reverse-list xs)
  (define (go x acc)
    (if (null? x) acc (go (cdr x) (cons (car x) acc))))
  (go xs '()))

;; Chequeo de límites para coordenadas (r,c).
(define (in-bounds? rows cols r c)
  (and (<= 0 r) (< r rows) (<= 0 c) (< c cols)))

;; Vecinos en 8 direcciones (hasta 8 celdas).
;; Retorna lista de pares (r . c) válidos.
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

;;                         BOMBAS ALEATORIAS

;; Coordenada aleatoria válida (r . c).
(define (random-pos rows cols)
  (cons (random rows) (random cols)))

;; ¿Está la coordenada pos en la lista lst?
(define (pos-member? pos lst)
  (cond [(null? lst) #f]
        [(equal? pos (car lst)) #t]
        [else (pos-member? pos (cdr lst))]))

;; Genera n posiciones aleatorias ÚNICAS en el tablero.
;; Estrategia: ir acumulando sin repetir (si choca, reintenta).
(define (unique-random-positions n rows cols)
  (define (loop acc)
    (cond [(= (list-length acc) n) acc]
          [else
           (define p (random-pos rows cols))
           (loop (if (pos-member? p acc) acc (cons p acc)))]))
  (loop '()))

;; Coloca bombas (1) en las posiciones indicadas sobre una matriz base.
(define (place-bombs grid positions)
  (cond [(null? positions) grid]
        [else
         (define pr (car (car positions)))
         (define pc (cdr (car positions)))
         (grid-set (place-bombs grid (cdr positions)) pr pc 1)]))

;; Crea la matriz de bombas (0 = no bomba, 1 = bomba).
(define (make-bomb-grid rows cols num-bombs)
  (place-bombs (make-grid rows cols)
               (unique-random-positions num-bombs rows cols)))

;;                          CONTEOS ADYACENTES

;; Cuenta bombas alrededor de (r,c) usando neighbors.
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

;; A partir de la matriz de bombas, construye la matriz de CONTEOS.
;; Convención: 9 significa "bomba" (marcador interno para simplificar).
(define (counts-from-bombs bombs)
  (define rows (grid-rows bombs))
  (define cols (grid-cols bombs))
  ;; Construye una fila de conteos para la fila r.
  (define (build-row r c acc)
    (cond [(= c cols) (reverse-list acc)]
          [else
           (define v (grid-ref bombs r c))
           (define cell (if (= v 1) 9 (count-bombs-around bombs r c)))
           (build-row r (+ c 1) (cons cell acc))]))
  ;; Recorre todas las filas acumulando.
  (define (build-grid r acc)
    (cond [(= r rows) (reverse-list acc)]
          [else (build-grid (+ r 1) (cons (build-row r 0 '()) acc))]))
  (build-grid 0 '()))

;;                           TABLERO

;; Estructura del tablero: lista de 4 matrices (bombs, counts, revealed, marked).
(define (make-board* bombs counts revealed marked)
  (list bombs counts revealed marked))

;; Crea un tablero nuevo: genera bombas y conteos; revealed/marked en #f.
(define (make-board rows cols num-bombs)
  (define bombs (make-bomb-grid rows cols num-bombs))
  (define counts (counts-from-bombs bombs))
  (define revealed (make-grid-val rows cols #f))  ;; nada revelado al inicio
  (define marked  (make-grid-val rows cols #f))   ;; sin banderas al inicio
  (make-board* bombs counts revealed marked))

;; Accesores (posición fija dentro de la lista).
(define (board-bombs   board) (list-ref-safe board 0))
(define (board-counts  board) (list-ref-safe board 1))
(define (board-revealed board) (list-ref-safe board 2))
(define (board-marked   board) (list-ref-safe board 3))

;; Predicados/consultas de casilla.
(define (bomb-at? board r c)
  (= (grid-ref (board-bombs board) r c) 1))

(define (count-at board r c)
  (grid-ref (board-counts board) r c))

(define (revealed? board r c)
  (grid-ref (board-revealed board) r c))

(define (marked? board r c)
  (grid-ref (board-marked board) r c))

;; "Setters" funcionales: devuelven un board nuevo con el cambio aplicado.
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

;;                           EXPANSIÓN DE CEROS

;; Revela una celda y, si es 0, expande tipo "isla de ceros".
;; NOTA: si es bomba, no explota aquí; la GUI decide la derrota.
(define (reveal-at board r c)
  (define rows (grid-rows (board-bombs board)))
  (define cols (grid-cols (board-bombs board)))
  (cond
    [(not (in-bounds? rows cols r c)) board]  ;; fuera del tablero
    [(marked? board r c) board]               ;; con bandera, no se revela
    [(revealed? board r c) board]             ;; ya estaba revelada
    [(bomb-at? board r c) board]              ;; bomba (la GUI maneja perder)
    [else
     ;; revelamos la celda y, si es >0, paramos; si es 0, propagamos DFS
     (define b1 (set-revealed board r c #t))
     (define cnt (count-at b1 r c))
     (if (> cnt 0)
         b1
         (reveal-dfs-list
          b1
          (neighbors rows cols r c)
          ;; matriz visit para no re-visitar (marcamos origen como #t)
          (grid-set (make-grid-val rows cols #f) r c #t)))])) 

;; Paso recursivo de la expansión: visita (r,c) si aplica.
(define (reveal-dfs board r c visit)
  (define rows (grid-rows (board-bombs board)))
  (define cols (grid-cols (board-bombs board)))
  (cond
    [(not (in-bounds? rows cols r c)) board] ;; fuera
    [(grid-ref visit r c) board]             ;; ya visitado en esta expansión
    [(marked? board r c) board]              ;; no pisamos banderas
    [(revealed? board r c) board]            ;; ya revelado
    [(bomb-at? board r c) board]             ;; no expandir sobre bomba
    [else
     (define visit2 (grid-set visit r c #t)) ;; marcar visitado
     (define b1 (set-revealed board r c #t)) ;; revelar actual
     (define cnt (count-at b1 r c))
     (if (= cnt 0)
         ;; si también es 0, seguimos expandiendo por sus vecinos
         (reveal-dfs-list b1 (neighbors rows cols r c) visit2)
         ;; si >0, aquí paramos (frontera de números)
         b1)]))

;; Itera la DFS sobre una lista de vecinos pendientes.
(define (reveal-dfs-list board lst visit)
  (cond [(null? lst) board]
        [else
         (define rr (car (car lst)))
         (define cc (cdr (car lst)))
         (reveal-dfs-list (reveal-dfs board rr cc visit) (cdr lst) visit)]))

;;                             VICTORIA

;; Gana si TODAS las celdas SIN bomba están reveladas.
(define (victory? board)
  (define bombs (board-bombs board))
  (define rev   (board-revealed board))
  ;; Recorre la fila r validando desde la columna c.
  (define (row-ok? r c)
    (cond [(= c (grid-cols bombs)) #t]
          [else
           (define is-bomb (= (grid-ref bombs r c) 1))
           (define is-rev (grid-ref rev r c))
           (if is-bomb (row-ok? r (+ c 1)) (and is-rev (row-ok? r (+ c 1))))]))
  ;; Recorre todas las filas.
  (define (all-ok? r)
    (cond [(= r (grid-rows bombs)) #t]
          [else (and (row-ok? r 0) (all-ok? (+ r 1)))]))
  (all-ok? 0))

;;                             DERROTA 

;; Para mostrar el "game over": revela todas las bombas (aunque tuvieran bandera).
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

;;                             PRIMER CLICK SEGURO 

;; Reconstruye el tablero hasta que la celda inicial NO sea bomba y valga 0.
(define (make-board-safe-first rows cols num-bombs r c)
  (define (try-build)
    (define b (make-board rows cols num-bombs))
    (cond
      [(bomb-at? b r c) (try-build)]       ;; si cayó bomba, rehacer
      [(> (count-at b r c) 0) (try-build)] ;; si no es 0, rehacer
      [else b]))
  (try-build))


;; Expone funciones útiles para la interfaz.
(provide
  ;; construcción
  make-bomb-grid counts-from-bombs
  make-board make-board-safe-first
  ;; consultas
  bomb-at? count-at revealed? marked? victory?
  ;; acciones
  reveal-at set-revealed set-marked
  reveal-all-bombs)
