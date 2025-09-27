;; LÓGICA DE LA INTERFAZ DEL BUSCAMINAS
#lang racket
(require racket/gui/base)
(require "logic.rkt")

;; Parámetros visuales
(define CELL 40)          ; tamaño de cada celda del tablero en píxeles
(define CFG-W 500)        ; ancho de la ventana de inicio (canvas)
(define CFG-H 380)        ; alto  de la ventana de inicio (canvas)

;; Ventana de configuración (100% canvas)
(define config-frame
  (new frame%
       [label "Menú Pricipal"]
       [width CFG-W]
       [height CFG-H]))

;; Estado de configuración en boxes
(define cfg-rows-box (box 8))             ; número de filas seleccionado
(define cfg-cols-box (box 8))             ; número de columnas seleccionado
(define cfg-diff-idx-box (box 0))         ; índice de dificultad: 0=Fácil,1=Intermedio,2=Difícil
(define cfg-diff-list '("Fácil" "Intermedio" "Difícil")) ; nombres para mostrar
(define cfg-hit-rects-box (box '()))      ; hitboxes de los “botones” dibujados en el canvas

;; Convierte el nombre de dificultad en porcentaje de bombas
(define (difficulty->pct s)
  (cond [(string=? s "Fácil") 0.10]
        [(string=? s "Intermedio") 0.15]
        [else 0.20]))

;; Utilidades canvas configuración
(define (clamp v lo hi)
  (cond [(< v lo) lo]
        [(> v hi) hi]
        [else v]))

;; ¿(x,y) cae dentro del rectángulo (rx,ry,rw,rh)?
(define (in-rect? x y rx ry rw rh)
  (and (>= x rx) (>= y ry) (< x (+ rx rw)) (< y (+ ry rh))))

;; Para no repetir “dibujar botón estilo flat”
(define BTN-FONT (make-object font% 14 'modern 'normal 'bold))

(define (draw-button dc x y w h label enabled?)
  (send dc set-font (make-object font% 12 'modern 'normal 'bold)) ; antes 14
  (send dc set-pen   (if enabled? "dimgray"   "lightgray") 1 'solid)
  (send dc set-brush (if enabled? "gainsboro" "whitesmoke") 'solid)
  (send dc draw-rectangle x y w h)
  (send dc set-text-foreground (if enabled? "black" "gray"))
  (define-values (tw th _dx _dy) (send dc get-text-extent label))
  (define tx (+ x (quotient (- w tw) 2)))
  (define ty (+ y (quotient (- h th) 2)))
  (send dc draw-text label tx ty))

;; Declarada aquí para usarla desde el menú
(define start-game #f)

;;                  Canvas de configuración
;; Dibuja todo el menú de inicio y maneja sus “botones” por coordenadas.

(define config-canvas
  (new (class canvas%
         (super-new)

         ;; Dibujo del menú
         (define/override (on-paint)
           (define dc (send this get-dc))
           ;; fondo liso blanco
           (send dc set-brush "white" 'solid)
           (send dc set-pen "white" 1 'solid)
           (send dc draw-rectangle 0 0 CFG-W CFG-H)

           ;; título
           (send dc set-font (make-object font% 28 'modern 'normal 'bold))
           (send dc set-text-foreground "midnight blue")
           (define title "BusCE Minas")
           (define-values (tw th _dx _dy) (send dc get-text-extent title))
           (define tx (quotient (- CFG-W tw) 2))
           (define ty 24)
           (send dc draw-text title tx ty)

           ;; subtítulo
           (send dc set-font (make-object font% 11 'modern))
           (send dc set-text-foreground "gray")
           (define sub "elige tamaño y dificultad")
           (define-values (sw sh _dx2 _dy2) (send dc get-text-extent sub))
           (send dc draw-text sub (quotient (- CFG-W sw) 2) (+ ty th 6))

           ;; secciones: filas, columnas, dificultad
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (send dc set-text-foreground "black")

           ;;                   FILAS
           (define sec-x 60)
           (define sec-y 120)
           (send dc draw-text "Filas (8–15)" sec-x sec-y)
           ;; botones - / valor / +
           (define r-val (unbox cfg-rows-box))
           (define rx-minus (+ sec-x 0))
           (define ry-minus (+ sec-y 24))
           (draw-button dc rx-minus ry-minus 36 30 "−" (> r-val 8))
           ;; número mostrado
           (send dc set-font (make-object font% 16 'modern 'normal 'bold))
           (define r-box-x (+ rx-minus 44))
           (define r-box-y ry-minus)
           (draw-button dc r-box-x r-box-y 56 30 (number->string r-val) #t)
           ;; botón +
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (define rx-plus (+ r-box-x 64))
           (define ry-plus ry-minus)
           (draw-button dc rx-plus ry-plus 36 30 "+" (< r-val 15))

           ;;                   COLUMNAS
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (define c-sec-x 300)
           (define c-sec-y 120)
           (send dc draw-text "Columnas (8–15)" c-sec-x c-sec-y)
           (define c-val (unbox cfg-cols-box))
           (define cx-minus (+ c-sec-x 0))
           (define cy-minus (+ c-sec-y 24))
           (draw-button dc cx-minus cy-minus 36 30 "−" (> c-val 8))
           (send dc set-font (make-object font% 16 'modern 'normal 'bold))
           (define c-box-x (+ cx-minus 44))
           (define c-box-y cy-minus)
           (draw-button dc c-box-x c-box-y 56 30 (number->string c-val) #t)
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (define cx-plus (+ c-box-x 64))
           (define cy-plus cy-minus)
           (draw-button dc cx-plus cy-plus 36 30 "+" (< c-val 15))

           ;;                   DIFICULTAD 
           (define d-sec-x 60)
           (define d-sec-y 200)
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (send dc draw-text "Dificultad" d-sec-x d-sec-y)
           (define d-val (list-ref cfg-diff-list (unbox cfg-diff-idx-box)))
           (define d-box-x (+ d-sec-x 0))
           (define d-box-y (+ d-sec-y 24))
           (draw-button dc d-box-x d-box-y 180 30 d-val #t)
           ;; pista visual debajo
           (send dc set-font (make-object font% 10 'modern))
           (send dc set-text-foreground "gray")
           (send dc draw-text "Toca para alternar" (+ d-box-x 24) (+ d-box-y 36))

           ;;                   BOTÓN INICIAR
           (define start-x 300)
           (define start-y 210)
           (send dc set-font (make-object font% 14 'modern 'normal 'bold))
           (draw-button dc start-x start-y 130 44 "Iniciar" #t)

           ;; Guardamos las “hitboxes” para detección de clic en on-event
           (set-box! cfg-hit-rects-box
                    (list (cons 'r- (list rx-minus ry-minus 36 30))
                          (cons 'r+ (list rx-plus  ry-plus  36 30))
                          (cons 'c- (list cx-minus cy-minus 36 30))
                          (cons 'c+ (list cx-plus  cy-plus  36 30))
                          (cons 'd  (list d-box-x  d-box-y  180 30))
                          (cons 'go (list start-x  start-y  130 44)))))

         ;; Manejo de clics en el menú (hit-testing contra rectángulos)
         (define/override (on-event e)
           (when (send e button-down?)
             (define x (send e get-x))
             (define y (send e get-y))
             (define hit (unbox cfg-hit-rects-box))

             ;; helper: ¿el clic cae dentro del rect asociado a 'sym?
             (define (hit? sym)
               (define rect (assoc sym hit))
               (and rect
                    (in-rect? x y
                              (list-ref (cdr rect) 0)
                              (list-ref (cdr rect) 1)
                              (list-ref (cdr rect) 2)
                              (list-ref (cdr rect) 3))))

             (cond
               ;; filas -
               [(hit? 'r-)
                (define v (unbox cfg-rows-box))
                (set-box! cfg-rows-box (clamp (- v 1) 8 15))
                (send this refresh)]
               ;; filas +
               [(hit? 'r+)
                (define v (unbox cfg-rows-box))
                (set-box! cfg-rows-box (clamp (+ v 1) 8 15))
                (send this refresh)]
               ;; cols -
               [(hit? 'c-)
                (define v (unbox cfg-cols-box))
                (set-box! cfg-cols-box (clamp (- v 1) 8 15))
                (send this refresh)]
               ;; cols +
               [(hit? 'c+)
                (define v (unbox cfg-cols-box))
                (set-box! cfg-cols-box (clamp (+ v 1) 8 15))
                (send this refresh)]
               ;; dificultad (0 → 1 → 2 → 0)
               [(hit? 'd)
                (define i (unbox cfg-diff-idx-box))
                (set-box! cfg-diff-idx-box (modulo (+ i 1) 3))
                (send this refresh)]
               ;; INICIAR: calcula #bombas según dificultad y abre el juego
               [(hit? 'go)
                (define rows (unbox cfg-rows-box))
                (define cols (unbox cfg-cols-box))
                (define diff (list-ref cfg-diff-list (unbox cfg-diff-idx-box)))
                (define pct (difficulty->pct diff))
                (define total (* rows cols))
                (define n-redondeado (inexact->exact (round (* pct total))))
                (define num-bombs (max 1 (min (- total 1) n-redondeado)))
                (send config-frame show #f)
                (start-game rows cols num-bombs diff)]
               [else (void)])))) 
       [parent config-frame]
       [min-width CFG-W]
       [min-height CFG-H]))

;; Mostramos el menú de inicio apenas arranca el programa
(send config-frame show #t)


;;                  Ventana principal (canvas% juego)

(set! start-game
  (lambda (rows cols num-bombs dificultad-label)
    (define board-box (box #f))     ; tablero (se crea en el primer clic seguro)
    (define game-over-box (box #f)) ; bandera para deshabilitar interacción al terminar

    (define frame
      (new frame%
           [label (format "Busca Minas (~a x ~a, ~a) – CELDA ~apx"
                          rows cols dificultad-label CELL)]
           [width (+ (* CELL cols) 50)]
           [height (+ (* CELL rows) 170)]))

    ;; Panel principal centrando el canvas del juego
    (define main-panel (new vertical-panel% [parent frame] [alignment '(center top)] [horiz-margin 20] [vert-margin 10]))
    (define top-panel  (new horizontal-panel% [parent main-panel]))

    ;; “Botón” Menú dibujado como canvas para que coincida el estilo con la pantalla de inicio
    (define menu-btn-w 100)
    (define menu-btn-h 36)

    (define menu-btn
      (new
      (class canvas%
        (super-new)
        (define/override (on-paint)
          (define dc (send this get-dc))
          (draw-button dc 0 0 menu-btn-w menu-btn-h "Menú" #t))
        (define/override (on-event e)
          (when (send e button-down?)
            (send frame show #f)
            (send config-frame show #t))))
      [parent top-panel]
      [min-width menu-btn-w]
      [min-height menu-btn-h]
      [stretchable-width #f]
      [stretchable-height #f]))

    ;; diálogo de fin (gana/pierde) con botones “Reiniciar” y “Volver al menú”
    (define (show-game-over-dialog titulo mensaje)
      (set-box! game-over-box #t)

      ;; tamaños y layout del diálogo (mensaje grande + botones compactos)
      (define btn-restart-w 95)
      (define btn-menu-w    148)
      (define btn-h         28)
      (define spacing       15)
      (define total-width (+ btn-restart-w btn-menu-w spacing))

      ;; ventana dimensionada para que quepa el texto grande
      (define dialog-w (max 420 (+ total-width 80)))
      (define dialog-h 260)

      (define dlg
        (new dialog%
            [label titulo]
            [parent frame]
            [width  dialog-w]
            [height dialog-h]))

      ;; columna principal centrada
      (define v (new vertical-panel%
                    [parent dlg]
                    [alignment '(center center)]
                    [horiz-margin 0]
                    [vert-margin 12]))

      ;; mensaje en canvas para ajustar tamaño de fuente al ancho disponible
      (define msg-w (- dialog-w 20)) ; margen lateral
      (define msg-h 120)             ; alto del área del texto
      (new
      (class canvas%
        (super-new)
        (define/override (on-paint)
          (define dc (send this get-dc))
          ;; probamos 15/18/16 para ajustar al ancho disponible
          (define fsize 15)
          (define font (make-object font% fsize 'modern 'normal 'bold))
          (send dc set-font font)
          (define-values (tw th _dx _dy) (send dc get-text-extent mensaje))
          (define font2 (make-object font% 18 'modern 'normal 'bold))
          (define-values (tw2 th2 _dx2 _dy2) (begin (send dc set-font font2) (send dc get-text-extent mensaje)))
          (define font3 (make-object font% 16 'modern 'normal 'bold))
          (define-values (tw3 th3 _dx3 _dy3) (begin (send dc set-font font3) (send dc get-text-extent mensaje)))
          (define use-font
            (cond
              [(<= tw (- msg-w 10)) font]
              [(<= tw2 (- msg-w 10)) font2]
              [else font3]))
          (send dc set-font use-font)
          (send dc set-text-foreground
            (if (or (string=? titulo "Ganaste!")
                    (string=? mensaje "Felicidades por no explotar :)"))
                "forestgreen"
                "tomato"))
          (define-values (tfinal-w tfinal-h _dx4 _dy4) (send dc get-text-extent mensaje))
          (define tx (max 0 (quotient (- (send this get-width)  tfinal-w) 2)))
          (define ty (max 0 (quotient (- (send this get-height) tfinal-h) 2)))
          (send dc draw-text mensaje tx ty)))
      [parent v]
      [min-width msg-w]
      [min-height msg-h]
      [stretchable-width #f]
      [stretchable-height #f])

      ;; contenedor centrado para la fila de botones
      (define center-wrap
        (new horizontal-panel%
            [parent v]
            [alignment '(center center)]
            [stretchable-width #f] [stretchable-height #f]))

      (define btns
        (new horizontal-panel%
            [parent center-wrap]
            [alignment '(center center)]
            [spacing spacing]
            [stretchable-width #f] [stretchable-height #f]))

      ;; Botón Reiniciar 
      (new
      (class canvas%
        (super-new)
        (define/override (on-paint)
          (define dc (send this get-dc))
          (draw-button dc 0 0 btn-restart-w btn-h "Reiniciar" #t))
        (define/override (on-event e)
          (when (send e button-down?)
            (send dlg show #f)
            (send frame show #f)
            (start-game rows cols num-bombs dificultad-label))))
      [parent btns]
      [min-width btn-restart-w] [min-height btn-h]
      [stretchable-width #f] [stretchable-height #f])

      ;; Botón Volver al menú
      (new
      (class canvas%
        (super-new)
        (define/override (on-paint)
          (define dc (send this get-dc))
          (draw-button dc 0 0 btn-menu-w btn-h "Volver al menú" #t))
        (define/override (on-event e)
          (when (send e button-down?)
            (send dlg show #f)
            (send frame show #f)
            (send config-frame show #t))))
      [parent btns]
      [min-width btn-menu-w] [min-height btn-h]
      [stretchable-width #f] [stretchable-height #f])

      (send dlg show #t))



    ;;                  CANVAS DEL JUEGO
    (define canvas
      (new
       (class canvas%
         (super-new)

         ;; Dibujo del tablero y contenido de cada celda
         (define/override (on-paint)
           (define dc (send this get-dc))
           ;; fondo general del área jugable
           (send dc set-brush "white" 'solid)
           (send dc set-pen "white" 1 'solid)
           (send dc draw-rectangle 0 0 (* CELL cols) (* CELL rows))

           (define (draw-cells r c)
             (cond
               [(= r rows) (void)]
               [(= c cols) (draw-cells (+ r 1) 0)]
               [else
                (define bb (unbox board-box))
                (define x (* c CELL))
                (define y (* r CELL))
                (define rev? (and bb (revealed? bb r c)))
                (define bomb? (and bb (bomb-at? bb r c)))
                (define v (if (and rev? (not bomb?)) (count-at bb r c) -1))

                ;; fondo de la celda:
                ;; - oculta:        gainsboro
                ;; - revelada 0:    silver   (más oscuro para diferenciar “isla”)
                ;; - revelada >0 ó 💣: lightgray
                (send dc set-pen "gray" 1 'solid)
                (send dc set-brush
                      (cond
                        [(and rev? (not bomb?) (= v 0)) "silver"]
                        [rev? "lightgray"]
                        [else "gainsboro"])
                      'solid)
                (send dc draw-rectangle x y CELL CELL)

                ;; contenido (prioridad: bomba revelada > bandera > número)
                (cond
                  [(and rev? bomb?)
                   (send dc set-text-foreground "black")
                   (send dc draw-text "💣" (+ x 8) (+ y 6))]
                  [(and bb (marked? bb r c))
                   (send dc set-text-foreground "tomato")
                   (send dc draw-text "🚩" (+ x 8) (+ y 6))]
                  [(and rev? (not bomb?))
                   (if (= v 0)
                       (void) ; los 0 no se dibujan, solo el fondo más oscuro
                       (begin
                         (send dc set-text-foreground
                               (cond [(= v 1) "blue"]
                                     [(= v 2) "forestgreen"]
                                     [(= v 3) "red"]
                                     [(= v 4) "purple"]
                                     [(= v 5) "maroon"]
                                     [(= v 6) "teal"]
                                     [else "black"]))
                         (send dc draw-text (number->string v) (+ x 12) (+ y 8))))]

                  [else (void)])

                ;; borde de la celda
                (send dc set-pen "dimgray" 1 'solid)
                (send dc set-brush "transparent" 'transparent)
                (send dc draw-rectangle x y CELL CELL)

                (draw-cells r (+ c 1))]))
           (draw-cells 0 0))

         ;; Click izquierdo = revelar, derecho = bandera.
         ;; Primer clic: se construye un tablero con “isla” inicial segura (conteo 0).
         (define/override (on-event e)
           (define et (send e get-event-type)) ; 'left-down o 'right-down
           (when (and (not (unbox game-over-box))
                      (or (eq? et 'left-down) (eq? et 'right-down)))
             (define mx (send e get-x))
             (define my (send e get-y))
             (define c (quotient mx CELL))
             (define r (quotient my CELL))
             (cond
               [(or (< r 0) (>= r rows) (< c 0) (>= c cols)) (void)]
               [else
                (cond
                  ;; Clic DERECHO: alterna bandera (si ya existe tablero y la celda no está revelada)
                  [(eq? et 'right-down)
                   (cond
                     [(not (unbox board-box)) (void)]
                     [else
                      (define board (unbox board-box))
                      (cond
                        [(revealed? board r c) (void)]
                        [else
                         (define new-board (set-marked board r c (not (marked? board r c))))
                         (set-box! board-box new-board)
                         (send this refresh)])])]

                  ;; Clic IZQUIERDO: revelar (construye tablero seguro si es el primer clic)
                  [else
                   (cond
                     [(not (unbox board-box))
                      (set-box! board-box (make-board-safe-first rows cols num-bombs r c))])
                   (define board (unbox board-box))
                   (cond
                     [(marked? board r c) (void)] ; no revelamos si tiene bandera
                     [(bomb-at? board r c)
                      ;; Perder: revelamos todas las bombas, refrescamos y mostramos diálogo
                      (define exploded (reveal-all-bombs (set-revealed board r c #t)))
                      (set-box! board-box exploded)
                      (send this refresh)
                      (show-game-over-dialog "Fin del juego" "Nooo, explotaste 💥")]
                     [else
                      ;; Revelar normal (con posible propagación de ceros)
                      (define new-board (reveal-at board r c))
                      (set-box! board-box new-board)
                      (send this refresh)
                      ;; Chequear victoria y, si aplica, revelar bombas y bloquear
                      (when (victory? new-board)
                        (define revealed-final (reveal-all-bombs new-board))
                        (set-box! board-box revealed-final)
                        (send this refresh)
                        (show-game-over-dialog "Ganaste!" "Felicidades por no explotar :)"))])])]))))
       ;; propiedades del canvas (fuera del class)
       [parent main-panel]
       [min-width (* CELL cols)]
       [min-height (* CELL rows)]
       [stretchable-width #f]
       [stretchable-height #f]))

    ;; Mostrar la ventana de juego
    (send frame show #t)))
