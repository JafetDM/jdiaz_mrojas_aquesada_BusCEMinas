#lang racket
(require racket/gui/base)
(require "logic.rkt")

;; =============================================
;; Ventana de configuración
;; =============================================
(define config-frame
  (new frame%
       [label "Configuración Busca Minas"]
       [width 360]
       [height 270]))

(define config-panel (new vertical-panel% [parent config-frame]))

(new message% [parent config-panel] [label "Número de Filas"])
(define filas-field (new text-field% [parent config-panel] [label "Ingrese un número del 8 al 15: "]))

(new message% [parent config-panel] [label "Número de Columnas"])
(define cols-field (new text-field% [parent config-panel] [label "Ingrese un número del 8 al 15: "]))

(new message% [parent config-panel] [label "Dificultad:"])
(define difficulty-choice
  (new choice%
       [parent config-panel]
       [label "Selecciona dificultad:"]
       [choices '("Fácil" "Intermedio" "Difícil")]
       [selection 0]))

(define (difficulty->pct s)
  (cond [(string=? s "Fácil") 0.10]
        [(string=? s "Intermedio") 0.15]
        [else 0.20]))

;; =============================================
;; Auxiliar: iniciar con datos YA validados
;; =============================================
(define (start-with-valid-input n-filas n-cols)
  (define selected (send difficulty-choice get-string-selection))
  (define pct (difficulty->pct selected))
  (define total (* n-filas n-cols))
  (define n-redondeado (inexact->exact (round (* pct total))))
  (define num-bombs (max 1 (min (- total 1) n-redondeado)))
  (send config-frame show #f)
  (start-game n-filas n-cols num-bombs))

(new button%
     [parent config-panel]
     [label "Iniciar Juego"]
     [callback
      (lambda (btn evt)
        (define n-filas (string->number (send filas-field get-value)))
        (define n-cols  (string->number (send cols-field get-value)))
        (if (or (not n-filas) (not n-cols)
                (< n-filas 8) (> n-filas 15)
                (< n-cols 8)  (> n-cols 15))
            (message-box "Error" "Debes ingresar valores entre 8 y 15.")
            (start-with-valid-input n-filas n-cols)))])

(send config-frame show #t)

;; =============================================
;; Ventana principal del juego
;; =============================================
(define (start-game rows cols num-bombs)
  ;; board-box empieza #f: construimos el tablero al PRIMER click,
  ;; garantizando que esa casilla sea 0 (mini-isla inicial).
  (define board-box (box #f))

  (define frame (new frame%
                     [label (format "Busca Minas (~a x ~a)" rows cols)]
                     [width (+ (* 30 cols) 50)]
                     [height (+ (* 30 rows) 120)]))
  (define main-panel (new vertical-panel% [parent frame]))

  (define top-panel (new horizontal-panel% [parent main-panel]))
  (new button%
       [parent top-panel]
       [label "Reiniciar"]
       [callback
        (lambda (btn evt)
          (send frame show #f)
          (send config-frame show #t))])

  (define grid-panel (new vertical-panel% [parent main-panel]))

  ;; matriz de botones para poder refrescar tras flood-fill
  (define btns
  (for/list ([r (in-range rows)])
    (define row-panel (new horizontal-panel% [parent grid-panel]))
    (for/list ([c (in-range cols)])
      (new button%
           [parent row-panel]
           [label " "]
           [min-width 30]
           [min-height 30]
           [callback
            (lambda (b e)
              ;; construir tablero en primer click garantizando isla 0
              (cond
                [(not (unbox board-box))
                 (set-box! board-box (make-board-safe-first rows cols num-bombs r c))])
              (define board (unbox board-box))
              (cond
                ;; ==== CASO: BOMBA ====
                [(bomb-at? board r c)
                 ;; Revela la bomba tocada y todas las demás
                 (define exploded
                   (reveal-all-bombs (set-revealed board r c #t)))
                 (set-box! board-box exploded)
                 ;; refrescar toda la UI
                 (for ([rr (in-range rows)])
                   (for ([cc (in-range cols)])
                     (define bb (unbox board-box))
                     (define btn (list-ref (list-ref btns rr) cc))
                     (cond
                       [(revealed? bb rr cc)
                        (cond
                          [(bomb-at? bb rr cc) (send btn set-label "💣")]
                          [else
                           (define v (count-at bb rr cc))
                           (send btn set-label (if (= v 0) "." (number->string v)))])]
                       [else
                        (send btn set-label " ")])))
                 (message-box "Fin" "💥 Boom! Perdiste")]
                ;; ==== CASO: NO BOMBA ====
                [else
                 (define new-board (reveal-at board r c))
                 (set-box! board-box new-board)
                 ;; refrescar todas las etiquetas según 'revealed'
                 (for ([rr (in-range rows)])
                   (for ([cc (in-range cols)])
                     (define bb (unbox board-box))
                     (define btn (list-ref (list-ref btns rr) cc))
                     (cond
                       [(revealed? bb rr cc)
                        (cond
                          [(bomb-at? bb rr cc) (send btn set-label "💣")]
                          [else
                           (define v (count-at bb rr cc))
                           (send btn set-label (if (= v 0) "." (number->string v)))])]
                       [else
                        (send btn set-label " ")])))]))]))))


  (send frame show #t))
