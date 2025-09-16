#lang racket
(require racket/gui/base)
(require "logic.rkt") ;; lógica funcional con make-bomb-grid y grid-ref

;; =============================================
;; Ventana de configuración
;; =============================================
(define config-frame
  (new frame%
       [label "Configuración Busca Minas"]
       [width 350]
       [height 250]))

(define config-panel (new vertical-panel% [parent config-frame]))

;; Filas
(new message% [parent config-panel] [label "Número de filas (8-15):"])
(define filas-field (new text-field% [parent config-panel] [label "8"]))

;; Columnas
(new message% [parent config-panel] [label "Número de columnas (8-15):"])
(define cols-field (new text-field% [parent config-panel] [label "8"]))

;; Dificultad con choice%
(new message% [parent config-panel] [label "Dificultad:"])
(define difficulty-choice
  (new choice%
       [parent config-panel]
       [label "Selecciona dificultad:"] ; obligatorio en choice%
       [choices '("Fácil" "Intermedio" "Difícil")]
       [selection 0])) ; por defecto "Fácil"

;; =============================================
;; Botón Iniciar
;; =============================================
(new button%
     [parent config-panel]
     [label "Iniciar Juego"]
     [callback
      (lambda (btn evt)
        (define n-filas (string->number (send filas-field get-value)))
        (define n-cols (string->number (send cols-field get-value)))
        (if (or (not n-filas) (not n-cols)
                (< n-filas 8) (> n-filas 15)
                (< n-cols 8) (> n-cols 15))
            (message-box "Error" "Debes ingresar valores entre 8 y 15.")
            (let* ([selected (send difficulty-choice get-string-selection)]
                   [num-bombs (cond [(string=? selected "Fácil") 5]
                                    [(string=? selected "Intermedio") 10]
                                    [else 15])])
              (send config-frame show #f)
              (start-game n-filas n-cols num-bombs))))])

;; Mostrar ventana de configuración
(send config-frame show #t)

;; =============================================
;; Función que abre la ventana principal del juego
;; =============================================
(define (start-game rows cols num-bombs)
  ;; Crear la grilla con bombas usando lógica funcional
  (define grid (make-bomb-grid rows cols num-bombs))

  ;; Crear ventana del juego
  (define frame (new frame%
                     [label (format "Busca Minas (~ax~a)" rows cols)]
                     [width (+ (* 30 cols) 50)]
                     [height (+ (* 30 rows) 100)]))
  (define main-panel (new vertical-panel% [parent frame]))

  ;; Botón Reiniciar arriba
  (define top-panel (new horizontal-panel% [parent main-panel]))
  (new button%
       [parent top-panel]
       [label "Reiniciar"]
       [callback
        (lambda (btn evt)
          (send frame show #f) ;; cerrar ventana del juego
          (send config-frame show #t))]) ;; volver a ventana de configuración

  ;; Panel de la cuadrícula
  (define grid-panel (new vertical-panel% [parent main-panel]))

  ;; Crear la cuadrícula con botones
  (for ([r rows])
    (define row-panel (new horizontal-panel% [parent grid-panel]))
    (for ([c cols])
      (new button%
           [parent row-panel]
           [label " "] ;; inicialmente vacío
           [min-width 30]
           [min-height 30]
           [callback
            (lambda (b e)
              ;; Mostrar valor de la grilla (0 o 1)
              (let ([val (grid-ref grid r c)])
                (send b set-label (number->string val))
                (displayln (format "Clic en (~a,~a): ~a" r c val))))])))

  ;; Mostrar ventana del juego
  (send frame show #t))
