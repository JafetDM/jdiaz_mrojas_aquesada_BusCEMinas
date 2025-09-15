#lang racket/gui
(require "logic.rkt")

;; ============= Interfaz gráfica =============
(define rows 5)
(define cols 5)

;; Creamos la cuadrícula lógica inicial
(define grid (make-grid rows cols))

;; Creamos la ventana principal
(define frame (new frame%
                   [label "Busca Minas"]
                   [width 400]
                   [height 400]))

;; Panel para la cuadrícula
(define grid-panel (new horizontal-panel% [parent frame]))

;; Usamos nested panels para simular la matriz de botones
(for ([r rows])
  (define row-panel (new vertical-panel% [parent grid-panel]))
  (for ([c cols])
    (new button%
         [parent row-panel]
         [label " "]
         [callback
          (lambda (btn evt)
            (send btn set-label (format "~a,~a" r c)))])))

(send frame show #t)

