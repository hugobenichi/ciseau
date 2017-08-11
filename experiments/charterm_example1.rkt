#! /usr/bin/env racket

#lang racket/base

(require data/collection)
(require charterm)


; utilities

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

; charterm utilities and wrappers

(define (get-size)
  (let-values ([(w h) (charterm-screen-size)])
    (list w h)))

(define (display-line l)
  (charterm-display l "\r\n"))

(define (display-lines lines)
  (for-each display-line lines))

(define (display-n-lines n lines)
  (when (> n 0)
    (when (not (empty? lines))
      (display-line (first lines))
      (display-n-lines (dec n) (rest lines)))))

(define (display-lines-fitting-screen lines)
  (display-n-lines (second (get-size)) lines))


; examples and tests

(define (example1)
  (with-charterm
   (charterm-clear-screen)
   (charterm-cursor 10 5)
   (charterm-display "Hello, ")
   (charterm-bold)
   (charterm-display "you")
   (charterm-normal)
   (charterm-display ".")
   (charterm-cursor 1 1)
   (charterm-display "Press a key...")
   (let ((key (charterm-read-key)))
     (charterm-cursor 1 1)
     (charterm-clear-line)
     (printf "You pressed: ~S\r\n" key))))

(define verylong
    "thisisaverylonglinethisisaverylonglinethisisaverylonglinethisisaverylonglinethisisaverylonglinethisisaverylonglinethisisaverylonglinethisisaverylongline")

(define (test-lines)
  (with-charterm
    (display-lines (list verylong "foo" "bar" "baz"))))

(define (print-file p)
  ; CONTINUE: file->lines should work !
  (let ((data (file->lines p)))
    (with-charterm
      (display-lines-fitting-screen data))))


#| roadmap to mvp editor
    done:
      get keyboard input
      print keyboard input
      write a loop that put input on screen
    todo:
      read a file, then put on screen
      create a struct that hold term size and is created at each 
      cursor can move around
      implement scrolling
      write file
      basic navigation: by word, by paragraph
      implement keyword syntax highlightning for Java ?
      add Java parser that knows about structure
        add semantic highlights for variables nesting
        add semantic moves by functions
 |#


; a simple loop that just print the key pressed
(define (loop)
  (let ((key (charterm-read-key)))
    (charterm-clear-screen)
    (charterm-display "You pressed: " key "\r\n")
    (when (not (equal? 'ctrl-c key))
      (loop))))

(define (print-input)
  (with-charterm (loop)))

; a loop that print all key pressed, most recent up
(define (loop2 key_history)
  (define key (charterm-read-key))
  (define new_key_history (conj key_history key))
  (charterm-clear-screen)
  (display-lines-fitting-screen new_key_history)
  (when (not (equal? 'ctrl-c key))
    (loop2 new_key_history)))

(define (print-input2)
  (with-charterm
    (loop2 '())))

(define (main)
  ;(example1))
  ;(test-lines))
  ;(print-input))
  ;(print-input2))
  (print-file "./charterm_example1.rkt"))

(main)
