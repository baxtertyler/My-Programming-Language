#!/usr/bin/env racket
#lang typed/racket
 
(require "main.rkt")
 
(define dont-care
  (file-interp (vector-ref (current-command-line-arguments) 0)))
