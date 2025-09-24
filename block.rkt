; This file is part of Scheme+

;; Copyright 2021 - 2024 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;; code from r6rs version

(module block racket/base

  (provide $>
	   $+>
	   begin-def)

    
(define-syntax $>
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))

;; scheme@(guile-user)> (def bar ($+> (declare x y) {x <- 1} {y <- 2} (lambda (t) (+ x y t))))
;;
;; (def bar ($+> (declare x y)
;;               {x <- 1} 
;;               {y <- 2}
;;               (lambda (t) (+ x y t))))
;;
;; scheme@(guile-user)> (bar 7)
;; $2 = 10
(define-syntax $+>
  (syntax-rules ()
    ((_ ev)  (let () ev)) ;;  there can be a <+ in it expanding with a 'define'
    ((_ ev ...) (let () ev ...))))

(define-syntax begin-def
  (syntax-rules ()
    ((_ ev ...) ($+> ev ...))))


;; then and else do as BEGINners ;-)
(define-syntax then-block
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))

(define-syntax else-block
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))


) ; end library
