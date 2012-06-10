
(define-module debug
  (export p))

(use utils)

;; デバック用
(define-method p ((lis <list>))
  (map (lambda (elem) (p elem)) lis))

(define-macro (define-proxy name)
  `(define-method p ((,name ,(+ "<" (+ name ">")))) ,name))

(define-proxy number)
(define-proxy string)
(define-proxy symbol)
