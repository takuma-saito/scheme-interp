

;; amb評価機を実装する

(use util.standard)

;; 失敗継続を保存する
(define *fail* #f)

;; 失敗継続を呼び出す
(define (call-fail-cont)
  (if (not *fail*) (*fail*)))

(define-macro (amb . body)
  (with-gensyms
   (cont)
   `(if (null? ',body) ,call-fail-cont
        (call/cc
         (lambda (,cont)
           (let ((fail0 *fail*))
             (set! *fail*
                   (lambda ()
                     (set! *fail* fail0)
                     (,cont (amb (cdr body)))))
             (,cont ,(car body))))))))

(macroexpand `(amb 1 2 3 4 5))
(amb 1 2 3 4 5)
(define body '(1 2 3 4 5))

