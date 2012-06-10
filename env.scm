
(define-module env
  ;; *env* を公開するのはデバック用のため
  (export *env* frame-clear! frame-push! frame-pop! frame-top-level
          lookup lookup-top-level bind! bind-foreach))
(select-module env)

(use srfi-1)
(use srfi-11)
(use hash)

(define (make-frame) (make-hash))

(define (setup-frame)
  `(,(make-frame)))

(define *env* (setup-frame))

(define-method empty? ((frames <list>))
  (equal? frames '()))

(define-method pop ((frames <list>))
  (if (empty? frames) #f
      (car frames)))

(define (search-keyword frame key)
  (if (exists? #?=frame key)
      (get frame key)
      #f))

;; public method

(define (frame-push!)
  (set! *env* (cons (make-frame) *env*)))

(define (frame-pop)
  (if (equal? (cdr *env*) '()) #f
      (begin
        (set! *env* (cdr *env*))
        (car *env*))))

(define (frame-top-level)
  (last *env*))

(define (frame-clear!)
  (set! *env* (setup-frame)))

(define (lookup keyword)
  (define (lookup-inner frames key)
    (cond [(empty? frames) #f]
          [(pop frames) => (lambda (frame) (search-keyword frame keyword))]
          [else (lookup-inner (cdr frames) key)]))
  (lookup-inner *env* keyword))

(define (lookup-top-level keyword)
  (let ((frame (last *env*)))
    (if (exists? frame keyword) (get frame keyword) #f)))

(define (bind! key value)
  (put! (car *env*) key value))

(define (bind-foreach lis)
  (for-each (lambda (elem) (bind! (car elem) (cdr elem))) lis))
  