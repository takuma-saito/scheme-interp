
(add-load-path ".")
(use utils)
(use hash)

(+ "fuga-" 'hoge)
(rest '(1 2 3 4 5))
(first '(1 2 3 4 5))
(1- 5)

(define h (make-hash))
(define h2 (make-hash))
(d h)
(put! h 'name 'takuma)
(put! h 'height 165)
(put! h 'weight 65)
(update! h 'name (lambda (x) 'saito))
(delete! h 'height)
(exists? h 'height)
(map (lambda (key value) (list key value)) h2)

(use env)

(frame-pop!)
(bind! 'name 'takuma)

(use tag)
(define tag1 (make-tag :name :primitive :body (lambda (x) (* x x))))
(tag= tag1 :primitive)
(tag= tag1 :keyword)
(tag tag1)
((body tag1) 5)

(define-generic p)
;; closure
(define-method p ((self <closure>)) self)
