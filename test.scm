
(use test-suite)
(use env)

(test-section "env")

(define env (make-env))

(test* "a => 10" 10
       (bind! env 'a 10)
       (get env 'a))

(test* "name => env" 'env
       (bind! env 'name 'env)
       (get! env 'name))

(test* "exists? name => #t" #t
       (exists? env 'name))

(test* "exists? test => #f" #f
       (exists? env 'test))

(test* "frame-pop => #f" #f
       (frame-pop))

(test* "update! => new-name" 'new-env
       (update! env 'name 'new-env))

(test* "frame-grow!" 'new-name
       (frame-grow!)
       (lookup env 'name))

(test* "char => a" 'a
       (bind! env 'char 'a)
       (get env 'char))

(test* "lookup-frame" #t
       (equal? (lookup-frame env 'name) (last (ref env 'env))))

(test* "lookup-top-level" #t
       (lookup-top-level env 'name))

(test* "env" 2
       (length (ref env 'env)))

(test* "name => new-name2" 'new-name2
       (update! env 'name 'new-name2)
       (get env 'name))

(test* "bind-foreach" ('b 'c 'e #f)
       (bind-foreach env `((a d) (b e) (c f)))
       (list (get env 'a) (get env 'b) (get env 'c) (get env 'd)))

(test* "frame-clear!" (1 #f)
       (frame-clear!)
       (list (length (ref env 'env)) (get env 'a)))

(test-end)

(test-section "")

(test-result)
