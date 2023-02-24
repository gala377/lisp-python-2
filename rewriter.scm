(begin

  (define #f ())
  (define #t 'true)
  (define else 'else)
  (define caar (lambda (l) (car (car l))))
  (define cdar (lambda (l) (car (cdr l))))
  (define cddar (lambda (l) (car (cdr (cdr l)))))
  (define cdddar (lambda (l) (car (cdr (cdr (cdr l))))))
  (define cddr (lambda (l) (cdr (cdr l))))
  (define not (lambda (x) (cond (x #f) (else #t))))

  (define map 
    (lambda (func l)
      (cond
        ((nil? l) ())
        (else 
          (cons (func (car l)) (map func (cdr l)))))))

  (define rewrite-sexpr 
    (lambda (expr) 
      (cond
        ((list? expr) (rewrite-list expr))
        (else expr))))

  (define rewrite-list
    (lambda (expr)
      (cond 
        ((eq? (car expr) 'let) 
          (rewrite-let (cdar expr) (cddr expr)))
        ((eq? (car expr) 'if)
          (rewrite-if (cdar expr) (cddar expr) (cdddar expr)))
        ((eq? (car expr) 'define)
          (rewrite-define (cdar expr) (cddr expr)))
        ((eq? (car expr) 'and)
          (rewrite-and (cdr expr)))
        (else (map rewrite-sexpr expr)))))

  (define rewrite-let 
    (lambda (bindings body)
      (cons
        (list
          'lambda
          (map car bindings)
          (cons 'begin (rewrite-sexpr body)))
        (map rewrite-sexpr (map cdar bindings)))))

  (define rewrite-if
      (lambda (condition then else)
        (list
          'cond
          (list (rewrite-sexpr condition) (rewrite-sexpr then))
          (list 'else (rewrite-sexpr else)))))
  
  (define rewrite-define 
    (lambda (name rest)
      (cond
        ((list? name) 
          (list
            'define
            (car name)
            (list
              'lambda
              (cdr name)
              (cons 'begin (rewrite-sexpr rest)))))
        (else (cons 'define (cons name (rewrite-sexpr rest)))))))

  (define rewrite-and
    (lambda (expr)
      (cond 
        ((nil? expr) #t)
        (else 
          (list
            'cond
            (list (rewrite-sexpr (car expr)) (rewrite-and (cdr expr)))
            (list else #f))))))

  (define rewrite-or
    (lambda (expr)
      (cond
        ((nil? expr) #f)
        (else 
          (list 
            'cond
            (list (rewrite-sexpr (car expr)) #t)
            (list else (rewrite-or (cdr expr))))))))

  (define source-with-let 
    '(begin
      (define addition 
        (lambda (x y)
          (let ((ret (+ x y))) ret)))
      (define (equal x y) (if (eq? x y) "yes" "no"))
      
      (and 
        (let ((x 1)) (eq? x 1))
        (eq? 1 1)
        #f
        (nil? (map (lambda (x) (+ x 1)) '(1 2 2))))
  ))

  (print (rewrite-sexpr source-with-let))
  (eval (rewrite-sexpr source-with-let))
)