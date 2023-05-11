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
  (define pair (lambda (x y) (list x y)))
  (define fst (lambda (p) (car p)))
  (define snd (lambda (p) (cdar p)))

  (define map 
    (lambda (func l)
      (cond
        ((nil? l) ())
        (else 
          (cons (func (car l)) (map func (cdr l)))))))

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
        ((eq? (car expr) 'or)
          (rewrite-or (cdr expr)))
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
        ((nil? expr) '(quote true))
        (else 
          (list
            'cond
            (list (rewrite-sexpr (car expr)) (rewrite-and (cdr expr)))
            (list 'else ()))))))

  (define rewrite-or
    (lambda (expr)
      (cond
        ((nil? expr) ())
        (else 
          (list 
            'cond
            (list (rewrite-sexpr (car expr)) '(quote true))
            (list 'else (rewrite-or (cdr expr))))))))

  (define macros 
    (list
      (pair 'define rewrite-define)
      (pair 'let rewrite-let)
      (pair 'if rewrite-if)
      (pair 'and rewrite-and)
      (pair 'or rewrite-or)))

  (define rewrite-sexpr 
    (lambda (expr) 
      (cond
        ((list? expr) (rewrite-list expr))
        (else expr))))

  (define source-with-let 
    '(begin
      (or 
        (let ((x 1)) (eq? x 1))
        (eq? 1 1)
        #f
        (nil? (map (lambda (x) (+ x 1)) '(1 2 2))))
  ))

  (print (rewrite-sexpr source-with-let))
  (eval (rewrite-sexpr source-with-let))
)