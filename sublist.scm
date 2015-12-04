(load "../test-manager/load.scm")


(define (is-head-of x y)
  (cond ((null? x)
         #t)
        ((null? y)
         #f)
        (else 
          (and (eq? (car x)
                    (car y))
               (is-head-of (cdr x)
                           (cdr y))))))

(define (contained x y)
  (cond ((null? x)
         #t)
        ((null? y)
         #f)
        ((is-head-of x
                     y)
         #t)
        (else (contained x
                         (cdr y)))))

(define (sub-list a b)
  (let ((contained-a-b (contained a b))
        (contained-b-a (contained b a)))
    (cond ((and contained-a-b contained-b-a)
           'equal)
          ((and contained-a-b (not contained-b-a))
           'sublist)
          ((and contained-b-a (not contained-a-b))
           'superlist)
          (else
            'unequal))))

(in-test-group
     contained-examples
     (define-test (true)
                  "x is contained in y"
                  (check (eq? #t
                              (contained '() '())))
                  (check (eq? #t
                              (contained '() '(1))))
                  (check (eq? #t
                              (contained '(1) '(1 2))))
                  (check (eq? #t
                              (contained '(2) '(1 2))))
                  (check (eq? #t
                              (contained '(1 2) '(1 3 1 2)))))

     (define-test (false)
                  "x is not contained in y"
                  (check (eq? #f
                              (contained '(1) '())))
                  (check (eq? #f
                              (contained '(1 2) '(1 3)))))
                  (check (eq? #f
                              (contained '(1 2) '(1 3 2)))))

(in-test-group
     sublist-examples
     (define-test (equal-result)
                  "Two empty lists"
                  (check (eq? 'equal
                              (sub-list '()
                                        '())))
                  (check (eq? 'equal
                              (sub-list '(1)
                                        '(1))))
                  (check (eq? 'equal
                              (sub-list '(1 2)
                                        '(1 2)))))
     (define-test (sublist-result)
                  "?"
                  (check (eq? 'sublist
                              (sub-list '()
                                        '(1))))
                  (check (eq? 'sublist
                              (sub-list '(2)
                                        '(1 2)))))
     (define-test (superlist-result)
                  "?"
                  (check (eq? 'superlist
                              (sub-list '(1)
                                        '())))
                  (check (eq? 'superlist
                              (sub-list '(1 2)
                                        '(2)))))
     (define-test (unequal-result)
                  "?"
                  (check (eq? 'unequal
                              (sub-list '(1)
                                        '(2)))))
                  (check (eq? 'unequal
                              (sub-list '(1 2)
                                        '(1 3)))))

(run-registered-tests)


