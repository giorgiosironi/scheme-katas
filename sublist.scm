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
                                        '(1 2))))
                  (check (eq? 'equal
                              (sub-list (iota 1000000 1 1)
                                        (iota 1000000 1 1)))))
     (define-test (sublist-result)
                  "?"
                  (check (eq? 'sublist
                              (sub-list '()
                                        '(1))))
                  (check (eq? 'sublist
                              (sub-list '()
                                        '(1 2))))
                  (check (eq? 'sublist
                              (sub-list '(2)
                                        '(1 2))))
                  (check (eq? 'sublist
                              (sub-list '(1 2 3)
                                        '(1 2 3 4 5))))
                  (check (eq? 'sublist
                              (sub-list '(4 3 2)
                                        '(5 4 3 2 1))))
                  (check (eq? 'sublist
                              (sub-list '(3 4 5)
                                        '(1 2 3 4 5))))
                  (check (eq? 'sublist
                              (sub-list '(1 1 2)
                                        '(1 1 1 2))))
                  (check (eq? 'sublist
                              (sub-list '(3 4 5)
                                        (iota 1000000 1 1))))
                  (check (eq? 'sublist
                              (sub-list '(1 2 1 2 3)
                                        '(1 2 3 1 2 1 2 3 2 1)))))
     (define-test (superlist-result)
                  "?"
                  (check (eq? 'superlist
                              (sub-list '(1)
                                        '())))
                  (check (eq? 'superlist
                              (sub-list '(1 2)
                                        '())))
                  (check (eq? 'superlist
                              (sub-list '(1 2)
                                        '(2))))
                  (check (eq? 'superlist
                              (sub-list '(1 2 3 4 5)
                                        '(1 2 3))))
                  (check (eq? 'superlist
                              (sub-list '(5 4 3 2 1)
                                        '(4 3 2))))
                  (check (eq? 'superlist
                              (sub-list '(1 2 3 4 5)
                                        '(3 4 5))))
                  (check (eq? 'superlist
                              (sub-list '(1 1 1 2)
                                        '(1 1 2))))
                  (check (eq? 'superlist
                              (sub-list (iota 1000000 1 1)
                                        '(3 4 5)))))
     (define-test (unequal-result)
                  "?"
                  (check (eq? 'unequal
                              (sub-list '(1)
                                        '(2)))))
                  (check (eq? 'unequal
                              (sub-list '(1 2)
                                        '(1 3))))
                  (check (eq? 'unequal
                              (sub-list (iota 1000001 10 1)
                                        (iota 1000000 1 1))))
                  (check (eq? 'unequal
                              (sub-list '(1 2 1 2 3)
                                        '(1 2 3 1 2 3 2 3 2 1)))))

(run-registered-tests)


