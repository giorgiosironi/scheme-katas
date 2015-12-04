(load "../test-manager/load.scm")

(define (sub-list a b)
  (cond ((and (null? a) (null? b))
         'equal)
        ((null? a)
         'sublist)
        ((null? b)
         'superlist)
        (else
         (if (eq? (car a)
                  (car b))
             'equal
             'unequal))))

(in-test-group
     sublist-examples
     (define-test (equal-result)
                  "Two empty lists"
                  (check (eq? 'equal
                              (sub-list '()
                                        '())))
                  (check (eq? 'equal
                              (sub-list '(1)
                                        '(1)))))
     (define-test (sublist-result)
                  "?"
                  (check (eq? 'sublist
                              (sub-list '()
                                        '(1)))))
     (define-test (superlist-result)
                  "?"
                  (check (eq? 'superlist
                              (sub-list '(1)
                                        '()))))
     (define-test (superlist-result)
                  "?"
                  (check (eq? 'unequal
                              (sub-list '(1)
                                        '(2))))))

(run-registered-tests)


