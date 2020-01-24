;; SPDX-License-Identifier: Apache-2.0
;; Copyright 2020 Flynn Liu

(define mode:off 0)
(define mode:summary 10)
(define mode:report-failed 20)
(define mode:report 30)

(define %check-mode mode:report)
(define %check-state 'not-started)
(define %check-correct 0)

(define (wrap= f)
  (let ((has-run #f))
    (lambda (a b)
      (define result (f a b))
      (unless has-run
        (set! has-run #t)
        (if result (set! %check-correct (+ 1 %check-correct)))
        )
      result
      )))

(define-syntax check
  (syntax-rules (=>)
    ((check <expr> (=> <equal>) <expected>)
     (begin
       (when (symbol=? 'not-started %check-state)
         (let ((old-mode %check-mode))
           (check-reset!)
           (set! %check-mode old-mode)))
       (test-equal (wrap= <equal>) <expected> <expr>)
       ))

    ((check <expr> => <expected>)
     (check <expr> (=> equal?) <expected>))
    ))

(define %builtin-reporter (current-test-group-reporter))
(define (%wrapped-reporter grp)
  (if (= %check-mode mode:off)
      (display "(turned off)\n")
      (%builtin-reporter grp)))

(define %builtin-applier (current-test-applier))
(define (%wrapped-applier expect expr info)
  (if (= %check-mode mode:off)
      ((current-test-skipper) info)
      (%builtin-applier expect expr info)))

(define (check-report)
  ((current-test-group-reporter) (current-test-group))
  (when (and (>= %check-mode mode:report-failed)
             (> (test-failure-count) 0))
    (newline)
    (error "** Test failed. See above **")))

(define (check-set-mode! mode)
  (define mode-number
    (case mode
     ((off)           mode:off)
     ((summary)       mode:summary)
     ((report-failed) mode:report-failed)
     ((report)        mode:report)
     (else (error "unknown mode"))
     ))
  (set! %check-mode mode-number))

(define (check-reset!)
  (set! %check-mode mode:report)
  (set! %check-correct 0)
  (if (symbol=? 'group-started %check-state)
      (test-end))
  (current-test-group-reporter %wrapped-reporter)
  (current-test-applier %wrapped-applier)
  (test-begin (car (command-line)))
  (test-failure-count 0)
  (set! %check-state 'group-started))

(define (check-passed? expected-total-count)
  (and (= (test-failure-count) 0)
       (= %check-correct expected-total-count)))
