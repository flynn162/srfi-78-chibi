;; -*- scheme -*-
;; SPDX-License-Identifier: Apache-2.0
;; Copyright 2020 Flynn Liu

(define-library (srfi 78)
  (import (scheme base) (scheme write)
          (scheme process-context)
          (chibi test)
          )
  (export check check-report check-set-mode! check-reset! check-passed?)
  (include "./78.scm"))
