#|
  This file is a part of cl-to-html project.
|#

(in-package :cl-user)
(defpackage cl-to-html-test-asd
  (:use :cl :asdf))
(in-package :cl-to-html-test-asd)

(defsystem cl-to-html-test
  :depends-on (:cl-to-html :lisp-unit)
  :components ((:module "t"
                :components
                ((:file "cl-to-html"))))
  :description "Test system for cl-to-html"
  :perform (test-op (op c)
                    (eval (read-from-string "(cl-to-html-test:test-all)"))))
