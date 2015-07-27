#|
  This file is a part of cl-to-html project.
|#

(in-package :cl-user)
(defpackage cl-to-html-asd
  (:use :cl :asdf))
(in-package :cl-to-html-asd)

(defsystem cl-to-html
  :version "0.1"
  :author "Di Giorgi, Hernan Ezequiel>"
  :license "MIT LICENSE"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-to-html"))))
  :description "Transforms CL expressions to HTML"
  :in-order-to ((test-op (test-op cl-to-html-test))))
