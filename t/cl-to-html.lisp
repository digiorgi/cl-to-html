(in-package :cl-user)
(defpackage cl-to-html-test
  (:use :cl :cl-to-html :lisp-unit
        :cl-to-html)
  (:export :test-all))
(in-package :cl-to-html-test)

(defparameter +test-string1+ "test-string1")
(defparameter +test-string2+ "test-string2")
(defparameter +href-keyword+ :href)


(define-test cl-to-html-test
  (assert-equalp "<a att1='att1'>body</a>"
                 (cl-to-html (:a :att1 "att1" "body")))
  (assert-equalp "3"
                 (cl-to-html (+ 1 2)))
  (assert-equalp "<a href='test-string1'>"
                 (cl-to-html (:a :href +test-string1+)))
  (assert-equalp "<a href='test-string1'><p>body</p></a>"
                 (cl-to-html (:a +href-keyword+ +test-string1+
                                 (:p "body"))))
  (assert-equalp "<!DOCTYPE html><strong>I'm strong</strong>"
                 (cl-to-html +html5-doctype+ (:strong "I'm strong")))
  (assert-equalp (concatenate 'string
                              "<ul><li>You saw an XXX-FILM</li>"
                              "<li>I saw an YYY-FILM</li></ul>")
                 (let ((x "XXX-FILM") (y "YYY-FILM"))
                   (cl-to-html (:ul (:li "You saw an " x)
                                    (:li "I saw an " y)))))
  (assert-equalp "<a href='/home/'>Hello world</a>"
                 (let ((variable nil))
                   (cl-to-html (:a (if variable :br :href) "/home/"
                                   "Hello world"))))
  (assert-equalp "<script src='foo'>"
                 (cl-to-html (:script :src "foo")))
  (assert-equalp "<script src='foo'></script>"
                 (cl-to-html (:script :src "foo" +no-self-close+))))

(defun test-all ()
  (lisp-unit:remove-tests)
  (setq lisp-unit:*print-failures* t)
  (asdf:load-system :cl-to-html-test :force t)
  (let ((test (lisp-unit:run-tests :all :cl-to-html-test)))
    (when test (print-errors test))))
