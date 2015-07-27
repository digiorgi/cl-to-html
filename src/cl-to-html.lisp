(in-package :cl-user)
(defpackage cl-to-html
  (:use :cl)
  (:export :cl-to-html
           :cl-to-html-stream
           :+html5-doctype+))
(in-package :cl-to-html)

(defvar +html5-doctype+ "<!DOCTYPE html>")
(defvar *result*)

(defun tagp (tag)
  (typep tag 'KEYWORD))

(defun open-start-tag (tag &optional stream)
  (format stream "<~a" (string-downcase (string tag))))

(defun close-start-tag (&optional stream)
  (format stream ">"))

(defun close-end-tag (tag &optional stream)
  (format stream "</~a>" (string-downcase (string tag))))

(defun write-attributes (body &optional stream)
  "Writes the attributes and the values to the STREAM and returns the rest of
the body that doesn't have a pair of attribute and value."
  (loop for cons on body by #'cddr
     for att   = (car  cons)
     for value = (or (cadr cons) "")
     do
       (if (tagp att)
           (format stream " ~a='~a'" (keyword->tag att) value)
           (return cons))))

(defun write-body (body &optional stream)
  (loop for value in body do (translate value stream)))

(defun write-html (body &optional stream)
  (let ((tag (car body))
        (tag-attributes-body (cdr body)))
    (when tag
      (open-start-tag tag stream)
      (let ((tag-body (write-attributes tag-attributes-body stream)))
        (close-start-tag stream)
        (when tag-body
          (write-body tag-body stream)
          (close-end-tag tag stream))))))

(defun eval-evaluable (x)
  (if (or
       ;;IS A FUNCTIONA
       (and (listp x) (not (typep (car x) 'KEYWORD)))
       ;;IS A VARIABLE
       (typep x 'SYMBOL))
      (eval x)
      x))

(defun eval-list (list)
  (mapcar #'eval-evaluable list))

(defun translate (body &optional stream)
  (typecase body
    (STRING     (write-string body stream))
    (LIST       (write-html (eval-list body) stream))
    (OTHERWISE  (format stream "~a" body))))

(defmacro cl-to-html-stream (stream &body body)
  (let ((iterator (gensym "ITERATOR")))
    `(loop for ,iterator in (eval-list (quote ,body)) do
          (translate ,iterator ,stream))))

(defmacro cl-to-html (&body body)
  (let ((stream   (gensym "STREAM")))
    `(let ((,stream (make-string-output-stream)))
       (cl-to-html-stream ,stream ,@body)
       (get-output-stream-string ,stream))))
