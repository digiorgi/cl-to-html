(in-package :cl-user)
(defpackage cl-to-html
  (:use :cl)
  (:export :cl-to-html
           :cl-to-html-stream
           :+html5-doctype+
           :+no-self-close+))
(in-package :cl-to-html)

(defvar +html5-doctype+ "<!DOCTYPE html>")
(defvar +no-self-close+ "")

(defun tagp (tag)
  (typep tag 'KEYWORD))

(defun list-start-with-keywordp (list)
  (and (listp list)
       (typep (car list) 'KEYWORD)))

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
           (format stream " ~a='~a'" (string-downcase (string att)) value)
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

(defun add-list-call (list-of-lists)
  "Add the #'LIST function call to every list starting with a keyword. With
this there is no problem to write (:something 'like this')."
  (if (list-start-with-keywordp list-of-lists)
      (cons 'list (loop for i in list-of-lists
                     collecting (add-list-call i)))
      list-of-lists))

(defmacro add-list-call-macro (&body body)
  "Transforms the body to only one list, and every sublist if start with a
keyword get transformed too."
  (cons 'list (loop for item in body collecting (add-list-call item))))

(defun translate (body &optional stream)
  (typecase body
    (STRING     (write-string body stream))
    (LIST       (write-html body stream))
    (OTHERWISE  (format stream "~a" body))))

(defmacro cl-to-html-stream (stream &body body)
  (let ((iterator (gensym "ITERATOR"))
        (cl-html  (gensym "CL-HTML")))
    `(let ((,cl-html (add-list-call-macro ,@body)))
       (loop for ,iterator in ,cl-html do
            (translate ,iterator ,stream)))))

(defmacro cl-to-html (&body body)
  (let ((stream   (gensym "STREAM")))
    `(let ((,stream (make-string-output-stream)))
       (cl-to-html-stream ,stream ,@body)
       (get-output-stream-string ,stream))))
