# CL-TO-HTML
Ultra simplistic Common Lisp to HTML translator. You have two main macros to
play with: ``cl-to-html`` that returns an string, and ``cl-to-html-stream``
that prints its output to a stream.

## Why other library?
Other libraries need constants, in the macros. For example in CL-WHO its hard
to write macros or function calls inside the html generation macros. But with
the CL-TO-HTML approach:
*  In compile time: Only the basic transformations to avoid compilation errors
are made.
*  In runtime the body of the macro is transformed to HTML.

This allows you to put any arbitrary code inside the ``cl-to-html``. **The only
requeriment is that any form subject to HTML transformation starts with a
keyword**.

## Usage
```lisp
> (use-pacakge cl-to-html)

> (cl-to-html (:a :b "as" (:div :class "class" "hi man")) "a")
"<a b='as'><div class='class'>hi man</div></a>a"

> (cl-to-html +html5-doctype+ (:strong "I'm strong"))
"<!DOCTYPE html><strong>I'm strong</strong>"

>(let ((x "XXX-FILM") (y "YYY-FILM"))
                   (cl-to-html (:ul (:li "You saw an " x)
                                    (:li "I saw an " y))))
"<ul><li>You saw an XXX-FILM</li><li>I saw an YYY-FILM</li></ul>"

>(let ((variable nil))
  (cl-to-html (:a (if variable :br :href) "/home/"
                  "Hello world")))
"<a href='/home/'>Hello world</a>"
```

## Pros & Cons
The pros is that is very flexible to write code inside the macros of this
library. The cons are that is slow in comparison of the other library that
expands only one time in compile time, but there you can put anything you want.
