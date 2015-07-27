# CL-TO-HTML
Ultra simplistic Common Lisp to HTML translator.

## Usage
```lisp
> (cl-to-html (:a :b "as" (:div :class "class" "hi man")) "a")
"<a b='as'><div class='class'>hi man</div></a>a"

> (cl-to-html +html5-doctype+ (:strong "I'm strong"
"<!DOCTYPE html><strong>I'm strong</strong>"
```

## Installation
