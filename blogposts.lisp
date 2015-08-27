;;; A personal blog engine main file
(defpackage :piserv.blogposts
  (:use :piserv.blog))

(defblogpost 3649655845 "Blog created"
  (:div
   (:p "Okay, I've created a blog engline and will post something here.")
   (:p "As usual it's written in neat Common Lisp and can be seen at my "
       (:a :href "https://github.com/rayslava" "GitHub profile")))
  '("en" "site" "news" "lisp"))
