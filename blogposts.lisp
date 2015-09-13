;;; A personal blog engine main file
(defpackage :piserv.blogposts
  (:use :piserv.blog))

(in-package :piserv.blogposts)

(defblogpost 3649655845 "Blog created"
  (:div
   (:p "For the past years I tired of repeating myself in every conversation on certain
   subjects, so I'll just place my thoughts and reasoning here and provide links to
   everyone who cares.")
   (:p "Therefore there won't be any comment engine — all discussions (if any) are to go
   in places where the link is placed (e.g. facebook).")
   (:p "As usual everything's written in Common Lisp and can be seen at my "
       (:a :href "https://github.com/rayslava" "GitHub profile") ".")
   (:p "Also if you know what RSS is you might be interested in subscribing "
       (:a :href "/rss" "the feed") "."))
  :tags '("en" "site" "news" "lisp")
  :meta  ((:meta :property "og:title" :content "Blog created")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "A note about blog creation")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3649655845")))

(defblogpost 3649996316 "Сделал блог"
  (:div
   (:p "Соорудил бложик, в котором буду записывать свои мысли по разным поводам. Просто
   надоело по нескольку раз писать одно и то же, когда в интернете кто-то неправ, поэтому
   запишу здесь свою позицию по всяким вопросам, вызывающим споры в интернетах, со всеми
   своими аргументами и буду давать ссылку.")
   (:p "По этой же причине здесь не будут прикручены комментарии — все обсуждения (если
   таковые возникнут) будут проходить в тех местах, куда я буду давать ссылку, скажем, в
   том же facebook. Из дополнительных фич — некоторые посты (как этот, например) буду
   оформлять с тегами \"en\" и \"ru\" и оставлять "
       (:a :href "/blog?id=3649655845" "кросс-линки") ", вдруг меня когда-то прочитает
  кто-нибудь, не знающий русского :)")
   (:p "С технической стороны всё точно так же написано на Common Lisp и выложено на "
       (:a :href "https://github.com/rayslava" "GitHub") ".")
   (:p "Для особых людей, знающих, что такое RSS, сделана " (:a :href "/rss" "лента, на
   которую можно подписаться.")))
  :tags '("ru" "site" "news" "lisp")
  :meta  ((:meta :property "og:title" :content "Сделал блог")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "Заметка о том, что у меня теперь есть бложик")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3649996316")))
