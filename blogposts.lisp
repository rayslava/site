;;; A personal blog engine main file
(defpackage :piserv.blogposts
  (:use :piserv.blog))

(in-package :piserv.blogposts)

(defblogpost 3649655845 "On blog creation"
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
  :meta  ((:meta :property "og:title" :content "On blog creation")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "A note about blog creation")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3649655845")))

(defblogpost 3649996316 "О блоге"
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
  :meta  ((:meta :property "og:title" :content "О блоге")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "Заметка о том, что у меня теперь
	  есть бложик")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3649996316")))

(defblogpost 3653209732 "О веб-дизайне"
  (:div
   (:p "Глядя на современный веб мне становится грустно и печально, иногда  вплоть до
   негодования: как мы могли из практически свободного сообщества инженеров скатиться в
   то, что можно видеть сейчас – тотальную централизацию, засилье рекламы, абсолютную
   компьютерную безграмотность пользователей.")
   (:p "Впрочем, про это написано уже достаточно много статей и я не хочу повторяться,
   этот пост про то, что я вижу на веб-страницах, как пользователь.")
   (:p "Для начала замечу, что основным моим браузером течение рабочей недели является "
       (:a :href "http://www.emacswiki.org/emacs/emacs-w3m" "emacs-w3m") ", и лишь когда не
   удается что-то открыть в нем, с расстройством и матюгами запускаю firefox.")
   (:p "Так вот последние годы я наблюдаю, как веб превращается в набор приложений,
   написанных на javascript, с html5-интерфейсом, которые используют браузер, как средство
   доставки. И это отвратительно.")
   (:p "Я еще могу понять, когда корпорации, нацеленные на выкачивание денег из населения,
   занимаются тем, что рисуют финтифлюшки и пытаются загнать как можно больше народу на
   свои площадки, используя веселую ферму и фильтры для фотографий, превращающие
   плохонькие картинки в настолько страшную мешанину цветов, что Мунк обзавидуется. Но
   когда я пытаюсь открыть " (:strong "новостной сайт") ", чтобы посмотреть
   одну-единственную новость и не могу этого сделать, потому что страница весит 3.5
   мегабайта, требует поддержку cookie, javascript и еще какого-нибудь флеша, чтобы просто
   отобразить три абзаца текста — я искренне и безмерно удивляюсь. Кому и зачем это
   надо?")
   (:p "Похоже, что массы убили почти всю суть веба, которая, как и все остальное
   держалась на идеалистах, твердо соблюдающих идеи Тима Бернерса-Ли.")
   (:p "В общем, мое сугубое мнение — работа должна выполняться на серверах, анимация не
   нужна, js вреден, cookie можно выдавать в тех местах, где
   пользователю " (:strong "необходимо авторизоваться") ", а не на каждом сайте просто для
   того, чтобы насовать каждому как можно больше разной рекламы. Плюс, я иду в сеть за
   информацией, а не за подачей, поэтому я все еще не совсем понимаю, для чего люди вешают
   на свои страницы фоновые картинки по мегабайту и делают переливающиеся анимации по
   углам. Остались, конечно, хорошие сайты, и ярчайший пример – Википедия. Абсолютно
   простой прямой дизайн, информация представлена в том виде, в котором человек ожидает ее
   получить, не нужно ни javascript ни cookie, форматирование присутствует на минимально
   необходимом уровне, чтобы выделять реально важные места. Также отлично оформлены
   страницы проекта GNU.")
   (:p "Антипример — любая социальная сеть. Скажем facebook или, еще лучше, instagram.
   Последний без javascript и cookie не позволяет даже посмотреть изображение. А с
   поддержкой — не дает скопировать ссылку на это изображение. Приходится писать скрипты
   для парсинга. И все это густо обвешано жуткого вида баннерами, наполнено рекламными
   постами и ценность представляет чуть менее, чем никакую.")
   (:p "В целом веб-дизайн по моему мнению дизайн должен быть как у "
       (:a :href "https://gnu.org/" "GNU") " и "
       (:a :href "https://wikipedia.org" "Wikipedia")
   ": минималистичным, доносящим информацию, не зависящим от устройства и не делающим
   лишнюю работу, чего я и стараюсь придерживаться на этом сайте.")
   (:small "There is an " (:a :href "/blog?id=3653209733" "english version") " of this
   post"))
  :tags '("ru" "web" "design")
  :meta  ((:meta :property "og:title" :content "О веб-дизайне")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "Плач и стенания о том, как
	  веб-дизайн катится в бездну ужаса")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3653209732")))

(defblogpost 3653209733 "On web design"
  (:div
   (:p "When looking on modern web I become very sad and sometimes even frustrated. How
   could we make it from nearly free and independent community of engineers to the awful
   thing we see now in browsers?")
   (:p "Total centralization, advertisements everywhere, users don't have elementary
   computer usage knowledge.")
   (:p "However there are a lot of articles on this subject and I don't want to repeat
   them. This post is about content I see on web-pages as a user.")
   (:p "In the beginning I'll notice that my main web-browser is "
       (:a :href "http://www.emacswiki.org/emacs/emacs-w3m" "emacs-w3m") " and only if it
   can't handle something I launch firefox feeling very disappointed.")
   (:p "And during the last several years I see how web mutates into a bunch of
   applications written in js with html5 interface which use browser just as a delivery
   system. And this is disgusting.")
   (:p "I can understand when corporations targeted to getting money from users work on
   creating jingles and whistles, online farms and creating photo filters which convert
   not-so-good images into color mess so strange that Munch would envy badly. But when I
   try to open a " (:strong "news site") " and can't see just a news because it's 3.5 MB,
   requires js, cookie, flash and quad hd monitor just to show me three paragraphs of text
   — I'm really surprised. Who and why does need that?")
   (:p "It looks like masses kill the essence of web which was initially handled only by
   idealists who were standing strong on Tim Berners-Lee ideas.")
   (:p "So my opinion on web — the jobs must be done on server-side. Animation is not
   needed, js harms, cookies may only be used in places where user " (:strong "must") " be
   identified not everywhere just to spy the user and push him lots of new ads.")
   (:p "I go to the net to get information, please give me my portion of information, not
   mad designer fantasies. However there are still sites which are beautiful and one of
   the most noticeable — Wikipedia. The information is represented in a way user want see
   it, no need of js and cookies, formatting exists, but is very minimalistic and form
   doesn't dominate on contents. The other good example is GNU project pages.")
   (:p "Example of the opposite — any social network. Say facebook or even better —
   Instagram. The latter won't allow you to see the image without js and won't allow to
   copy image URL when you enable one. So the only way to see image is to write a
   parser. And all of that is full of awful banners, filled up with advertising posts and
   has nearly zero informational value.")
   (:p "So web design should be just like it's done at "
       (:a :href "https://gnu.org/" "GNU") " and "
       (:a :href "https://wikipedia.org" "Wikipedia") " sites: minimalistic, clean and
       don't do excessive things. And I'm trying to keep the site this way.")
  (:small "There is a " (:a :href "/blog?id=3653209732" "russian version") " of this
  post"))
  :tags '("en" "web" "design")
  :meta  ((:meta :property "og:title" :content "On web design")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "Mourning the web design and the
          whole web itself while looking on modern sites.")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3653209733")))

(defblogpost 3678124120 "On Doom"
  (:div
   (:p "I've finished
      the " (:a :href "https://store.steampowered.com/app/379720/" "Doom (2016)")
      " game lately and it triggered my Doom admiration again. Coincidentally I've
      got the "
       (:a :href "https://www.amazon.com/Masters-Doom-Created-Transformed-Culture/dp/0812972155"
	   "Masters of Doom") " book inside my Kindle and could read it as well nearly the same.")
   (:h3 "The Game")
   (:p "So to begin with the game: I liked it! Yeah, I mean " (:strong "liked") ". Not so new
   experience and genre-breaking gameplay but still exactly that slightly nervous feeling during
   pressing the buttons and good old friends starting right from the great big fat Mancubus. The two
   major things changed to classic Doom (and nearly everyone noted that) — the game became easier
   and the arenas appeared.")
   (:p "The first part is " (:a :href "https://www.youtube.com/watch?v=W1ZtBCpo0eU" "not surprising
   at all") ": since video games became the massively produced entertainment and gained the huge
   industry around they're becoming easier year to year and the difference
   between " (:em "Ultraviolence") " then and " (:em "Ultraviolence") " now can be seen by anyone
   without any efforts. But it looks like there's several levels above in the new Doom so there's
   possibly a place for training.")
   (:p "The second part is questionable: on the one hand arena is the Quake III attribute while Doom
   has always been the rather large world (divided into levels due to technical limitations
   sometimes) with place to retreat if monsters push you too hard; on the other hand these new
   arenas are planned well and the design is done great not only in terms of level design with
   paths, walls, jumpers and portals on their places but even from aesthetic point of view:
   textures, colors, sinners dangling around… However I'd still like good old levels with space for
   maneuver better.")
   (:p "Monsters are rebalanced greatly but the visual style is still recognizable: when you see a
   shotgun guy you know who he is and when you meet pinkie you'll even shout \"- Hey!\" but only
   until it rush you. From my point of view monsters are weaker now but deal more damage and you
   have to move, move and move around without pauses. The first stop usually means death for
   you. And again this game speed is more quake-like than doom, but still fun. And rebalance is
   quite irritating if you remember your feelings of the classical Doom. Now the first time you meet
   the Baron of Hell you'll " (:s "shit bricks") " be surprised how he beats you to death by two or
   three heavy punches but on the last levels he'll receive no more attention than zombies.
   Situation with the Imp is quite the opposite: in classical Doom to beat one or two or squad of
   them was not even a noticeable event; now they're one of the most dangerous enemies in the game
   especially due to numbers. There's lots of imps, they're everywhere, they run walls and ceilings
   and they shoot you with their fireballs right when you're going to retreat and find a little
   medicine.")
   (:p "Weapon set seems good to me and I don't really care much about fitting the classical set:
   Gauss gun seems strange and not very effective, but shotgun with burst is fine. Double-barrel
   shotgun is still the logo and the main device to use throughout the game, balance between "
   (:strong "power") " and ammo is beautiful. And of course there's the chainsaw. And of course
   it's the great thing!  Even greater you can imagine — it'll split nearly anyone into two (or
   more) pieces in a second but looking for fuel is the main quest of the game.")
   (:p "Summarizing: I definitely advice you to try the game if you still didn't do it and make your
   own opinion. At least killing hellish creatures with the shotgun is as fun as always!")
   (:hr)
   (:h3 "The Book")
   (:p "And two words about the book: " (:strong "must read") ". Yep. These two ones. David Kushner
   did a great job collecting all the information and gathering it into the book. The most exciting
   thing is all the history from the book just happened in front of us. Russian video game world had
   been late by couple of years but the situation in the video games industry was very alike.")
   (:p "From the professional point of view I marked out for myself the moments of initial id
   company creation when the programmers needed a manager who could deal with negotiation and
   barbecue supplying and situation with Ion Storm rise and fall: sad but usual story. The great
   professional is not always capable of organizing a team and running business even if he thinks he
   is. The team (or person) who constantly delivers (which is John Carmack is all about) always
   beats colossal but unimplemented plans. The great discoveries are based not only on genius on the
   dull meetings and everyday work.")
   (:p "Anyway the books get 5/5 stars from me and I do not regret a cent spent."))
  :tags '("en" "games" "dev" "thoughts" "books")
  :meta  ((:meta :property "og:title" :content "On Doom")
	  (:meta :property "og:type" :content "blog")
	  (:meta :property "og:description" :content "My personal opinion about the Doom game")
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3678124120")))
