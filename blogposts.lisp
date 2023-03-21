;;; A personal blog engine main file
;;; Post id is `(local-time:timestamp-to-universal (local-time:now))'
;;;
;;; (local-time:reread-timezone-repository)
;;; (local-time:timestamp-to-universal (local-time:encode-timestamp 0 0 00 10 21 12 2020 :timezone (local-time:find-timezone-by-location-name "Europe/Moscow")))

(defpackage :site.blogposts
  (:use :site.blog :cl :cl-css :cl-who :site.activitypub))

(in-package :site.blogposts)

;;; Perform a cleanup unless we want duplication
(setf site.blog::*blog-posts* nil)

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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "A note about blog creation")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3649655845")))

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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Заметка о том, что у меня теперь
	  есть бложик")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3649996316")))

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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Плач и стенания о том, как
	  веб-дизайн катится в бездну ужаса")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3653209732")))

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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Mourning the web design and the
          whole web itself while looking on modern sites.")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3653209733")))

(defblogpost 3678124120 "On Doom"
  (:div
   (:p "I've finished
      the " (:a :href "https://store.steampowered.com/app/379720/" "Doom (2016)")
      " game lately and it triggered my Doom admiration again. Coincidentally I've
      got the "
      (:a :href "https://www.amazon.com/Masters-Doom-Created-Transformed-Culture/dp/0812972155"
	  "Masters of Doom") " book inside my Kindle and could read it as well nearly the
	   same time.")
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
   (:p "Anyway the book gets 5/5 stars from me and it worths money and time.")
   (:small "Есть " (:a :href "/blog?id=3678124121" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "games" "dev" "thoughts" "books")
  :meta  ((:meta :property "og:title" :content "On Doom")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "My personal opinion about the Doom (2016) game")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3678124120")))

(defblogpost 3678124121 "О Doom"
  (:div
   (:p "Недавно допрошёл
   игру " (:a :href "https://store.steampowered.com/app/379720/" "Doom (2016)")
   " и снова проникся восхищением ко всей серии. И как раз в это же время вспомнил, что
      у меня в Kindle есть книга "
   (:a :href "https://www.amazon.com/Masters-Doom-Created-Transformed-Culture/dp/0812972155"
       "Masters of Doom") " и как раз в это же время я её и прочёл.")
   (:h3 "Игра")
   (:p "Начну с игры: игра понравилась. Да, прямо вот " (:strong "понравилась") ". Не
   сказать, что это такой уж новый опыт и революция в жанре, но то самоё слегка
   нервирующее ощущение при нажатии на кнопки присутствует, ну и присутствуют все наши
   старые друзья, начиная прямо с замечательного жирного Манкубуса. Две основные вещи,
   которые поменялись с классического Doom (что, впрочем, заметили почти все) — игра стала
   проще и появились арены.")
   (:p "Ну, первая часть "
       (:a :href "https://www.youtube.com/watch?v=W1ZtBCpo0eU" "совершенно не удивляет") ":
   игры стали реально массовым развлечением, собрали вокруг себя огромную индустрию и
   становятся проще с каждым годом. Разницу между " (:em "Ultraviolence") " тогда
   и " (:em "Ultraviolence") " сейчас можно заметить без всяких усилий. Впрочем, после
   первого прохождения там добавилось несколько уровней сложности сверху, поэтому вполне
   возможно, что там ещё есть, где поупражняться.")
   (:p "Со вторым пунктом несколько сложнее: с одной стороны арены — неотъемлемая часть
   Quake III, а Doom всегда был про большой мир (пусть даже и разделённый на уровни из-за
   технических ограничений) с пространством, куда можно отступить, если монстры давят
   чересчур сильно; с другой стороны эти новые арены неплохо спроектированы и хорошо
   смотрятся не только с точки зрения гейм-дизайна с продуманными маршрутами, стенами,
   прыжковыми площадками и порталами, но и с эстетической точки зрения: текстуры, цвета,
   свисающие вокруг грешники… Впрочем, мне всё равно старые уровни с пространством для
   манёвра нравились больше.")
   (:p "Монстры подверглись ребалансу, но внешне всё ещё узнаваемы: если видишь шотганера,
   сразу понятно, что это он, а когда встречаешь Пинки, непроизвольно его приветствуешь,
   правда, пока он на тебя не бросится. По ощущениям кажется, что монстры стали слабее, но
   наносят больший урон, заставляя тебя постоянно и безостановочно двигаться. Первая же
   остановка обычно означает смерть. И это снова приносит ощущение игры скорее в Quake,
   чем в Doom, но всё равно круто. А сам ребаланс, кстати, весьма сильно раздражает, если
   хорошо помнишь игру в классический Doom. Теперь первая же встреча с Baron of Hell "
       (:s "заставляет отложить кирпичей") " очень сильно удивляет тем, что он забивает игрока
   насмерть двумя-тремя мощными ударами. Впрочем, на последних уровнях он удостаивается не
   большего внимания, чем обычные зомби. Ситуация с Imp'ом обратная: в классике завалить
   одного, двух, целую группу их, не является каким-то особым достижением, сейчас же они
   одни из самых опасных врагов в игре, особенно из-за количества. Imp'ов множество, они
   везде, они бегают по стенам и потолкам и швыряются оттуда файрболлами ровно в тот
   момент, когда ты собираешься отступить и поискать аптечку.")
   (:p "Набор оружия мне вполне нравится и не особо раздражает, что он не совпадает с
   классическим набором: гаусс-ган выглядит несколько странно, да и не сказал бы, что он
   очень уж эффективен, а вот шотган с режимом \"burst fire\" весьма неплох в некоторых
   местах. Двухстволка всё ещё символ и основное орудие труда на протяжении всей игры так
   как баланс между " (:strong "мощью") " и патронами весьма хорош. Ну и конечно же
   бензопила. И она, конечно, всё ещё чудо, как хороша! Даже лучше, чем можно себе
   представить — она распиливает практически кого угодно пополам (ну или на несколько
   частей) за секунду, а поиск бензина к ней — главный квест в игре.")
   (:p "В общем определённо рекомендую попробовать, если вы ещё этого не сделали, и
   составить собственное мнение. В крайнем случае, даже если игра не понравится, расстрел
   демонов ада из ружья — это весело!")
   (:hr)
   (:h3 "Книга")
   (:p "И пару слов о книге: " (:strong "must read") ". Да. Вот эта пара слов. Дэвид
   Кушнер отлично поработал, собрав кучу самой разноплановой информации из разных
   источников и запихав их в книгу. Наиболее захватывающая часть, конечно — вся эта
   история прошла прямо перед нашими глазами. Российский игровой мир отставал на несколько
   лет, но ситуация была прямо точно такой, как описано в книге.")
   (:p "Ну и с профессионально точки зрения я отметил для себя несколько моментов: момент
   изначального создания id, когда группа программистов всё равно нуждалась в менеджере,
   который возьмёт на себя всякие переговоры и будет жарить мясо, пока все работают и
   ситуацию в взлётом и падением Ion Storm: печальная, но закономерная история. Отличный
   профессионал необязательно окажется способен организовать команду и вести бизнес, даже
   если думает, что сможет. Команда (или человек), которая постоянно доставляет
   продукт (за что всё время держится Кармак) всегда побеждает огромные нереализованные
   планы, а все великие свершения базируются не только на гениальных озарениях, но и на
   скучных митингах и планомерной ежедневной работе.")
   (:p "В любом случае книга получает от меня 5/5 баллов и определённо стоит и своих денег
   и потраченного времени.")
   (:small "There is an " (:a :href "/blog?id=3678124120" :lang "en" "english version")
	   " of this post"))
  :tags '("ru" "games" "dev" "thoughts" "books")
  :meta  ((:meta :property "og:title" :content "О Doom")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Моё личное мнение об игре Doom (2016)")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3678124121")))

(defblogpost 3720711026 "О GPD Pocket"
  (:div
   (:h3 "TLDR")
   (:p "Девайс очень понравился и оказался ровно тем мобильным устройством для
   каждодневного ношения с собой, что я хотел.")
   (:p "Если вы ищете полноценный лаптоп для работы — это явно не ваш выбор.")
   (:h3 "Железо")
   (:table
    (:tr (:td "CPU") (:td "Intel Atom X7-Z8750"))
    (:tr (:td "RAM") (:td "8GB LPDDR3-1600"))
    (:tr (:td "Display") (:td "7inch IPS 1920x1200"))
    (:tr (:td "Storage") (:td "128GB eMMC SSD (non-replacable)"))
    (:tr (:td "Battery") (:td "7000mAH"))
    (:tr (:td "WiFi") (:td "Broadcom 4356 802.11ac"))
    (:tr (:td "Bluetooth") (:td "Broadcom 2045"))
    (:tr (:td "Audio") (:td "Realtek ALC5645"))
    (:tr (:td "Ports") (:td "1 x USB 3 type A, 1 x MicroHDMI, 1 x USB 3 type C,
    1 x 3.5mm Headphone Jack "))
    (:tr (:td "Touchscreen") (:td "Goodix Capacitive TouchScreen")))
   (:p "В целом, штуковина оказалась весьма шустрая. несмотря на то, что
атом. И даже видеокарта в этом самом атоме весьма достойная и годится для
небольших игрушек.")
   (:p "К сожалению, пока не купил переходник на HDMI и не проверил, работает
ли вывод на внешний монитор, но у меня есть подозрение, что для показа
презентаций вполне подойдёт.")
   (:p "Памяти аж восемь гигабайт, что для такого карманного девайса очень даже хорошо.")
   (:p "Остальная начинка, конечно, набрана с миру по нитке и заставляет
конструировать ядро из спичек и желудей.")
   (:p "Вайфай и блютус бродкомы, причем не лучшие, тачскрин вообще некий
Goodix Capacitive TouchScreen.")
   (:p "Клавиатура весьма терпимая для английского языка, хотя и костыльная для
русского. Половина клавиш с правой части распихана по таким углам. что не сразу
разберешься, где и искать. Иногда чуть залипает, если нажимать не на центр
клавиш, но к этому привыкаешь довольно быстро.")
   (:p "Корпус просто прекрасен, металлический, как говорят американцы, solid,
прямо приятно взять в руки. Открывается крышка плотно, \"внатяг\", прямо
приятно, ничего не скрипит, не люфтит.  Экран, помимо высокого разрешения и
прилагающихся проблем с масштабом и батареей, ничем не плох, скорее даже
хорош. Даже с тачем. На удивление не становится нечитаемым даже на солнце,
хотя, конечно, несколько бликует.  Трэкпойнт в принципе юзабельный, хотя и не
сказать, что сильно удобный, но тачскрин эту проблему решает - очень недалеко
тянуться от клавиатуры, поэтому вполне можно работать и с ним.")
   (:p "Главное - весит немного и размер ровно такой, чтобы влезать в сумку любого
размера, или даже достаточно большой карман.")
   (:h3 "Софт")
   (:p "С софтом традиционно хуже, как и у любого китайца. Приходится долго и
внимательно изучать вывод lshw перед тем, как собрать ядро. Причем ядро из
мейнлайна тоже пока работает так себе, надо брать " (:a :href "#kernel" "ядро
Ганса") ". Со всем остальным несколько проще, я поставил генту, из коробки
заработал BlueZ, после установки " (:tt "linux-firmware") " заработал
wifi. Честь и хвала ребятам из Интела, сразу заработало видео.")
   (:p "В иксах пришлось довольно долго развлекаться, подбирая шрифты и
масштаб, но в целом это уже меньшая проблема.")
   (:p "Также я видел как минимум " (:a :href "#respin" "\"фанатскую\"
respin-сборку убунты") ", которую авторы-китайцы благословили и торжественно
нарекли официальной. Там, поговаривают, все из коробки предустановлено. Оттуда
пришлось позаимствовать настройки тачкрина и трэкпада, а также пару полезных
пакетов, вроде управления кулером.")
   (:p "Вообще, оно сделано довольно интересно. Кулер есть, а автоматического
управления - нет. Решено скриптом " (:tt "gpd-fan") ", который тыкает палочкой
в gpio-порт. Хотя и до его запуска нельзя сказать, что процессор так уж сильно
перегревался.")
   (:p "Из портов есть один USB type A, type C, micro hdmi и 3.5mm jack, в
целом я больше ничего и не ожидал. Type C работает не только как гнездо
питания, но и как вполне себе USB, даже картридер для него предлагается.")
   (:p "UEFI очень неплох, все настройки, которых я там ждал, на месте, а в
моей линуксовой версии даже хранилище ключей по-честному пустое, никакого вам
микрософта. Шелл в комплекте есть, так что rescue shell для экспериментов с
загрузкой будет.")
   (:h3 "Батарея")
   (:p "От батареи работает, как я и ожидал, а скорее даже
надеялся, " (:em "ОЧЕНЬ") " долго. Тест показал, что примерно шесть-семь часов
компиляции в три потока (чтобы не перегревался), высаживают около восьмидесяти
процентов батареи.")
   (:h3 "Недостатки")
   (:p "Из недостатков можно упомянуть разве что далёкую от идеальной клавиатуру,
которая, к тому же, генерит странные сканкоды. Лично я не люблю мелкие иконки,
поэтому высокое разрешение экрана тоже скорее минус, чем плюс.")
   (:p "Спорный момент - отсутствие сим-карты. В целом выпускать его в интернет было бы
удобно, наверное, но это легко решается tethering'ом с телефона, например.")
   (:h3 "Вывод")
   (:p "В общем, как я и ожидал, когда донатил на кикстартере в первый же день,
девайс оказался практически идеален в качестве этакого \"карманного
емакса\". Для ответов на письма, быстрой правке кода на лету, в качестве этакой
записной книжки для каких-нибудь meeting minutes или просто для
структурированной записи мыслей. ")
   (:p "Либреофис работает неожиданно бодро, когда на ходу нуюно проверить или
поправить какую-нибудь внезапную табличку, никаких заметных проблем не
возникает.")
   (:p "Также если в дороге совсем скучно, можно попробовать позапускать
какое-то количество игрушек. Благодаря x86 устройство становится идеальным для
различных обитателей досбокса или какой-нибудь классики под wine (тех же
HoMM3). Впрочем можно и из стима наставить различных головоломок, вроде Shenzhen
i/o или MHRD, или даже скромных 3d.")
   (:h3 "Ссылки")
   (:ul
    (:li :id "kernel" (:a :href "https://github.com/jwrdegoede/linux-sunxi" "Hans de Goede kernel"))
    (:li (:a :href "https://www.reddit.com/r/GPDPocket/" "GPD Pocket subreddit"))
    (:li (:a :href "https://wiki.archlinux.org/index.php/GPD_Pocket" "Arch Linux Wiki"))
    (:li :id "respin" (:a :href "https://github.com/stockmind/gpd-pocket-ubuntu-respin" "GPD Pocket Ubuntu Respin"))
    (:li (:a :href "https://www.indiegogo.com/products/gpd-pocket" "GPD Pocket Marketplace")))
   (:h3 "Что докупил")
   (:ul
    (:li (:a :href "https://www.aliexpress.com/item/AUKEY-Braided-Nylon-USB-C-to-USB-3-0-USB-A-to-Type-C-Cable-for/32783788043.html" "Провод"))
    (:li (:a :href "https://www.aliexpress.com/item/AUKEY-Dual-USB-Wall-Charger-with-Quick-Charge-3-0-AIPower-Tech-Port-34-5W-9V/32737801057.html" "Зарядник"))
    (:li (:a :href "https://www.aliexpress.com/item/Laptop-Sleeve-Bag-for-GPD-Pocket-7-Inch-Mini-Laptop-UMPC-Windows-10-System-Notebook-Bag/32829812080.html" "Чехол")))
  :tags '("ru" "hw" "gpd")
  :meta  ((:meta :property "og:title" :content "О GPD Pocket")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Впечатления и опыт использования GPD Pocket")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3720711026")
	  (:style :type "text/css"
		  (str
		   (css '(("table"
			   :width "100%"
			   :border-collapse "collapse"
			   :border-color "grey")
			  ("td,th"
			   :padding "3px"
			   :border "1px solid grey"))))))))

(defblogpost 3747291152 "On Smart Home"
  (:div	(:P "I'm building a " (:I "smart home")
	    " in my new apartments and describe the process here.")
	(:H2 "Naming")
	(:P "First of all: I don't like all this movement with Internet of "
	    (:I "Anything") ", like IoT, IoE and so on. All this is just
advertising bullshit and nothing more.")
	(:P "The most appropriate word in my opinion is \"industrial
automation\", but it's about really " (:B "industrial") " things, where no one
cares about price or compatibility and interconnection with other systems. I
mean when you're trying to add automation into steel production facility,
you're ready to pay even for creation of own network standards or produce some
custom wire types if they are needed. But when you just try to improve your
quality of life locally, you have to find kind of trade-off.")
	(:P "So basically it's home automation, but I'm to improve the system
with small decision making capabilities, so can proudly call it Smart,
therefore Smart Home it is.")
	(:H2 "Key points")
	(:P "My view to the smart home idea based on several key points")
	(:H3 "Absolutely custom")
	(:P "No mass-production from famous brands may be used in key points of
real smart home.")
	(:OL
	 (:LI "Security" :BR
	      (:P "They say that \"The letter S in IoT stands for
    Security\". And that's true—big companies do not treat security at all, the
    idea is to fill the market with a huge set of devices and force customers
    to buy them using advertisement."))
	 (:LI "Data policy" :BR
	      (:P "Any company, either huge corporation like Google or any
small startup wants to collect your data first of all. All devices you buy
follow only one single target: gather all your data to make advertisements work
more effective (or resell the data to someone, who cares). Actually I don't
really care about ads (I'm not a target audience anyway), but nearly every IoT
service has security flaws and the data becomes available to everyone which is
not good at all."))
	 (:LI "Internet connectivity" :BR
	      (:P "Almost every mass-market smart home solution strongly
requires internet connectivity to function. This is absolutely inappropriate:
even if we don't take internet censorship in Russia which just switched off
Xiaomi smart home devices one day, we can't be sure that provider has 100% SLA,
there will be no issues with uplink network hardware and so on."))
	 (:LI "Customization" :BR
	      (:P "This point is related to almost all hardware now: you don't
buy devices anymore, you rent them. Every vendor makes everything to lock you
in own infrastructure, forbid you change any behavior and so on. This might be
fine for large purchases with support, like building a business center, when
you can sign up a contract and vendor is interested in customization for your
purposes. But this is not fine at all when you have a single apartment (or
house) and just want to control your devices the way you want.")))
	(:H3 "Fully autonomous")
	(:P "I'm going to design the system which will be fully autonomous and
will require nothing but electrical power to function. This includes:")
	(:OL
	 (:LI "No internet connectivity required" :BR
	      (:P "Using cloud to control your home is a sick idea, as I
already mentioned. Local server should be enough to get all features work."))
	 (:LI "No internet connectivity used" :BR
	      (:P "In order to improve security all devices which use network
should be isolated in one dedicated net (physically), so no 0-day bug will
affect smart home in any way."))
	 (:LI "No server required" :BR
	      (:P "Even if server goes down, devices must have a fallback
solution to be controlled. Again: single point of failure is not an option."))
	 (:LI "No wireless connectivity" :BR
	      (:P "All devices are to be connected using wires.")
	      (:P "It's not so hard to achieve when you build the solution from
scratch, but it solves lots of issues:")
	      (:UL
	       (:LI "You don't depend on environment, which is now an issue in
city, e.g. wi-fi frequencies are full of devices and connectivity is not so
good.")
	       (:LI "Hacking from outside is not possible: you have to enter
the room to connect to the network.")
	       (:LI "No power issues for sensors: when you use something like
ESP8266, you have to think about power and either you will constantly change
power cells, or you have to provide a wall outlet connection which is also
awful."))))
	 (:H3 "Fully open-source")
	 (:P "From my personal experience, open-source solutions are the best
options if you know what you do. I'm not a GPL fanatic, but when "
	     (:B "you") " control the program's behavior it's much easier to solve
any issue. You may try to buy the whole proprietary solution which will work
out-of-box and have beautiful design (and maybe even support for the first
time), but when you want to customize it slightly different than owner (vendor,
not you) wants it, you're busted. And you never know what's inside and what
will the device do next moment, and the worst part is: you have no way to know
it.")
	 (:H2 "Optional points") (:H3 "Configurability")
	 (:P "Not sure how wide the limits of this point should be, but I'm not
the fan of \"made once, works forever\" approach. We live in a changing world,
so putting wires inside concrete walls is not my choice. Still I do understand,
that I may agree to that if price difference is big enough.")
	 (:P "If we talk about software, I see no problems at all: using open
source software solves this problem completely.")
	 (:H3 "Reproduction possibility")
	 (:P "From side of vendors/component developers I expect detailed
technical specification, not just user manual and several examples, as some
companies like to do. As I mentioned already, I want to be able to control
every pdevice in my system or replace it with something else.")
	 (:P "From my side I'll describe my adventures in this blog and publish
all sources or hardware descriptions created during the process. The more
success (opr failure) stories we have, the more possibilities we give to those
who comes after us. And I consider experience sharing to be the great thing.")
	 (:small "Есть " (:a :href "/blog?id=3747291153" :lang "ru" "русская версия") " этого поста"))
	:tags '("en" "smart home" "refurbishment")
	:meta  ((:meta :property "og:title" :content "On Smart Home")
		(:meta :property "og:type" :content "article")
		(:meta :property "article:author" :content "https://www.facebook.com/rayslava")
		(:meta :property "og:description" :content "My vision of Smart Home design")
		(:meta :property "og:url" :content "https://rayslava.com/blog?id=3747291152")))

(defblogpost 3747291153 "Об умном доме"
  (:DIV
   (:P "Я строю " (:I "умный дом") " в своей новой квартире и буду описывать
  процесс здесь.")
   (:H3 "Название")
   (:P "Во-первых: я недолюбливаю это движение с названиями Интернет "
       (:I "Чего-то") ", IoT, IoE и так далее. Всё это просто рекламная фигня и
    не несёт никакой смысловой нагрузки.")
   (:P "Наиболее подходящим названием, по моему мнению, будет \"промышленная
    автоматизация\", однако оно применимо только к "
       (:B "промышленности") ", где никого не интересует цена, совместимость
    или возможность подключения к другим системам. Я имею в виду, что пытаясь
    внедрить автоматизацию на завод по производству стали, вы готовы заплатить
    скажем за создание нового сетевого стандарта или выпуска кабеля нового
    типа, если он понадобится. Однако, когда вы просто хотите несколько
    улучшить качество своей жизни, приходится идти на некоторые компромиссы.")
   (:P "В целом это просто автоматизация домашней жизни, однако я собираюсь
    расширить эту систему ограниченными возможностями по самостоятельному
    принятию решений, поэтому могу гордо называть её Умной, собственно
    получается Умный Дом.")
   (:H3 "Ключевые моменты")
   (:P "Моё видение умного дома базируется на нескольких ключевых вещах")
   (:H4 " Полностью самодельный")
   (:P "При создании умного дома нельзя использовать никаких серийных решений от
    известных брендов.")
   (:OL
    (:LI "Безопасность" :BR
	 (:P
	  "Уже есть поговорка \"The letter S in IoT stands for Security\" (буква
	\"S\" в аббревиатуре \"IoT\" обозначает Безопасность). И это правда:
	крупные производители не обращают ни малейшего внимания на безопасность
	их решений, на данный момент их цель — наводнить рынок огромной массой
	устройств и заставить пользователей покупать их, с помощью рекламы."))
    (:LI "Отношение к пользовательским данным" :BR
	 (:P
	  "Любая компания, будь это огромная корпорация, вроде Google, или
	небольшой стартап, в первую очередь хочет собирать ваши данные. Все
	устройства, которые вы можете купить преследуют ровно одну цель:
	собрать все возможные данные, чтобы заставить рекламу работать более
	эффективно (или кому-нибудь эти данные перепродать). И в целом я не
	слишком беспокоюсь о рекламе (всё равно я не целевая аудитория), но,
	поскольку практически в каждом сервисе и устройстве есть уязвимости и
	дыры в безопасности, эти данные доступны всем желающим, что уже совсем
	нехорошо."))
    (:LI "Зависимость от интернет-подключения" :BR
	 (:P "Практически все серийные решения для умного дома жёстко требуют
	наличия интернет-подключения для работы. Это абсолютно неприемлемо:
	даже если не принимать во внимание цензурирование интернета в России,
	которое не так давно просто выключило все устройства для умного дома от
	Xiaomi, дома невозможно иметь провайдера со стопроцентной доступностью,
	быть уверенным, что ничего не случится с проводом и так далее."))
    (:LI "Настраиваемость" :BR
	 (:P
	  "Этот пункт сейчас относится практически ко всему оборудованию: вы больше
	не покупаете устройства, вы берёте их в аренду. Каждый производитель
	делает всё возможное, чтобы заманить вас в свою экосистему и там
	закрепить, запретить вам менять настройки, поведение по умолчанию, и тому
	подобное. Это может быть вполне допустимо для больших закупок вместе с
	поддержкой и, скажем при оборудовании бизнес-центра, когда вы можете
	подписать подходящий контракт и производитель согласится на донастройку
	под ваши цели. Однако это довольно плохо, когда у вас всего лишь квартира
	или дом и вы просто хотите управлять вашими устройствами так, как вам
	нравится.")))
   (:H4 "Полная автономность")
   (:P
    "Система, которую я создаю, будет полностью автономной и не будет требовать
    ничего, кроме электроэнергии для работы:")
   (:OL
    (:LI "Соединения с интернетом не требуется" :BR
	 (:P
	  "Использование облака для управления умным домом — очень плохая идея,
	как я уже говорил выше. Локального сервера должно быть достаточно для
	работы всех систем."))
    (:LI "Соединение с интернетом не используется при работе" :BR
	 (:P "Для увеличения безопасности, все устройства должны быть
	подключены к отдельной, физически изолированной сети, чтобы ни одна
	уязвимость нулевого дня не могла повлиять на работу умного дома."))
    (:LI "Сервер не требуется" :BR
	 (:P "Даже если сервер упал, у устройств должна быть возможность
	аварийного управления. Повторюсь: единая точка отказа — не вариант."))
    (:LI "Отсутствие беспроводной связи" :BR
	 (:P "Все устройства должны быть подключены по проводам.")
	 (:P "Достигнуть этого не слишком сложно, когда вся система строится с
	нуля, однако этот подход решает множество проблем:")
	 (:UL
	  (:LI "Независимость от окружения, что сейчас в больших городах уже
	  является проблемой: например частоты wi-fi забиты и качество
	  соединения не слишком хорошее.")
	  (:LI
	   "Невозможен взлом снаружи: нужно физически войти в комнату и
	  подключиться к сети.")
	  (:LI
	   "Решение проблемы с питанием датчиков: когда вы используете
	  какой-нибудь ESP8266, приходится думать о питании и либо постоянно
	  менять батарейки, либо организовывать электрическую розетку, что не
	  менее ужасно."))))
   (:H4 "Полностью открытое и свободное программное обеспечение")
   (:P "Из моего личного опыта, решения на основе открытого ПО — лучшее
    решение, когда вы понимаете, что вы делаете. Я не фанатичный последователь
    GPL, однако, когда есть возможность "
       (:B "самому") " управлять поведением программы, любую проблему всегда
    решить сильно проще. Можно, конечно, попробовать купить проприетарное
    решение, которое будет работать прямо из коробки, да ещё и иметь
    великолепный дизайн (а может быть даже и поддержку первое время), однако
    как только вы захотите что-нибудь настроить чуточку не так, как хочет
    хозяин (производитель, не вы), вы попали. А ещё вы никогда не узнаете, что
    находится внутри и что устройство сделает в следующий момент, а самая
    худшая часть — у вас нет возможности об этом узнать.")
   (:H3 "Дополнительные плюсы") (:H4 "Настраиваемость")
   (:P "Не уверен, насколько широко должен распространяться этот пункт, но я не
    сторонник подхода \"сделал раз, работает вечно\". Мы живём в меняющемся
    мире, поэтому запихивать провода внутрь бетонных стен — не мой выбор. С
    другой стороны, я прекрасно понимаю, что пойду и на это, если будет
    значительная разница в цене.")
   (:P "Если же речь о ПО, то не вижу никакой проблемы, открытый исходный код
    полностью решает эту проблему.")
   (:H4 " Воспроизводимость")
   (:P "Со стороны производителей и разработчиков компонентов я ожидаю
    подробной технической спецификации, а не просто руководства пользователя с
    несколькими примерами, как любят делать некоторые компании. Как я уже
    упоминал, я хочу иметь возможность управлять каждый устройством в системе и
    иметь возможность заменить его чем-нибудь другим.")
   (:P "Со своей стороны, постараюсь описывать все мои приключения в этом блоге
    и публиковать исходный код и описание железа, создаваемое в процессе. Чем
    больше историй успеха (или провала) напишем мы, тем больше возможностей мы
    дадим тем, кто придёт за нами. Я считаю такую возможность поделиться опытом
    замечательной штукой.")
   (:small "There is an " (:a :href "/blog?id=3747291152" :lang "en" "english
   version")))
	:tags '("ru" "умный дом" "ремонт")
	:meta  ((:meta :property "og:title" :content "Об умном доме")
		(:meta :property "og:type" :content "article")
		(:meta :property "article:author" :content "https://www.facebook.com/rayslava")
		(:meta :property "og:description" :content "Моё видение разработки умного дома")
		(:meta :property "og:url" :content "https://rayslava.com/blog?id=3747291153")))

(defblogpost 3817522800 "Дневник вакцинации Sputnik-V #1"
  (:div
   (:p "Чо-то вокруг началась совсем дичь и заболевают повально все, скажем за
прошлые две недели у меня заболела буквально вся семья (разнесённая
географически), радует хоть, что в лёгкой форме.")
   (:p "Вот и я решил немножко поболеть, но контролируемо. Раз уж началась
массовая вакцинация нашим хвалёным спутником, принял участие.")
   (:p "Зашёл на mos.ru, нажал «записаться на прививку», оно спросило номер
полиса, я ввёл, выбрал время и всё. Пришёл к ним, они спросили фамилию и где
работаю. Назвал фамилию, сказал, что работаю в исследовательском центре, иногда
приходится читать лекции школьникам и выступать на научных
конференциях. Девушка покивала, сказала «Ок», дала на заполнение форму согласия
и отправила в процедурный кабинет.")
   (:p "Вообще там народа прямо совсем немного, в нашей районной поликлинике
выделили целое крыло под эту программу вакцинации, я провёл там минут сорок
пять, встретил только трёх человек, не считая персонала. Похоже, никто особо не
спешит прививаться, а самой поликлинике тоже надо набирать статистику.")
   (:small "There is an " (:a :href "/blog?id=3817522801" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #1")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3817522800")))

(defblogpost 3817522801 "Sputnik-V vaccination log #1"
  (:div
   (:p "The COVID-19 situation becomes harder and harder, more and more people
get sick. For example my whole family got infected during last two weeks
despite they all live in different places.")
   (:p "So I decided to get sick as well and went to take part in massive
vaccination program started by our government.")
   (:p "Just performed registration via " (:a :href "https://mos.ru" "mos.ru")
       ", entered insurance ID, chose appropriate time and came to the local
clinic. Administrator just asked me about my workplace, and accepted my answer
that I'm a member of research center, work in \"Samsung School\" program and
take part in conferences. The only thing I had to do is to fill an agreement
form and get a Sputnik-V shot.")
   (:p "Apparently the vaccine is not so popular, I've only seen three people
who came for vaccination during 45 minutes I spent in clinic.")
   (:small "Есть " (:a :href "/blog?id=3817522801" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #1")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
	  :content "https://rayslava.com/blog?id=3817522800")))

(defblogpost 3817548000 "Дневник вакцинации Sputnik-V #2"
  (:div
   (:p "Начинаю ощущать небольшой дискомфорт, как при лёгкой простуде. Похоже,
   что аденовирус добрался по адресу. Надеюсь, что он ещё и принёс с собой
   нужный белок. Небольшой озноб, лёгкая головная боль, першение в горле. ")
   (:p "Сравнил ощущения с привившимся коллегой, отличий никаких, у него было
   ровно то же.")
   (:small "There is an " (:a :href "/blog?id=3817548001" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #2")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3817548000")))

(defblogpost 3817548001 "Sputnik-V vaccination log #2"
  (:div
   (:p "Feeling kind of discomfort very alike to a mild cold, sore throat and a
   light headache. Discussed it with a colleague who got the shot a while ago
   and he said that he felt exactly the same. Hope this means that
   adenovirus (platform of Sputnik-V) reached its destination and got
   SARS-CoV-2 protein with it. So it seems that currently I'm doing good.")
   (:small "Есть " (:a :href "/blog?id=3817548000" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #3")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817548001")))

(defblogpost 3817558800 "Дневник вакцинации Sputnik-V #3"
  (:div
   (:p "Поужинал, полчасика полежал, попил тёплого чая с лимоном, боль прошла,
   горло успокоилось, некоторое ощущение слабости и температура 36.9°C В
   принципе, не всё так плохо.")
   (:small "There is an " (:a :href "/blog?id=3817558801" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #3")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url" :content "https://rayslava.com/blog?id=3817558800")))

(defblogpost 3817558801 "Sputnik-V vaccination log #3"
  (:div
   (:p "Cold symptoms are gone. Had a dinner, got a rest for 30 minutes and a
   cup of warm tea with lemon, now head and throat are fine, now it's just a
   light weakness and temp. of 36.9°C")
   (:small "Есть " (:a :href "/blog?id=3817558800" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #3")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817558801")))

(defblogpost 3817609200 "Дневник вакцинации Sputnik-V #4"
  (:div
   (:p "Первая ночь прошла вполне терпимо. В районе половины одиннадцатого
   температура свалилась до 36.2°С, потом к полуночи вернулась до 37°C и уже не
   опускалась. Несколько раз удалось подремать, но без нормального сна, только
   в районе трёх часов получилось,наконец, заснуть. И уже в 7:20 я проснулся
   слегка вспотевшим и с тем самым ощущением «иммунитет дожрал
   вирус». Побаливало место укола, ощущалась слабость, но температура уже в
   норме и состояние довольно приемлемое. ")
   (:p "Молодцы они в этом Центре Гамалеи, дозу аденовируса намешали очень
   профессионально. Ровно на одну ночь и ровно до состояния «неприятно, но не
   болезненно». Видно, что давно работают с ней. ")
   (:p "Сейчас вот проснулся окончательно, состояние вполне терпимое: чуть
   першит в горле, ощущается некоторая усталость и недосып, но и всё, можно
   даже попробовать поработать. ")
   (:small "There is an " (:a :href "/blog?id=3817609200" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #4")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
	  :content "https://rayslava.com/blog?id=3817609200")))

(defblogpost 3817609201 "Sputnik-V vaccination log #4"
  (:div
   (:p "The first night was rather fine. About 22:30 the temperature came down
   to 36.2°С, then returned back to 37°C at midnight and stabilized. Several
   times I've been snoozing but fell asleep about 3 AM. And at 7:20 I woke up
   in a sweat and with that feeling \"my immune system just finished up the
   virus\". The spot of the shot hurt a little, but there was no fever and
   overall condition was rather good. ")
   (:p "Good job for Gamaleya Research Institute: the adenovirus doze was
   prepared professionally. Just for single night and accurately up to
   condition \"not very comfortable but it doesn't hurt\". I guess, this
   clearly indicates that they work with it for a long time already. ")
   (:p "Now I'm awaken at last, my condition is appropriate: throat is sore a
   little, feeling tired and sleepy, but nothing more. Will try to work. ")
   (:small "Есть " (:a :href "/blog?id=3817609200" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #4")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817609201")))

(defblogpost 3817632600 "Дневник вакцинации Sputnik-V #5"
  (:div
   (:p "Начались эффекты второго дня. Лёгкое раздражение кожи, слабые боли в
   мышцах, 36.8°C, тяжёлая голова, болит место укола. Озноб практически не
   ощущается, состояние лучше, чем вчера вечером.")
   (:small "There is an " (:a :href "/blog?id=3817632601" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #5")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817632600")))

(defblogpost 3817632601 "Sputnik-V vaccination log #5"
  (:div
   (:p "Second day side-effects are coming. Light skin irritation, light muscle
   pain, 36.8°C, heaviness in the head, and the shot spot hurts a little. Fever
   is hardly sensible though, feels much better than yesterday.")
   (:small "Есть " (:a :href "/blog?id=3817632600" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #5")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817632601")))

(defblogpost 3817693800 "Дневник вакцинации Sputnik-V #6"
  (:div
   (:p "Возвращаюсь в нормальное состояние. Вчера где-то до одиннадцати часов
   температура слегка плавала от 36.2°C до 36.8°C, но в принципе не особо
   мешала. Вполне нормально уснул, и вполне нормально выспался. Из неприятных
   ощущений осталось только тянущее ощущение в месте укола. Похоже, что
   действие аденовируса закончилось, и теперь остаётся ожидать, что будут
   генерироваться антитела. ")
   (:small "There is an " (:a :href "/blog?id=3817693801" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #6")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817693800")))

(defblogpost 3817693801 "Sputnik-V vaccination log #6"
  (:div
   (:p "Getting back to normal. Yesterday evening the temperature floated
   between from 36.2°C to 36.8°C but it wasn't too uncomfortable. Fell asleep
   just fine and got enough sleep. Now the only thing left from vaccination is
   light nagging in the shot spot. Apparently adenovirus effect has ended and
   now I can only wait until I get enough antibodies. ")
   (:small "Есть " (:a :href "/blog?id=3817693800" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #6")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3817693801")))

(defblogpost 3787713600 "Дневник вакцинации Sputnik-V #7"
  (:div
   (:p "Получил второй компонент прививки. По сравнению с прошлой
   прививкой,куда я пришёл в нормальном состоянии, в этот раз у меня чуть
   побаливает горло (видимо новогоднее шампанское было слишком холодным), так
   что ожидаю усиленного эффекта именно на горло.")
   (:small "There is an " (:a :href "/blog?id=3787713601" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #7")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3787713600")))

(defblogpost 3787713601 "Sputnik-V vaccination log #7"
  (:div
   (:p "Received a shot of second vaccine component. Now I've got my throat a
   little sore, so I expect it as a primary place to feel effect.")
   (:small "Есть " (:a :href "/blog?id=3787713600" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #7")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3787713601")))

(defblogpost 3819423600 "Дневник вакцинации Sputnik-V #8"
  (:div
   (:p "Вторая прививка оказалась потяжелее первой: примерно к часу ночи
   поднялась до 37.8° и не опускалась часов до четырёх, впрочем, кроме
   температуры ничего не болело, так что было скорее просто неприятно, даже
   место укола в этот раз почти не болит. В итоге половину ночи всё равно не
   мог уснуть, поэтому сидел и пил тёплый чай с лимоном, в общем, состояние так
   себе, хотя, после того, как температура упала, и удалось поспать.")
   (:small "There is an " (:a :href "/blog?id=3819423601" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #8")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3819423600")))

(defblogpost 3819423601 "Sputnik-V vaccination log #8"
  (:div
   (:p "The second component feels a little worse than first one. By 1 AM the
   fever came with 37.8°C and remained until 4 AM. There were no symptoms
   except the fever though, no pain, no sore throat, no head ache, no running
   nose, just discomfort. But still I couldn't sleep well, so stayed awake and
   just drank lots of warm lemon tea until temperature went down. Now I feel
   myself tired, but rather fine.")
   (:small "Есть " (:a :href "/blog?id=3819423600" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #8")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3819423601")))

(defblogpost 3819510000 "Дневник вакцинации Sputnik-V #9"
  (:div
   (:p "Эта ночь прошла спокойно, никаких проблем, никаких неприятных ощущений,
   похоже, второй компонент тоже прижился. Теперь подожду пару недель и пойду
   проверюсь на наличие антител.")
   (:small "There is an " (:a :href "/blog?id=3819510001" :lang "en" "english
   version")))
  :tags '("ru" "covid" "вакцинация")
  :meta  ((:meta :property "og:title" :content "Дневник вакцинации Sputnik-V #9")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Описание ощущений от
	  прививки")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3819510000")))

(defblogpost 3819510001 "Sputnik-V vaccination log #9"
  (:div
   (:p "This night went well, no discomfort, no symptoms, nothing. Apparently
   the second component is set. So now I'll wait for two weeks and perform an
   IgG check.")
   (:small "Есть " (:a :href "/blog?id=3819510000" :lang "ru" "русская версия")
	   " этого поста"))
  :tags '("en" "covid" "vaccination")
  :meta  ((:meta :property "og:title" :content "Sputnik-V vaccination log #9")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Describing my life after
	  Sputnik-V vaccination")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3819510001")))

(defblogpost 3887892772 "ActivityPub enabled"
  (:div
   (:p "Lately I've spent some time and added the very basic ActivityPub support to
   the blog. Now posts with the 'fedi' tag will be automatically published at
   @blog@rayslava.com account.")
   (:p "Now I need a bit of rest and then will share some thoughts on the standard
   and implementation"))
  :tags '("en" "fedi" "test")
  :meta  ((:meta :property "og:title" :content "ActivityPub enabled")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "announce")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3887892772")))

(defblogpost 3888337755 "Поддержка локализации ActivityPub"
  (:div
   (:p "Внезапно, под покровом ночи, рывком добавил поддержку тега с языком в
   ActivityPub. Потому что могу.")
   (:p "И да, я всё ещё помню, что я хотел написать пост про эту поддержку, но пока не
   добрался"))
  :tags '("ru" "fedi" "дыбр")
  :meta  ((:meta :property "og:title" :content "Поддержка локализации ActivityPub")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Добавление локализации в ActivityPub")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3888337755")))

(defblogpost 3888412487 "Добавил Unsubscribe и Update в ActivityPub"
  (:div
   (:p "Покрон ночи, конечно, починить не вышло, потому что слишком много времени
   прошло, но я дописал ещё кусочек функциональности в часть с поддержкой
   ActivityPub.")
   (:p "В целом, как всегда, когда есть базовая часть, дописывать новые куски с каждым
   разом всё проще и проще. Такими темпами у меня появится вполне живая
   поддержка, собственно, из того, что я хотел бы добавить, остался только сбор
   статистики по бустам и добавлению в фэйвориты."))
  :tags '("ru" "fedi" "дыбр")
  :meta  ((:meta :property "og:title" :content "Поддержка локализации ActivityPub")
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
	  (:meta :property "og:description" :content "Добавил Unsubscribe и Update в ActivityPub")
	  (:meta :property "og:url"
		 :content "https://rayslava.com/blog?id=3888412487")))

;;; Push new posts to activitypub if needed
(maybe-deliver-new-posts)
