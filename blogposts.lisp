;;; A personal blog engine main file
;;; Post id is `(local-time:timestamp-to-universal (local-time:now))'
(defpackage :site.blogposts
  (:use :site.blog :cl :cl-css :cl-who))

(in-package :site.blogposts)

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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
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
	  (:meta :property "og:type" :content "article")
	  (:meta :property "article:author" :content "https://www.facebook.com/rayslava")
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
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3678124120")))

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
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3678124121")))

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
	  (:meta :property "og:url" :content "http://rayslava.com/blog?id=3720711026")
	  (:style :type "text/css" 
		  (str
		   (css '(("table"
			   :width "100%"
			   :border-collapse "collapse"
			   :border-color "grey")
			  ("td,th"
			   :padding "3px"
			   :border "1px solid grey"))))))))

