module SurpriseTales
open IfEngine
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open Farkdown.Experimental.Helpers
open FsharpMyExtension.ResultExt
open IfEngine.Discord.Utils

type CustomStatement = unit
type CustomStatementArg = unit

type Label =
    | Menu
    | StartTaleAboutWizard
    | StartTaleAboutKing
    | StartTaleAboutSvarshik

let beginLoc: Label = Menu

let menu caption choices =
    CommonContentWithNarrator.createMenu caption choices

let say content =
    CommonContentWithNarrator.createSay content

let bluetoothSay =
    CommonContentWithNarrator.createNarratorSay' "Максишебник" "https://cdn.discordapp.com/avatars/884492693475053639/d6a06abf4c3458f29ff32bde06dec390.webp?size=48"

let surpriseNarrator =
    Narrator.create "Сказочница" "https://cdn.discordapp.com/avatars/807631911131807765/716cf4e01b5450e4e39de93bb9aaa3e7.webp?size=48"

let surpriseMenu =
    CommonContentWithNarrator.createNarratorMenu
        surpriseNarrator

let surpriseSay =
    CommonContentWithNarrator.createNarratorSay surpriseNarrator

let agentSay =
    CommonContentWithNarrator.createNarratorSay'
        "Агентарито"
        "https://cdn.discordapp.com/avatars/796931597898088448/e9c47d2b4a6e14797d1d348e69a33836.webp?size=48"

let adalindaSay =
    CommonContentWithNarrator.createNarratorSay'
        "Адасиринити"
        "https://cdn.discordapp.com/avatars/572010412157960192/407a80311750dc55d6d4abe188c545b4.webp?size=48"

let sobenokSay =
    CommonContentWithNarrator.createNarratorSay'
        "СОВЕНОК"
        "https://cdn.discordapp.com/guilds/927554008263032832/users/516737047147446272/avatars/a25b7a033b212f52ff822da145c47201.webp?size=48"

let taleAboutSvarshik =
    [
        label StartTaleAboutSvarshik [
            say [
                p [[ text "В далёком энном году в деревне Колхозовка жил простой трудовой народ. У каждого из этих работников были свои обязанности которые они с большим удовольствием выполняли. Вся деревня трудилась на благо своих семей, близких, друзей, друзей-друзей…" ]]
            ]
            say [
                p [[ text "Деревня Колхозовка была слишком маленькая и ценные работники были просто на вес золота, как трудяга-сварщик Макситруд." ]]
            ]
            say [
                p [[ text "Хороший был мужик Макситруд, но страсть как не любил баб." ]]
            ]
            say [
                p [[ text "Обеспокоилось деревня, что Такой мужик пропадает. Начали способы искать засватать." ]]
            ]
            agentSay [
                p [[ text "А потом пришла доярка с, дескать, большими \"глазами\", в которые он влюбился по уши? <:pepeSmile:947129401785593978>" ]]
            ]
            surpriseSay [
                p [[ text "Нет, это было бы слишком банально <:pepeSmile:947129401785593978>" ]]
                p [[ text "И вообще, не трогайте мою сказку <:beltPunishment:934683057716330496>" ]]
            ]
            say [
                p [[ text "Так вот, обеспокоилась деревня Колхозовка. Начали думать, что делать с Макситрудом." ]]
            ]
            say [
                p [[ text "Собрались все на площади у колодца и давай все вместе втихаря от Макситруда думки думать." ]]
            ]
            bluetoothSay [
                p [[ text "Я теперь тоже в этом колодце, в котором все думали, что со мной делать)" ]]
            ]
            say [
                p [[ text "Посыпались предложения… первая подскочила местная доярка Адалин. В народе ее любили ласково называть Даля." ]]
            ]
            say [
                p [[ text "И кричит на всю площадь:" ]]
                p [[ text "— Считаю, надо больше в деревне создавать рабочих мест, чтобы бабы приезжали свободные и выбора побольше было!" ]]
            ]
            agentSay [
                p [[ text "Слова не доярки, но председателя колхоза <:pepeFeelWow:979013476880109568>" ]]
            ]
            surpriseSay [
                p [[ text "<:beltPunishment:934683057716330496>" ]]
            ]
            agentSay [
                p [[ text "Молчу-молчу! 🙀" ]]
            ]
            say [
                p [[ text "Люди уважали Далю, прислушались. Минутное затишье на обдумывание… но из всей толпы больше всего идея понравилась местному ловеласу Агентасу. Такие идеи ему всегда по душе. Наконец-то разгуляй будет." ]]
            ]
            say [
                p [[ text "Люди обдумали всё хорошенько и поддержали доярку Далю и ловеласа Агентаса." ]]
            ]
            say [
                p [[ text "Но тут в обсуждение встревает с вопросом местная медсестра Маризуб: «Где рабочие места взять»?" ]]
            ]
            say [
                p [[ text "Задумались снова людишки: а ведь Маризуб правильные вопросы задает!" ]]
            ]
            say [
                p [[ text "Общими усилиями, правдами и неправдами пришли к выводу — Макситруд сам должен что-то создать своё." ]]
            ]
            say [
                p [[ text "Пришли Агентас, Машазуб и местный авторитет-доярка Даля к Макситруду на разговор." ]]
            ]
            say [
                p [[ text "Макситруд не сразу хотел открывать этим людям, не понравилось ему, что женщины приперлись, ну раз один мужчина с ними — так и быть, открыл." ]]
            ]
            say [
                p [[ text "Тут председатель колхоза он же местный ловелас Агентас промолвил: «Ходить я вокруг да около не буду, деревню надо поднимать, вовлекать новых людей»" ]]
            ]
            say [
                p [[ text "Макситруд, как всегда, с первого раза не понял, что от него хотят, ну была у него такая проблема, и 250 раз всё переспрашивал. С детства такой тугенький был, но это не мешало ему быть хорошим сварщиком" ]]
            ]
            bluetoothSay [
                p [[ text "— Слушай, ты) а ты никого со мной не перепутала?)" ]]
            ]
            surpriseSay [
                p [[ text "— А причём тут ты, вообще-то это сказка <:pepeSmile:947129401785593978>" ]]
            ]
            bluetoothSay [
                p [[ text "— Что вот с тобой делать)" ]]
            ]
            say [
                p [[ text "Председатель Колхозовки Агентас разрисовал снова все перспективы Макситруду, и все вместе взялись за дело." ]]
            ]
            say [
                p [[ text "Идея была проста: начать выращивать картофель и вывозить в страны заморские." ]]
            ]
            say [
                p [[ text "Ну что с делом общим определись, команда собрана. Пора и дело начинать." ]]
            ]
            say [
                p [[ text "Начали по всем новостным каналам близлежащих деревень трубить: «Деревня Колхозовка ждёт всех энергичных, молодых на работу. З.Ы. Жильё предоставляем!»" ]]
            ]
            say [
                p [[ text "Жилья в Колхозовке было много, т.к. много домов уже пустовало — никто ехать с города в глушь не хотел." ]]
            ]
            bluetoothSay [
                p [[ text "А Макситруд, значит, родился в жопе) впрочем, как и всегда — коричневый)" ]]
            ]
            surpriseSay [
                p [[ text "Макситруд, Даля, Агентас были местными — место рождения д. Колхозовка, Колхозанской области. И вообще, кто давал право на претензию к авторской сказке?)" ]]
            ]
            bluetoothSay [
                p [[ text "Прошу прощения) А где кульминация?)" ]]
            ]
            surpriseSay [
                p [[ text "Не ваше дело ))" ]]
            ]
            say [
                p [[ text "Так вот, понаехали молодые, красивые и не очень девушки в самом соку." ]]
            ]
            say [
                p [[ text "Так вот, приехали девушки с разных уголков Колхазанской области. Брюнетки, шатенки, рыжие, брюнетки — на любой вкус и цвет. Счастье ловеласа-председателя не знало предела" ]]
            ]
            say [
                p [[ text "Макситруд всех обсуждающим взглядом оценил и дальше варить свои сварщенские дела пошёл" ]]
            ]
            bluetoothSay [
                p [[ text "Автор разбирается в сварочных работах)" ]]
            ]
            say [
                p [[ text "Шли недели, месяцы… но счастья Макситруд не нашёл…" ]]
                p [[ text "Тут в деревне обьявили о смерти пожилой местной учительницы… горе было в деревне" ]]
            ]

            bluetoothSay [
                p [[ text "Ты на что намекаешь)" ]]
            ]
            say [
                p [[ text "Не знал народ, что делать, кто детей будет учить и развивать образованность." ]]
            ]
            say [
                p [[ text "Время шло, дни — за днями, и вдруг приезжает почти молодая, сногшибательная учительница Светуч!" ]]
            ]
            bluetoothSay [
                p [[ text "И вот она <:pepeSmile:947129401785593978>" ]]
            ]
            say [
                p [[ text "Приглянулась Макситруду Светуч и начал он по своей неопытности странные знаки внимания уделять: то письма напишет непонятного содержания, то посылку с кусочком сварочного материала пришлёт." ]]
            ]
            say [
                p [[ text "Разозлил ее Макситруд, решила ему тем же отвечать" ]]
            ]
            say [
                p [[ text "Длилось это не день, не два… а недели. Но странные письма-посылки перешли в более душевные, тёплые…" ]]
                p [[ text "Так и переросло в что-то долгое и настоящие" ]]
                p [[ text "Деревня ликует, Макситруд пристроен" ]]
            ]
        ]
    ]

let taleAboutKing =
    [
        label StartTaleAboutKing [
            say [
                p [[ text "В некотором царстве, в некотором государстве жил-был король Максивред. Вредный был, ко всем людям докалупывался. Жил один из-за своего склочного характера! Народ не любил короля за это да и он себя в одиночку чувствовал комфортнее. Время шло, вредность его не уходила, а только накапливалась как и людской страх." ]]
            ]
            say [
                p [[ text "Людям надоело, что король у них такой докапучий, решили его свергнуть — убить. Народ бастовал! Не было ни одного человека кто бы встал на сторону короля. Не собирая вещей, Максивред пустился в бега." ]]
            ]
            say [
                p [[ text "Выбежал он из своего царства в лес тёмный. Бежит, оглядывается, страх не покидает. Вдруг, услышал Макивред шевеления в кустах и снова пустился в бега. Бежал столько, что от бессилия просто упал и уснул." ]]
            ]
            say [
                p [[ text "Просыпается, видит… что удобно и оказывается лежит в кровати. Не понял где оказался король. Давай смотреть по стенам — картины с цветами, на полках много разных хахаряшек ||хахаряшки — это брелки, всякие мелкие сувениры и прочее (прим. ред.)|| непонятных. Посмотрел король на обстановку, ничего понять не может… дом какой-то непонятный, всё заставлено непонятно чем. Некомфортно стало Максивреду. Либо выбираться из этого дома и погибнуть, либо жить и смириться" ]]
                p [[ text "Выбрал жить и смириться." ]]
            ]
            say [
                p [[ text "Ходил дальше, смотрел обстановку но любопытство распирало кто же хозяин этого непонятного места. Ждал час, ждал два… никто домой не приходит. Устал от ожидания да и проголодался король. Заглядывает в холодильник а там всё в сладком и мучном — тортики, пирожные. Некуда деваться Максиареду, взял что есть. Слышит, в дверях ключ поворачивается. Страшно и одновременно любопытно. Испуганные глаза короля видят молодую, длинноволосую девушку с большими «глазами». Не так уж и страшно, как думал король." ]]
            ]
            say [
                p [[ text "— Ну что, давай знакомиться? — сказала девушка королю. Дождалась кивок Максивреда и продолжила: — Меня зовут Светаскромн." ]]
            ]
            say [
                p [[ text "Очень много вопросов последовало от короля. Почему он в своих владениях никогда не встречал этого дома, кто она такая и т.д. Светаскромн слишком скромно отвечала на все вопросы названного гостя. Оказалось, Светаскромн родом из обычной крестьянской семьи, и она очень давно ушла жить отдельно в этот дом." ]]
            ]
            say [
                p [[ text "Много было разговоров у Максивред и Светаскромн." ]]
            ]
            say [
                p [[ text "И король понял, вот оно — наконец-то появился человек, который его понимает и не раздражает" ]]
                p [[ text "Доверился король девушке, рассказал о своих бедах с царством, и начали они продумывать план, как обратно вернуть доверие в королевстве." ]]
            ]
            say [
                p [[ text "Но у Светоскромн была знакомках колдунья Виолеттамаг" ]]
            ]
            say [
                p [[ text "Приходят они к колдунье Виолеттамаг и всё рассказывают. На что колдунья и отвечает — Вот вам цветочек волшебный под названием treshwork каждый из лепестков волшебный, случится беда, отрываете лепесток и загадываете желание" ]]
            ]
            say [
                p [[ text "Решила добрая и такая отзывчивая Светаскромн помочь всё-таки Максивреду -жалко же. Взяла цветок у колдуньи и в дорогу засобиралась" ]]
            ]
            say [
                p [[ text "Идут в направлении к царству короля и вылетает огромная птица ПтицАдалинд" ]]
                p [[ text "Отрывает тут же от treshwork лепесточек чтобы эта птица злющая исчезла…" ]]
            ]
            say [
                p [[ text "Но волшебный цветочек подействовал так, что злая и большая птица стала добрым союзником Светаскромн и Максивред" ]]
            ]
            say [
                p [[ text "Идут три союзника на завоевания царства и на них нападает злая ПантерНат" ]]
            ]
            say [
                p [[ text "Снова отрывает листочек Светаскромн" ]]
                p [[ text "И что же случилось со злом?)" ]]
                p [[ text "Правильно, его не стало)" ]]
            ]
            say [
                p [[ text "И кто бы мог подумать… одинокий король уже в компании прекрасных союзников…" ]]
            ]
            say [
                p [[ text "А дальше уважаемые читатели случилось то, что не ожидал никто…" ]]
            ]
            say [
                p [[ text "Каждый раз встречались на пути союзников непонятные злые существа которые после вырывания лепестка цветка становились добрее" ]]
                p [[ text "Ворвались союзники в царство, забрали трон обратно" ]]
            ]
            say [
                p [[ text "И король понял… что без встречи Светаскромн он бы так и остался в бегах от людей и был бы одиноким" ]]
            ]
            say [
                p [[ text "Кстати насчёт цветочка…" ]]
            ]
            say [
                p [[ text "Когда трон был захвачен, остался один лепесточек" ]]
                p [[ text "Смотрит на цветочек король и на Светаскромн и задумался…" ]]
            ]
            say [
                p [[ text "Кричит, зовёт Виолеттамаг" ]]
                p [[ text "Появилась колдунья" ]]
            ]
            say [
                p [[ text "И говорит ей «Виолеттамаг, нам так помог этот цветочек, мое последнее желание будет сделать его человеком»" ]]
            ]
            say [
                p [[ text "А я вовсе не колдунья,я любила и люблю" ]]
                p [[ text "Это мне судьба послала, чот там грешную любовь моююю" ]]
            ]
            say [
                p [[ text "Услышала желание колдунья и по щелчку цветочек превращается в человека" ]]
            ]
            say [
                p [[ text "Так Максивред создал свою личную команду которой доверял и правили вместе целым государством" ]]
                p [[ text "Ну а что Светаскромн спросите вы?" ]]
            ]
            say [
                p [[ text "Светоскромн стала той, у которой всё было и куша это ничего не было <:pepeSmile:947129401785593978>" ]]
                p [[ text "Появился личный велосипед" ]]
                p [[ text "Хороший замок не с протекающей крышей" ]]
            ]
            say [
                p [[ text "И конечно же любовь всех кто находился в царстве )" ]]
                p [[ text "Виолеттамаг стала советником королевства которая всегда помогала магическими штуками в захвате территорий" ]]
                p [[ text "Цветочек, он же treshwork стал личным доктором королевства" ]]
            ]
            say [
                p [[ text "Добрая и красивая птица стала прекрасной хозяюшкой на кухне" ]]
            ]
            say [
                p [[ text "Так вот, в чем суть сказки… а она проста! Не надо быть вредным, дать людям шанс, раскрытиям им и оно тебя приятно удивят" ]]
            ]
            say [
                p [[ text "Ага,типо за каждым успешным мужчиной,стоит мудрая женщина <:pepeSmile:947129401785593978>" ]]
                p [[ text "" ]]
            ]
        ]
    ]

let taleAboutWizard =
    [
        label StartTaleAboutWizard [
            say [
                p [[ text "В таинственном лесу, на высокой-высокой горе стоял маленький домик из старого деревца, а в домике этом жил одинокий, злой волшебник Максишебник — тот еще ненавистник женского пола." ]]
                p [[ img "https://media.tenor.com/h5Eq_6ia4IIAAAAC/unpacking-wizard.gif" "wizard" "wizard" ]]
            ]

            say [
                p [[ text "В народе ходили легенды о таинственно волшебнике, но никто не знал, существует ли он на самом деле." ]]
            ]

            say [
                p [[ text "Разные народы называли его по-разному: Максищи, Максиборщ, Максисельдь. Но в могуществе его никто не сомневался, и все его побаивались." ]]
            ]

            say [
                p [[ text "Некоторые даже верили, что именно этот волшебник меняет погоду, управляет стихиями, посевом и чуть ли не стоит за всеми явлениями природы!" ]]
            ]

            say [
                p [[ text "Пока народ боялся волшебника, тот спокойно жил на своей горе в маленьком домике и был счастлив." ]]
            ]

            bluetoothSay [
                p [[ text "А почему злой и могущественный волшебник живёт в маленькой халупе?)" ]]
            ]

            surpriseSay [
                p [[ text "А ну цыц!" ]]
            ]

            bluetoothSay [
                p [[ text "Слушаюсь, моя госпожа!" ]]
                p [[ img "https://cdn.discordapp.com/attachments/927554008263032836/1015483731554877471/22-10-31-16-00-48-20-28-34-13-49-05-21-43-30-23-41-05-20-05-05-844832.jpg" "" "" ]]

            ]

            say [
                p [[ text "Так вот, в далеком городе Носвегас один из местных жителей по имени Агентарито заговорил о том самом волшебнике, чем вызвал немалый интерес у народа. Агениарито говорил, что раз этот волшебник столь могуч, то и желания сможет исполнить каждого жителя городка." ]]
            ]

            say [
                p [[ text "Страх многих людей так и не отступил — решили не трогать зло. Тогда Агентарито собрал самую храбрую, предприимчивую группу путешественников." ]]
            ]

            say [
                p [[ text "В состав группы вошли:" ]]
                ul [
                    li [
                        text "Адасиринити (которую ласково называли Сиря) — местная странная шаманка;"
                    ] []
                    li [
                        text "Виосиси (коротко Сиси) — местная странная ясновидящая;"
                    ] []
                    li [
                        text "Ну и сам Агентарито — местный странный диванный критик, домосед, бездельник и просто любопытный человек, которому не сидится дома."
                    ] []
                ]
            ]

            say [
                p [[ text "В Носвегасе начали провожать с почестями группу, как в толпе закричал странный, противный, писклявый голос: «Сто-о-о-ойте, сто-о-о-ойте!»" ]]
                p [[ text "Народ расступился, и увидел ещё одного странного местного добровольца…"]]
            ]

            say [
                p [[ text "«О-о-о не-е-е-ет! — подумала группа героев: — Это же местная сваха Светсябитова!»" ]]
            ]

            say [
                p [[ text "— О-о-о да-а-а! — возликовал народ: наконец-то заживут спокойно!"]]
            ]

            say [
                p [[ text "Проводили с почестями из Носвегаса наших героев." ]]
            ]

            say [
                p [[ text "И молвил наш бездельник: «По коням!», — и дамы послушались диванного критика и сели рядом."]]
                p [[ img "https://cdn.discordapp.com/attachments/927554008263032836/1142190832682811493/3217733035.jpg" "" "" ]]
            ]

            agentSay [
                p [[ text "<:ohMy:945179464084647946>" ]]
            ]

            surpriseSay [
                p [[ text "Поправочка — не настолько рядом, как бы того хотел диванный критик, он же бездельник." ]]
            ]

            agentSay [
                p [[ text "<:catSad:1092657743137083413>" ]]
            ]

            say [
                p [[ text "Так вот, едут час, другой… Скучно стало Светсябитовой, начала вопросы задавать единственному мужчине в дороге."]]
            ]

            agentSay [
                p [[ text "<:pepeRee:958453248019267655>" ]]
            ]

            say [
                p [[ text "Вопросы сыпались из уст ее разные, но в основном по профессии." ]]
            ]

            say [
                p [[ text "В ходе разговора Светсябитова заключила диагноз Агентари: все его проблемы из-за того, что он не женат." ]]
            ]

            say [
                p [[ text "Дальше взор прекрасной Светсябитовой упал на скромно молчавших Сирю и Сиси." ]]
            ]

            agentSay [
                p [[ text "Сейчас и этим достанется! <a:pepeHaha:1142351326911660113>" ]]
            ]

            say [
                p [[ text "Выяснилось, Сиси была уже в браке и верна другому." ]]
            ]

            say [
                p [[ text "Но малышка Сиря хлопая своими прекрасными глазами как бы подмигивала — согласна. Так спустя два часа в дороге зародилась новая семья с фамилией Агентосиря." ]]
            ]

            adalindaSay [
                p [[ text "<:pepeRee:958453248019267655>" ]]
            ]

            agentSay [
                p [[ text "<:pepeRee:958453248019267655>" ]]
            ]

            bluetoothSay [
                p [[
                    text "Прям на телеге?"
                ]]
            ]

            surpriseSay [
                p [[ text "Потому что!" ]]
            ]

            say [
                p [[ text "Так вот, дорога была долгая, непростая..." ]]
            ]

            agentSay [
                p [[ text "И Светсябитова приложила все усилия, чтобы облегчить ее! <:pepeRee:958453248019267655>"]]
            ]

            say [
                p [[ text "Вдруг, откуда невозьмись подул сильный ветер «Бумявет», который всё сносил на своём пути!" ]]
            ]

            say [
                p [[ text "Запаниковала Сиси: «Это же могут быть проделки волшебника!»" ]]
            ]

            say [
                p [[ text "Но храбрый и уже женатый Агенторито прикрыл своим могучим телом девчонок!" ]]
            ]

            say [
                p [[ text "Наши путешественники снова готовы ехать, но нет, снова испытание — затряслась земля, застучал копытом конь..." ]]
            ]

            say [
                p [[ text "И уже летит огромная птица Буляпуля на наших героев!" ]]
            ]

            say [
                p [[ text "В неравной схватке с птицей пострадал наш герой Агентарито — его птица сильно клюнула. Увидела жена о случившимся, разозлилась…"]]
            ]

            say [
                p [[ text "Визг, писк, но Сиря не сдаётся… прогнала птицу!" ]]
            ]

            say [
                p [[ text "Возгордился путешественник! <:satan:933647316521463858>" ]]
            ]

            say [
                p [[ text "Наши герои готовы к дороге, они продолжают свой путь." ]]
            ]

            say [
                p [[ text "Поехали наши герои дальше. Сдружились они за всё это время, ведь столько всего на свете прошли: ветер, птицу, свадьбу..." ]]
            ]

            say [
                p [[ text "Смотрят — впереди лес тёмный, страшный!" ]]
            ]

            say [
                p [[ text "«Наконец-то, — подумали они. — Приехали»."]]
            ]

            say [
                p [[ text "Едут тихо, не спешат, мало ли какие подвохи могут быть в столь темном лесу." ]]
            ]

            say [
                p [[ text "Едут, никуда не сворачивают, боятся. И тут слышат шорох на деревьях…" ]]
            ]

            say [
                p [[ text "— О-о-о-о-о нет!, — крикнула Сиря." ]]
                p [[ img "https://media.discordapp.net/attachments/859253126878724126/1090804061676244992/output.gif" "drama queen" "drama queen" ]]
            ]

            say [
                p [[ text "— Страшно!, — крикнула Сиси."]]
            ]

            say [
                p [[ text "«Это же большая тетя Совинита!» — сказала Светсябитова." ]]
            ]

            say [
                p [[ text "Довела Совенита наших героев до горы волшебной и..." ]]
            ]

            agentSay [
                p [[ text "Подожди! Чего-то не хватает между" ]]
                p [[ text "> «Это же большая тетя Совинита» — сказала Светсябитова" ]]
                p [[ text "и" ]]
                p [[ text "> Довела Совенита наших героев" ]]
                p [[ text "<:pepeRee:958453248019267655>" ]]
            ]

            surpriseSay [
                p [[ text "Это материал для додумывания: не мне ж одной сказку на себе тащить <:pepeSmile:947129401785593978>" ]]
            ]

            sobenokSay [
                p [[ text "Чет сказочка у вас какая то странная 🤔" ]]
                p [[ img "https://cdn.discordapp.com/attachments/927554008263032836/1142202842891943956/2641950.jpg" "" "" ]]
            ]

            surpriseSay [
                p [[ text "Так, не мешайте творцу творить! <:pepeRee:958453248019267655>" ]]
            ]

            say [
                p [[ text "Так вот, довела Совенита наших героев до горы волшебной и промолвила:" ]]
                p [[ text "— Дальше пешком пойдёте. Вас там встретят два волшебных столба, их прикупить надо, чтобы впустили. Тот что Алексир — достанете рюмочку чачи заморской, а тот что Шейсир — музыку включите классическую." ]]
            ]

            say [
                p [[ text "Приблизились наши отважные ребята к горе, а на ней злой волшебник восседает, внизу столбы не пускают." ]]
            ]

            say [
                p [[ text "— Музыку включить? Легко! — сказала Светсябитова и достала из кармана плеер с ее любимой песней «Рюмка водки на столе»" ]]
                p [[ img "https://media.tenor.com/mA4XKYoYKAQAAAAd/shot-glass-vodka.gif" "только рюмка водки на столе" "только рюмка водки на столе" ]]
            ]

            agentSay [
                p [[ text "Плеер..." ]]
            ]

            say [
                p [[ text "— Но где же брать чачу? — растерялись наши авантюристы." ]]
            ]

            agentSay [
                p [[ text "Т.е. плеер найти в средневековой сказке с волшебниками и говорящими совами — эт как два пальца, а чачу найти — так бида <:pepeRee:958453248019267655>" ]]
            ]

            sobenokSay [
                p [[ text "Конечно, беда! Как все эти существа ее гнать-то будут?! 😂"]]
            ]

            surpriseSay [
                p [[ text "Не ваше дело! <:pepeSmile:947129401785593978>" ]]
            ]

            say [
                p [[ text "Так вот, смотрят они, а глаза у Сиси сверкают… Оказалось, увлекалась она втихую рюмашечками вмазать. Протянула она рюмку чачи, и столбы впустили наших героев" ]]
            ]

            bluetoothSay [
                p [[ text "А откуда чача взялась у неё?" ]]
            ]

            sobenokSay [
                p [[ text "Сова притащила." ]]
            ]

            surpriseSay [
                p [[ text "Говорю же: любила рюмашечку пригубить. С собой было." ]]
            ]

            agentSay [
                p [[ text "Т.е. она всю дорогу ехала с Агентарито, Светсябитовой и Адасиринити в повозке, бродила с ними по горам и лесам, а потом достала рюмашку из кармана и хряпнула?! <:pepeRee:958453248019267655>"]]
            ]

            surpriseSay [
                p [[ text "Вот представь, какие неблагодарные попутчицы бывают! <:pepeSmile:947129401785593978>" ]]
            ]

            say [
                p [[ text "Так вот!!! Забрались в гору. Наши герои видят перед собой не такого уж и страшного человека, как рассказывали легенды… Рост — полторашка, щупленький, бледнокожий мужчинка в самом расцвете сил." ]]
            ]

            say [
                p [[ text "Увидела его Светябитова и со словами «Откормить, отмыть» полезла обниматься." ]]
            ]

            say [
                p [[ text "Растаял волшебник, подобрел." ]]
            ]

            say [
                p [[ text "Сиси рюмашечкой угостила." ]]
            ]

            say [
                p [[ text "А молодая семья, что была в дороге создана..."]]
            ]

            agentSay [
                p [[ text "<:pepeRee:958453248019267655>" ]]
            ]

            say [
                p [[ text "...согрела волшебника своей улыбкой." ]]
            ]

            say [
                p [[ text "Так наш волшебник выбрал себе спутницу жизни, подобрел и стал уважительнее относиться к женщинам." ]]
            ]

            say [
                p [[ text "Наши герои смогли растопить лёд в Максишебнике своей добротой, духом авантюризма." ]]
            ]

            bluetoothSay [
                p [[ text "Авантюризма было хоть отбавляй) Шаман ясновидящая и бездельник — очаровательная компания) Но насколько они должны были поверить в себя, чтобы такой компанией пойти в лоб на могущественного волшебника?)" ]]
            ]

            say [
                p [[ text "Наш волшебник тугенький, и мысли приходят не с первого раза, но мы верим в нашего волшебника, верим, что у него всё сложится. Пусть с 250-го раза, но всё сложится! Конец!" ]]
            ]
        ]
    ]

let (scenario: Scenario<CommonContentWithNarrator, Label, CustomStatement>) =
    [
        let rec preludeLoc = nameof preludeLoc
        label Menu [
            surpriseMenu [
                p [[ text "Привет, путник! Какую сказку хочешь послушать? "]]
            ] [
                choice "Про короля" [
                    jump StartTaleAboutKing
                ]

                choice "Про кузнеца" [
                    jump StartTaleAboutSvarshik
                ]

                choice "Про волшебника" [
                    jump StartTaleAboutWizard
                ]
            ]
        ]

        yield! taleAboutKing

        yield! taleAboutWizard

        yield! taleAboutSvarshik
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList
    : Scenario<_, _, _>

open IfEngine.Engine
type State = State<CommonContentWithNarrator, Label>
type CustomStatementOutput = unit
type Engine = Engine<CommonContentWithNarrator, Label, CustomStatement, CustomStatementArg, CustomStatementOutput>

let initGameState: State =
    State.init
        beginLoc
        Map.empty

let create (state: State) : Result<Engine, string> =
    Engine.create
        CustomStatementHandler.empty
        scenario
        state
