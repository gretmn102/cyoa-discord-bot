module Cyoa.Model
open FsharpMyExtension
open MongoDB.Driver
open MongoDB.Bson

open Types
open Db

module MoraiGame =
    open IfEngine.Utils
    open IfEngine.Types
    open IfEngine.Interpreter
    open IfEngine
    open IfEngine.Discord.Utils

    type CustomStatement = unit
    type CustomStatementArg = unit

    type LabelName = string

    let mainMenuLoc = "MainMenu"
    let beginLoc: LabelName = mainMenuLoc

    let (scenario: Label<Text, LabelName, CustomStatement> list), vars =
        let vars = Map.empty
        let getLoseCounter, updateLoseCounter, vars = createNumVar "losesCount" 0 vars

        [
            let rec preludeLoc = nameof preludeLoc
            label mainMenuLoc [
                menu [
                    "Морайские сказки"
                    "Малюсенький интерактивный рассказ."
                    "v1.0"
                ] [
                    choice "Начать" [ jump preludeLoc ]
                    choice "Авторов!" [
                        menu [
                            "Морай (<@749610214948339732>) — сказочник"
                            "Агент (<@796931597898088448>) — разработчик бота"
                        ] [
                            choice "Назад" [ jump mainMenuLoc ]
                        ]
                    ]
                ]
            ]

            let rec continueSleep = nameof continueSleep
            let rec pressMoraiForContinue = nameof pressMoraiForContinue

            label pressMoraiForContinue [
                say "Конец! Пинайте Морая (<@749610214948339732>), чтобы дописал историю!"
            ]

            label preludeLoc [
                menu [
                    "Пушок проснулся рано. За дверь каюты ещё не было слышно привычных шагов, а в иллюминатор только начал пробираться рассеянный свет."
                    "Под одеялом было так тепло, что не хотелось никуда вылезать и встал вопрос..."
                ] [
                    choice "Вылезти из-под одеяла (todo)" [
                        jump pressMoraiForContinue
                    ]
                    choice "Остаться досыпать" [
                        jump continueSleep
                    ]
                ]
            ]

            let respond = "откликнуться"

            label continueSleep [
                menu [
                    "Поворочавшись немного, котик снова уснул и увидел чудесный сон об сытном и вкусно обеде за приятными разговорами."
                    "Но утро никого не щадит и котик постепенно просыпается под шаги за дверь за несколько секунд до того, как стучат в дверь..."
                ] [
                    choice "Откликнуться" [
                        jump respond
                    ]
                    choice "Сделать вид, что ещё спишь (todo)" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let прогнатьАгента = "прогнатьАгента"

            label respond [
                menu [
                    "- Ну чего там? - ворчит сонным голосом Пушок."
                    "- Все уже проснулись и собрались завтракать. Я подумал, что ты захочешь позавтракать с нами, - заглядывая в дверь, ответил Агент (пусть пока будут наши имена. Типично кошачьи сейчас в голову не идут). - К тому же тут с утра такое случилось."
                    "Агент просачивается внутрь каюты и выжидательно смотрит на Пушка, ожидая вопросов."
                ] [
                    choice "Спросить, что там (todo)" [
                        jump pressMoraiForContinue
                    ]
                    choice "Прогнать Агента и спокойно собираться" [
                        jump прогнатьАгента
                    ]

                ]
            ]

            let rec поднятьсяНаПалубу = nameof поднятьсяНаПалубу

            label прогнатьАгента [
                says [
                    "- Ой, иди ты. Дай мне сначала нормально проснуться!"
                    """Агент меняется в лице и выходит из каюты, бросая через плечо "Встретимся позже"."""
                ]
                menu [
                    "Пушок тем временем ещё некоторое время валяется в кровати, слушая топот и неразборчивые голоса. Потом поднимается с кровати, приводит себя в порядок и открывает дверь, намереваясь выйти."
                ] [
                    choice "Идти направо и подняться на палубу" [
                        jump поднятьсяНаПалубу
                    ]
                    choice "(todo) Идти налево и встретиться с Агентом в столовой" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec спроситьУШедоу = nameof спроситьУШедоу
            let rec идтиНаслаждатьсяВоздухом = nameof идтиНаслаждатьсяВоздухом
            label поднятьсяНаПалубу [
                menu [
                    "Решив, что для завтрака ещё рано и не помешает понюхать свежий морской воздух, Пушок отправляется на палубу, встретив уже почти у выхода Шэдоу. Он весьма мрачен и погружен в себя и это вызывает вопросы."
                ] [
                    choice "Спросить у Шэдоу что случилось" [
                        jump спроситьУШедоу
                    ]
                    choice "Не обращать внимания и идти наслаждаться воздухом" [
                        jump идтиНаслаждатьсяВоздухом
                    ]
                ]
            ]

            let rec goToKitchen = nameof goToKitchen

            label спроситьУШедоу [
                menu [
                    "На вопрос что стряслось, Шэдоу фокусирует свой взгляд на Пушке и с чувством выдаёт, кладя руку на плечо:"
                    "- Никогда не женись!"
                    "После этой впечатляющей и непонятной фразы он удаляется по коридору, не реагируя на дальнейшие оклики."
                ] [
                    choice "(todo) Подняться на палубу" [
                        jump pressMoraiForContinue
                    ]
                    choice "Всё-таки пойти в столовую" [
                        jump goToKitchen
                    ]
                ]
            ]

            let rec идтиЗаЕдой = nameof идтиЗаЕдой

            label goToKitchen [
                menu [
                    "Решив, что новости Агента могут касаться Шэдоу, Пушок все же направляется в столовую."
                    "Там стоит гул голосов, котики сидят компаниями и едят, читают, слушают музыку или рисуют."
                    "Недалеко от раздачи сидит Агент, который занят разговором с Адалиндой. Та явно на взводе и что-то очень торопливо и эмоционально говорит."
                    "Увидев Пушка, Агент соскакивает с места и идёт навстречу, ловко лавируя между всеми и перекидываясь приветствиями."
                    "- Ты не представляешь, что тут всех переполошило в пять утра, пока ты сладко спал!"
                ] [
                    choice "(todo) Спросить, что же всё-таки случилось" [
                        jump pressMoraiForContinue
                    ]
                    choice "Идти за едой, ведь Агент и так все расскажет" [
                        jump идтиЗаЕдой
                    ]
                ]
            ]

            label идтиЗаЕдой [
                says [
                    "Не обращая на явное ожидание вопроса, Пушок идёт к еде и начинает собирать свой завтрак, попутно слушая историю о том, что Адалинда проснулась с утра пораньше в игривом настроении и решила поделиться им со спящим рядом Шэдоу путем куся за бочок."
                    "На который тот отреагировал далеко не так, как она ожидала."
                    "История закончилась тем, что в половину шестого утра эта парочка подняла весь этаж на уши своей руганью и воплями."
                ]

                jump pressMoraiForContinue
            ]

            let rec поздороватьсяССолом = nameof поздороватьсяССолом


            label идтиНаслаждатьсяВоздухом [
                menu [
                    "Свежий морской воздух приятно бодрит после застойного воздуха кают."
                    "Вдалеке виднеются острова, вокруг с криками летают несколько чаек."
                    "На краю палубы, оперевшись на перила и задумчиво смотря вдаль, стоит Сол."
                ] [
                    choice "Подойти поздороваться" [
                        jump поздороватьсяССолом
                    ]
                    choice "(todo) Найти уголочек и побыть наедине с самим собой" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec помолчать = nameof помолчать

            label поздороватьсяССолом [
                menu [
                    "Сол поворачивается и здоровается с лёгкой улыбкой. Его хорошее настроение служит отличным дополнением к приятному ветерку и некоторое время повисает уютная тишина."
                    "- А ты во сколько встал? - наконец спрашивает Пушок."
                    "- Да я ещё и не ложился, - все так же умиротворенно отвечает Сол, созерцая зелень на горизонте."
                ] [
                    choice "(todo) Спросить, почему же он не ложился" [
                        jump pressMoraiForContinue
                    ]
                    choice "Вместе помолчать" [
                        jump помолчать
                    ]
                    choice "(todo)Пойти в столовую" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Вернуться в каюту" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec бытьВПервыхРядах = nameof бытьВПервыхРядах

            label помолчать [
                menu [
                    "Это утро приносит Пушку поистине умиротворенное настроение. "
                    "Мысли медленно переходят с одного на другое, все видится если не простым, то решаемым."
                    "И вот, когда уже захотелось есть, Сол неожиданно говорит:"
                    "- Наверно, стоит провести мероприятие для наших котиков."
                ] [
                    choice "(todo)Спросить, что привело его к таким пожеланиям" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Спросить, какое именно мероприятие" [
                        jump pressMoraiForContinue
                    ]
                    choice "Выразить желание быть в первых рядах" [
                        jump бытьВПервыхРядах
                    ]
                    choice "(todo)Просто промолчать" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec турнир = nameof турнир

            label бытьВПервыхРядах [
                menu [
                    "Сол улыбается."
                    "- Это отлично. Осталось решить, что именно будем делать. Может постановку какую-нибудь сделаем? Или устроим турнир по играм? А может танцы?"
                    "Видно, что ему приносит удовольствие придумывать что-то."
                ] [
                    choice "(todo)Конечно спектакль!" [
                        jump pressMoraiForContinue
                    ]
                    choice "Турнир звучит неплохо." [
                        jump турнир
                    ]
                    choice "(todo)Танцы подойдут почти всем." [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Может, проведем голосование?" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec goToKitchen2 = nameof goToKitchen2
            label турнир [
                menu [
                    "- Думаешь? Надо тогда определиться, по каким играм да и чем награждать. А главное, чтобы было где в это время сидеть."
                    "Стараясь учесть все, Сол прощается, пожав руку на прощанье, и уходит, вполголоса размышляя о том, что же ему надо сделать и как это все провернуть."
                ] [
                    choice "(todo)Остаться на месте" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Пройтись по палубе" [
                        jump pressMoraiForContinue
                    ]
                    choice "Пойти в столовую" [
                        jump goToKitchen2
                    ]
                    choice "(todo)Вернуться в каюту" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec поговоритьСДаней = nameof поговоритьСДаней

            label goToKitchen2 [
                menu [
                    "В прекрасном расположении духа Пушок спускается в коридор, а затем идёт в столовую. Окидывая взглядом помещение, выясняет, что Агент уже позавтракал и куда-то улетучился."
                    "Взяв еду, Пушок встал перед вопросом..."
                ] [
                    choice "(todo)Сесть за ближайший столик, не обращая внимания, что там сидит Даня" [
                        jump pressMoraiForContinue
                    ]
                    choice "Сесть за ближайший столик и поговорить с Даней" [
                        jump поговоритьСДаней
                    ]
                    choice "(todo)Поискать более свободное место" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec goToLibrary = nameof goToLibrary

            label поговоритьСДаней [
                menu [
                    "Пушок подсаживается к Дане, здоровается и начинает есть. Над столом повисает тишина. Даня занят какими-то мыслями и расчетами настолько, что не обращает внимания на происходящее вокруг и ковыряется в супе, раскладывая вермишель строго паралельно друг другу."
                    "*Мда, кажется, Даня немного не в духе*"
                ] [
                    choice "(todo)Доедать свою еду и возвращаться в каюту" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Доесть и посидеть, вслушиваясь в чужие разговоры" [
                        jump pressMoraiForContinue
                    ]
                    choice "Доесть и сходить в библиотеку" [
                        jump goToLibrary
                    ]
                    choice "(todo)Доесть и пойти на палубу" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec зависнутьРина = nameof зависнутьРина

            label goToLibrary [
                menu [
                    "Доев свой завтрак и чинно прибрав за собой, Пушок выходи из столовой с намерением пойти в библиотеку."
                    "Путь пролегает через палубу, где его окликает Рина."
                    "- Не хочешь поучаствовать в одной интересной штуке?"
                ] [
                    choice "(todo)Остановиться, подумать, отказаться и пойти дальше" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Остановиться, подумать и согласиться" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Сделать вид, что ничего не слышал и подниматься куда шел" [
                        jump pressMoraiForContinue
                    ]
                    choice "Зависнуть в мыслях о том, что же такого может предложить Рина" [
                        jump зависнутьРина
                    ]
                ]
            ]

            let rec кабачокСоветуетсяССолом = nameof кабачокСоветуетсяССолом

            label зависнутьРина [
                menu [
                    "*Прошлое ее предложение закончилось беганием по палубе в кофтах на ноги. Не понимаю, как эти бредовые мысли приходят ей в голову. У меня даже случайно подобные идеи не могут за...*"
                    "- Эй, ты в порядке?"
                    "Рина трогает Пушка за плечо, заставляя вынырнуть из своих мыслей и напряжённых попыток предсказать идею."
                    "- Нет. Так что у тебя там за идея?"
                    "- Ага <:catPleased:1041855910626213949>. Я знала, что тебе будет интересно! Вот смотри..."
                ] [
                    choice "(todo)...мы берём 100 удочек, одеваем на крючок разные штуки и на одну золотое кольцо. А потом закидываем все за борт и играем в лотерею!" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)...хочу на той неделе устроить концерт и ищу таланты. Ты у нас не поешь/танцуешь?" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)...вечер настольных игр! Прикинь как будет прикольно!" [
                        jump pressMoraiForContinue
                    ]
                    choice "Эммм... Кажется, сначала надо будет посоветоваться с Солом." [
                        jump кабачокСоветуетсяССолом
                    ]
                ]
            ]

            let rec пройтисьВдольПолок = nameof пройтисьВдольПолок

            label кабачокСоветуетсяССолом [
                menu [
                    "Неожиданно задумавшись и что-то бормоча, Рина отходит в сторону. Жестикуляция и споры сам с собой выглядят странно, но это же Рина. Пожав плечами, пушок поднимается ещё на пролет и оказывается этажом выше. Короткий коридор ведёт к двустворчатым прозрачным дверям, за которыми прячется библиотека."
                    "Зайдя в нее, Пушок оглядывается, размышляя о том, зачем же он сюда пришел."
                ] [
                    choice "(todo)Пройти и присесть за стол у иллюминатора" [
                        jump pressMoraiForContinue
                    ]
                    choice "Пройтись вдоль полок и поглазеть." [
                        jump пройтисьВдольПолок
                    ]
                    choice "(todo)Подождать библиотекаря" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec поразглядыватьКниги = nameof поразглядыватьКниги

            label пройтисьВдольПолок [
                menu [
                    "Путешествие вдоль полок приносит Пушку удовольствие. Судя по названиям книг, ему попадается отдел с книгами по мотивации и психологии. Заголовки \"Сделай себя с нуля\", \"Волшебный пендель, который вы ждали\", популярное \"Ни ссы!\" Заставляют улыбнуться. Ему уже давно кажется, что все мотивационная литература это сплошной обман."
                ] [
                    choice "(todo)Взять книгу не глядя" [
                        jump pressMoraiForContinue
                    ]
                    choice "Поразглядывать" [
                        jump поразглядыватьКниги
                    ]
                    choice "(todo)Пойти дальше" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec пройтисьВдольПолок = nameof пройтисьВдольПолок

            label поразглядыватьКниги [
                menu [
                    "- \"Будь дерзким!\", \"Будь без обид!\", \"Волшебные точки счастья\", \"Ключ к познанию себя\", \"Противоядие от несчастливой жизни\"... Названия одно другого лучше..."
                    "Ни одно не вызывало желания взять его в руки. Среди ярких обложек красовался тёмно-зеленый корешок с золотистыми буквами Зигмунд Фрейд."
                    "- Ну сейчас, ага. Только Фрейда в моей жизни не хватало ..."
                    "Переводя взгляд на другую полку, Пушок размышляет о том, что современная психология не для него."
                ] [
                    choice "(todo)Пойти между следующими стеллажами с надписью *Культура*" [
                        jump pressMoraiForContinue
                    ]
                    choice "Пройтись вдоль окон и просто посмотреть, нет ли кого между стеллажами." [
                        jump пройтисьВдольПолок
                    ]
                    choice "(todo)Вернуться и почитать журнальчики за столом" [
                        jump pressMoraiForContinue
                    ]
                ]
            ]

            let rec привлечьВнимание = nameof привлечьВнимание

            label пройтисьВдольПолок [
                menu [
                    "Решив, что беглый осмотр все же лучше блужданий, Пушок пошел вдоль окон, просматривая надписи. Не то, чтобы он прямо пылал желанием найти кого-то. Это было сделано скорее от скуки."
                    "Между стеллажами с надписью Биология были замечены Мур и Эля. Они что-то тихо, но очень горячо обсуждали, тыкая пальцами в страницы."
                    "Решив, что спрашивать у них что-то себе дороже, Пушок движется дальше."
                    "Где-то между 4 и 5 стеллажами с художественной литературой, прямо на полу сидит Морай и, опершись спиной на полки, увлеченно читает книгу \"Богиня гнева\"."
                ] [
                    choice "(todo)Идти дальше" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Вернуться и послушать жаркие споры биологов" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Посмотреть что-то интересное рядом с Мораем" [
                        jump pressMoraiForContinue
                    ]
                    choice "Привлечь внимание и попросить совета касательно книги" [
                        jump привлечьВнимание
                    ]
                ]
            ]

            let rec почемуСидитеНаПолу = nameof почемуСидитеНаПолу

            label привлечьВнимание [
                menu [
                    "- Кхм..."
                    "Тактичный кашель Пушка заставляет Морая поднять глаза от книги."
                    "- Конечно. Я вас слушаю."
                    "Отложив книгу, библиотекарь потянулся, выгнувшись как настоящий кот."
                    "Некоторое время Пушок молчал, пытаясь сформировать свою просьбу."
                ] [
                    choice "(todo)Не поможете мне выбрать что-нибудь из художественной литературы?" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Что у вас есть интересного из психологии?" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Хотелось бы почитать комиксы." [
                        jump pressMoraiForContinue
                    ]
                    choice "Почему вы сидите на полу?" [
                        jump почемуСидитеНаПолу
                    ]
                ]
            ]

            label почемуСидитеНаПолу [
                menu [
                    "- Какой интересный вопрос, - Морай улыбается. - Какие книги вы предпочитаете? Или, может, мне стоит порекомендовать вам что-то по своему в..."
                    "Довольный монолог прерывает оглушительный грохот."
                    "Изменившись в лице, Морай мгновенно исчезает где-то между стеллажей."
                ] [
                    choice "(todo)Полистать книгу, которую он читал" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Подождать и посмотреть, что вокруг" [
                        jump pressMoraiForContinue
                    ]
                    choice "(todo)Идти искать источник шума и выяснить, что же случилось." [
                        jump pressMoraiForContinue
                    ]
                ]
            ]
        ]
        |> fun scenario ->
            scenario, vars

    type State = State<Text, LabelName, CustomStatement>

    type Msg = Game.Msg<CustomStatement, CustomStatementArg>
    type Command = Command<Text, LabelName, CustomStatement, CustomStatementArg>

    type Game =
        {
            InitState: State
            InitCommand: Command
            Update: Msg -> State -> (Command * State)
        }

    let game: Game =
        let scenario =
            scenario
            |> List.map (fun (labelName, body) -> labelName, (labelName, body))
            |> Map.ofList
            : Scenario<_, _, _>

        let initGameState: State =
            {
                LabelState =
                    LabelState.create
                        beginLoc
                        (Stack.createSimpleStatement 0)
                Vars = vars
            }

        let interp (gameState: State) =
            let addon state stack (isWin: CustomStatementArg) (customStatement: CustomStatement) =
                failwith "custom statement"

            let handle subIndex (customStatement: CustomStatement) =
                failwithf "handle"

            gameState
            |> interp (addon, handle) scenario
            |> function
                | Ok x -> x
                | Error err ->
                    failwithf "%s" err

        let update (msg: Msg) (state: State) =
            let gameState init =
                {
                    Game.Game = interp init
                    Game.GameState = init
                    Game.SavedGameState = init
                }
            let x = Game.update interp initGameState msg (gameState state)
            (x.Game: Command), (x.GameState: State)

        {
            InitState = initGameState
            InitCommand = interp initGameState
            Update = update
        }

module Users =
    type GuildData =
        {
            GameState: MoraiGame.State option
        }
        static member Init gameState : GuildData =
            {
                GameState = gameState
            }
        static member Empty =
            {
                GameState = None
            }
        static member Serialize (data: GuildData) =
            data |> Json.ser
        static member Deserialize json =
            try
                let x: GuildData = Json.des json
                Ok x
            with e ->
                Error e.Message

    type Version =
        | V0 = 0

    type Id = UserId

    type Guild = CommonDb.Data<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guild =
        let create id data: Guild =
            CommonDb.Data.create id Version.V0 data

    type Guilds = CommonDb.GuildData<Id, Version, GuildData>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Guilds =
        let createData id =
            Guild.create id GuildData.Empty

        let init collectionName (db: IMongoDatabase): Guilds =
            // todo: make IfEngine.Interpreter.State serialize correctly
            // CommonDb.GuildData.init
            //     createData
            //     (fun ver doc ->
            //         match ver with
            //         | Some ver ->
            //             match ver with
            //             | Version.V0 ->
            //                 None, Serialization.BsonSerializer.Deserialize<Guild>(doc)
            //             | x ->
            //                 failwithf "Version = %A not implemented\n%A" x doc
            //         | None ->
            //             failwithf "Version is empty:\n%A" doc
            //     )
            //     collectionName
            //     db
            let collection = db.GetCollection<BsonDocument>(collectionName)
            {
                Cache = Map.empty
                Collection = collection
            }

        let set userId setAdditionParams (guildData: Guilds) =
            // todo: make IfEngine.Interpreter.State serialize correctly
            // CommonDb.GuildData.set
            //     createData
            //     userId
            //     setAdditionParams
            //     guildData
            let cache = guildData.Cache

            let id = userId

            {
                guildData with
                    Cache =
                        match Map.tryFind id cache with
                        | Some fullData ->
                            let data =
                                { fullData with
                                    Data = setAdditionParams fullData.Data
                                }

                            Map.add id data cache
                        | None ->
                            let x =
                                let x = createData id
                                { x with
                                    Data = setAdditionParams x.Data
                                }

                            Map.add id x cache
            }

        let drop (db: IMongoDatabase) (items: Guilds) =
            CommonDb.GuildData.drop db items

        let tryFindById id (items: Guilds): Guild option =
            CommonDb.GuildData.tryFind id items
