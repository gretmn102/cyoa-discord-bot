module SurpriseTales
open IfEngine
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open IfEngine.SyntaxTree.CommonContent
open IfEngine.SyntaxTree.CommonContent.Helpers
open Farkdown.Experimental.Helpers
open FsharpMyExtension.ResultExt

type CustomStatement = unit
type CustomStatementArg = unit

type Label =
    | Menu
    | StartTaleAboutWizard

let beginLoc: Label = Menu

let bluetoothSay content =
    say [
        h1 [ text "Максишебник" ] [
            yield! content
            yield p [[ img "https://cdn.discordapp.com/avatars/884492693475053639/d6a06abf4c3458f29ff32bde06dec390.webp?size=48" "" "" ]]
        ]
    ]

let surpriseContent content =
    [
        h1 [ text "Сказочница" ] [
            yield! content
            yield p [[ img "https://cdn.discordapp.com/avatars/807631911131807765/716cf4e01b5450e4e39de93bb9aaa3e7.webp?size=48" "Surprise" "Surprise" ]]
        ]
    ]

let surpriseMenu content choices =
    menu
        (surpriseContent content)
        choices

let surpriseSay content =
    say (surpriseContent content)

let agentSay content =
    say [
        h1 [ text "Агентарито" ] [
            yield! content
            yield p [[ img "https://cdn.discordapp.com/avatars/796931597898088448/e9c47d2b4a6e14797d1d348e69a33836.webp?size=48" "" "" ]]
        ]
    ]

let adalindaSay content =
    say [
        h1 [ text "Адасиринити" ] [
            yield! content
            yield p [[ img "https://cdn.discordapp.com/avatars/572010412157960192/407a80311750dc55d6d4abe188c545b4.webp?size=48" "" "" ]]
        ]
    ]

let sobenokSay content =
    say [
        h1 [ text "СОВЕНОК" ] [
            yield! content
            yield p [[ img "https://cdn.discordapp.com/guilds/927554008263032832/users/516737047147446272/avatars/a25b7a033b212f52ff822da145c47201.webp?size=48" "" "" ]]
        ]
    ]

let (scenario: Scenario<Content, Label, CustomStatement>) =
    [
        let rec preludeLoc = nameof preludeLoc
        label Menu [
            surpriseMenu [
                p [[ text "Привет, путник! Какую сказку хочешь послушать? "]]
            ] [
                choice "Про короля" [
                    say [
                        p [[ text "здесь должна быть сказка про кузнеца, но Агент ее еще не сделал." ]]
                    ]
                    jump Menu
                ]

                choice "Про кузнеца" [
                    say [
                        p [[ text "здесь должна быть сказка про кузнеца, но Агент ее еще не сделал." ]]
                    ]
                    jump Menu
                ]

                choice "Про волшебника" [
                    jump StartTaleAboutWizard
                ]
            ]
        ]

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
                p [[ text "В неравной схватке с птицей пострадал наш герой Агентарито - его птица сильно клюнула. Увидела жена о случившимся, разозлилась…"]]
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
                p [[ text "> «Это же большая тетя Совинита» - сказала Светсябитова" ]]
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
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList
    : Scenario<_, _, _>

open IfEngine.Engine
type State = State<Content, Label, CustomStatement>
type CustomStatementOutput = unit
type Engine = Engine<Content, Label, CustomStatement, CustomStatementArg, CustomStatementOutput>

let initGameState: State =
    State.init
        beginLoc
        Map.empty

let create (state: State) : Result<Engine, string> =
    Engine.create
        CustomStatementHandler.empty
        scenario
        state
