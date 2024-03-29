module JungleTales
open IfEngine
open IfEngine.SyntaxTree
open IfEngine.SyntaxTree.Helpers
open Farkdown.Experimental.Helpers
open FsharpMyExtension.ResultExt
open IfEngine.Discord.SyntaxTree
open IfEngine.Engine

type CustomStatement = unit
type CustomStatementArg = unit

type Label =
    | Menu
    | StartTaleAboutDolls

let beginLoc: Label = Menu

let say content =
    NarratorCommonContent.createSay content

let jungleNarrator =
    Narrator.create
        "Единолошад"
        "https://cdn.discordapp.com/attachments/912291464074117161/1144364493657358406/jungledrum700Avatar.webp"

let jungleSay content =
    NarratorCommonContent.createNarratorSay jungleNarrator content

let jungleMenu content =
    NarratorCommonContent.createNarratorMenu jungleNarrator content

let agentNarrator =
    Narrator.create
        "Агений"
        "https://cdn.discordapp.com/avatars/796931597898088448/e9c47d2b4a6e14797d1d348e69a33836.webp?size=48"

let agentSay content =
    NarratorCommonContent.createNarratorSay agentNarrator content

let taleAboutDolls =
    [
        label StartTaleAboutDolls [
            say [
                p [[ text "В некотором царстве, в некотором государстве жил-был один гений. А гений он был в одном хитром деле — создавал механических кукол он, и те куклы оживали, но не становились живыми — не было у них самого главного... чувства меры." ]]
            ]
            say [
                p [[ text "Гения этого звали... Да так и звали: Агений." ]]
            ]
            say [
                p [[ text "И не было чувства мер у этих гениальных кукол во всём: зайдёт ли клиент к Агению — новую машину (для уборки седых волос жены с пола) прикупить, куклы тут как тут. Шутят они без меры: а для чьей жены, а у жены не парик ли, а седая не от твоих рассказов ли? Клиент, прикупив машинку, в слезах убегал из лавки Агения." ]]
            ]
            say [
                p [[ text "Больше всех не знала меры самая последняя модель куклы, Агений назвал ее модель Светус ~~3 размер~~ 3000. Так и пыталась бесконтрольно сосватать Агению всех и всё: покупательниц, швабры, других кукол, даже комок пыли в дальнем тёмном углу." ]]
            ]
            say [
                p [[ text "Долго сопротивлялся Агений и, наконец, в его гениальную голову пришла мысль, что нужно направить созидательные стремления Светуса 3000 на другую цель. И создал он небольшую, страшненькую механическую куклу, правда, загрузил в нее программное обеспечение по-старинке — через блютуз. Так он и называл это недоразумение — Блютуз (чтобы коротко и понятно)." ]]
            ]
            say [
                p [[ text "Была у Блютуза и своя особенность: если он находил к чему придраться (а это было в 99.9% случаях), то придирался со вкусом, толком, расстановкой и обосновывал оппоненту его недостаток безмерно скучно и занудно." ]]
            ]
            say [
                p [[ text "Светус 3000 резко воодушевилась и с удовольствием переключилась сватать Блютузу всех ранее собранных кукол. Первой ~~под раздачу~~ удостоилась внимания очень красивая куколка, но на дисплее, где должно отображаться ее прелестное лицо светился голубой экран и белыми символами было написано Error404 — Агений уснул во время припаивания каких-то проводов, и не было времени разобраться в этом (он же гений)." ]]
            ]
            say [
                p [[ text "Из-за ошибки эта прелестная куколка не могла ни с кем подружиться, ведь когда она кого-то обнимала, то непременно делала это чрезмерно и у обнимающегося что-то непременно ломалось: например, у самой любимой куклы Агения испортилась мордочка, и после таких объятий улыбчивая жабка стала обладательницей недовольной лягушачьей морды." ]]
                p [[ img "https://cdn.discordapp.com/emojis/952317602594693171.webp?size=1024&quality=lossless" "max" "max" ]]
            ]
            say [
                p [[ text "Агений создал эту жабку в качестве своего помощника, но так как наш гений ко всему подходил с большой ответственностью, то и жабка стала гипертрофированно ответственной." ]]
                p [[ text "Когда какой-то малец хотел купить механизированную руку, то жабка приставала с расспросами: сколько лет, есть ли страхование жизни, какая группа крови, есть ли родственники, которые смогут пожертвовать почку, если их продукт поведет себя некорректно и так далее." ]]
                p [[ text "В особо ретивых жабка запускала тяжелую артиллерию — механизированный не очень современный и такой же интеллектуальный бумеранг." ]]
            ]
            say [
                p [[ text "Так как бумеранг был очень давно собран, во времена бурной молодости Агения, то и был он совершенно не совершенный: из него регулярно вытекало машинное масло, причём сильно переработанное. А при запуске в ретивых мальцов сильно скрипел, да и возвращался не сразу после запуска." ]]
                p [[ text "Была у него одна слабость... Слабость к маленькой птичке, что жила на большом дереве неподалеку. Бумеранг наматывал несколько кругов вокруг него, чем нервировал эту маленькую птичку по утрам, ведь утро начинается не с кофе." ]]
            ]
            say [
                p [[ text "Одним прекрасным весенним солнечным днём маленькую Птичечку настолько заколебал скрип Бумеранга, что она нажаловалась своему соседу по дереву — ночному сторожу Волшебного края — филину. Филин не любил солнечный свет, да и остальных жителей тоже не любил, ну то есть, он когда-то любил, но потом разлюбил всех (там всё сложно и запутанно, не то, что эта гениальная сказка). В общем, жил он в тени листьев и выходил совершать свои тенистые делишки ночью, пока остальные спят. Но птичку этот факт не останавливал и она периодически тормошила Тенюшку (не, ну а чо) попить кофе." ]]
            ]
            say [
                p [[ text "Была у Птички сестра-тройняшка, которой нравился скрип Бумеранга — белочка. Белочку зовут Адалиночка. Белочка прыгала без чувства меры: по веткам дерева, по крышам домов, чтобы заглянуть в мастерскую Агения, чтобы увидеть и попрыгать с Бумерангом. А так как белочка была ретивой, то недовольный Лягух запускал в нее Бумеранг и она радовалась и прыгала еще сильнее и громче, чем еще сильнее бесила Лягуха." ]]
            ]
            say [
                p [[ text "Жили-нетужили в своем Носочестве, горя не знали. Но однажды Агений утомился от прыткости Светуса 3000, когда та решила женить его на сбившемся ворсе от носков между пальцев. Не вытерпел он давления, упаковал Светуса 3000 в мешок и вывез в Темный сосисочный лес, за Пудинговое болото." ]]
            ]
            say [
                p [[ text "Выпуталась Светус 3000 из мешка и давай женить между собой шоколадных квакшей и мармеладных пиявок. Болотная живность была недовольна и несогласна с такой деятельностью и нажаловались своей защитнице Деве Единорожьей." ]]
            ]
            say [
                p [[ text "А Дева Единорожья с придурью была, впадала из крайности в крайность. Если злилась она, то не носить головы тому, кто под руку попадался, а если счастливая ходила, то причиняла добро окружающим насильно." ]]
            ]
            say [
                p [[ text "Вот и в этот раз, выслушав шоколад с мармеладом, Единорогиня взяла с собой помощника своего юродивого. Юродивого, но креативного. Подсказывал он ей (довольно редко, почти никогда) годные идеи по модернизации жизни пудингоболотного населения." ]]
            ]
            say [
                p [[ text "Взяла помощника Креативного, завязала ему розовый бантик на макушке, на шею надела манишку ажурную, а на ноги — полосатые гольфы, и поскакала на поиски Светуса 3000." ]]
            ]
            say [
                p [[ text "Как же обрадовалась Светус 3000, когда увидела креативно одетого Креатива. \"Люблю, — говорит, — не могу! До полубеспамятства люблю полосатые гольфы! Отдай!\"" ]]
            ]
            say [
                p [[ text "Креатив от спонтанности ситуации забулькал и начал пускать зефирные пузыри из носа. Единорог закусывала ими." ]]
            ]
            say [
                p [
                    [ text "Дева Единорог подошла к Светусу 3000 и говорит со своего понячьего роста:" ]
                    [ text "— Это ты тут маршмэллоу шоколадом поливаешь?!" ]
                    [ text "Света 3000 не растерялась с ответом:" ]
                    [ text "— Я — самая кринжовая дамочка в этом королевстве, а что можешь ты? — и закинула мармеладную пиявку в рот." ]
                    [ text "Единорожья поня погорцала вокруг механизированной сватьи, дотронулась до неё своим магическим рогом (чуть без глаза не оставила):" ]
                    [ text "— Поумней! — наколдовала Единорог." ]
                    [ text "И стала Светус 3000 квалифицированым педагогом — показывала в ай-блюдце с яблоком вебинары на тему: как захомутать богатого чародея, как отличить недомага от профессора кислых щей и так далее." ]
                ]
            ]

            agentSay [
                p [[ text "Значит, талантливый, но безмерно скромный Агений ночами не спал и мастерил эту свахо-юдо кринжовое, а потом прискакала единорог, приложила ей по лбу рогом, и та вмиг поумнела. И Агений такой: \"Ну да, ну да <:pepeEyesRoll:1133303912225587260>\"" ]]
            ]

            jungleSay [
                p [[ text "Ну ты ж не волшебный <:pepeSmile:947129401785593978>" ]]
            ]

            agentSay [
                p [[ text "Зачем нужна гениальность, когда есть валшепство?! <:pepeRee:958453248019267655>" ]]
            ]

            say [
                p [[ text "Так вот, Светус 3000 на радостях вручила Единорогу визитку с адресом Агения и полезла в Пудинговое болото откисать, готовиться к семинарам и вебинарам." ]]
            ]

            say [
                p [[ text "Дева Единорожья повертела визитку с галограмным эффектом, где волк превращается в зайца, и наоборт, дала креативные наставления своему креативному помощнику и поскакала по Глобальной Позиционирующей Системе в виде наклона ~~веток деревьев и кустарников~~ колбас и сосисок." ]]
            ]

            say [
                p [[ text "Пригоцала Единорог по указанному адресу, тормозила так, что брусчатку в рулон скрутила (лихой единорог, однако)." ]]
            ]

            say [
                p [[ text "Постучала в забрало рыцаря, что был вместо двери, и заржала:" ]]
                p [[ text "— Игого, тррр, ча-ча, тррр, ха-ха, — в доспехах исказилось эхо." ]]
                p [[ text "\"Пустозвон\", — подумала Дева Единорожья." ]]
            ]

            say [
                p [[ text "В щель между доспехами рыцаря и дверным проемом показалась жабья морда с недовольным выражением морды, наперевес с бумерангом коричневого цвета:" ]]
                p [[ text "— Чего надо? Устроили тут проходную, понимаешь, — проквакал Лягух. " ]]
                p [[ img "https://cdn.discordapp.com/emojis/952317602594693171.webp?size=1024&quality=lossless" "max" "max" ]]
            ]

            say [
                p [[ text "— Да вот, визитку вашу нашла, игого, решила познакомиться с тем, кто такой гениальный создал механизированного учителя года с выразительными глазами, игого, — пригарцовывая сказала пароль Единорог." ]]
                p [[ text "— Эй, доспехи, пропусти Единорогиню, — у доспехов в темном проёме забрала отобразилось анимированное кольцо загрузки, с пингом 13 секунд Единорог вошла в обитель Агения." ]]
            ]

            say [
                p [[ text "Тихо цокая копытами Единорог зашла в темное помещение. На тонких лучах света, что просачивался через мутное окно, плясали ажурные пылинки – крошки безе. За прилавком сидел гуманоидого вида механический октопус. Где-то в углу цокали шестеренки несмазанных часов. Вообще, смазывать всё была обязанность Бумеранга, но он был слишком занят тем, что летал по мастерской и к дереву – у него был специальный отсек со старым машинным маслом." ]]
            ]

            say [
                p [[ text "На стене висели головы неудавшихся экспериментов Агения, а из-за прилавка, где висела штора из рыбной чешуи раздавался стук молотка и непереводимая игра слов – гений работал." ]]
            ]

            say [
                p [[ text "Октопус пошевелился проявляя интерес к гостье — приподнял одно щупальце и открыл шторки-веки, зашуршали его механизмы." ]]
                p [[ text "— Что желает покупатель? — заученной фразой пробулькал Октопус и покосился на Лгуху. Лягух молчал, активировал свой гипновзгляд." ]]
                p [[ text "— Желаю увидеть того гения, что изобретает этих прекрасных механических кукол, — машет гривой Единорог. " ]]
            ]

            say [
                p [[ text "Октопус протянул очередное щупальце в сторону Лягухи. Он выключил гипновзгляд (не сработал) и пошлепал за чешуйчатую шторку. Тем временем из третьего щупальца октоуса вытекло нечто серо-буро-малиновое, амебообразное, стекло с прилавка и шлепнулось перед копытами Девы парнокопытной: «Экскремент», — подумала она" ]]
            ]

            say [
                p [[ text "И правда, желешка стала преобразовываться и вылупилось нечто с надпистью «Сделано в Китае», потом подумало и снова затряслось, вытянулось, выпуклось, стало похоже на карлика в тоге с сигаретой." ]]
                p [[ img "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fi.redd.it%2Fvn052mmz39841.jpg&f=1&nofb=1&ipt=79977780a52de9de2ded4366c686407d630709568757c19b1d24060c50370136&ipo=images" "" "" ]]
            ]

            say [
                p [[ text "«Не экскремент, — подумала Единорог. — Генная инженерия!»." ]]
                p [[ text "Карлик, прикуривая сигарету, засуетился с удивленным видом, оглянулся на штору и выбежал мимо доспехов вон." ]]
                p [[ text "Единорог потерла свой волшебный рог с мыслью, что пора пройти курсы повышения квалификации." ]]
            ]

            say [
                p [[ text "Через две минуты из-за шторки выпрыгнул Лягух:" ]]
                p [[ text "— Проходи в нашу Светлицу, — проквакал он. Единорог поцокала следом." ]]
            ]

            say [
                p [[ text "Светлица мало чем отличалась от магазинчика: тот же полумрак, те же пылинки — крошки безе, тот же творческий хаос на рабочем столе. Отличие было в том, что вместо голов на стене висели женские тела, но без голов, механические, естественно, но со всеми вторичными половыми признаками. И в самом дальнем углу висел потрепанный плакат с изображением спиралевидного бура." ]]
                p [[ text "«Магма течет по нашим венам...» всплыло в сознании Единорога, как что-то далекое и неуловимое. Дева парнокопытная встряхнула гривой, отгоняя наваждение." ]]
            ]

            say [
                p [[ text "— Кхе-кхе, — привлекла внимание Единорог. Агений обернулся. — Здоровеньки булы. Я только спросить." ]]
            ]

            say [
                p [[ text "Агений растерялся, напялил шляпу и спрятался за старый монитор, который светился синим экраном смерти:" ]]
                p [[ text "— Я буду всё отрицать! — лучшая защита — это нападение. — Я ничего не делал и не трогал! — возмущался Агений." ]]
                p [[ text "Единорог потыкала его палочкой — жив." ]]
            ]

            say [
                p [[ text "— Да я только спросить... Это ты создал куклу, которая переженила между собой половину Сосисочного леса и Пудингового болота? — заржала единорог." ]]
                p [[ text "— Я не волшебник... — начал Агений." ]]
                p [[ text "— Да-да, это я вижу, — отмахнулась полулошадь." ]]
                p [[ text "— Хамить было не обязательно, — <:catSad:1092657743137083413> погрустнел Агений. — Да, Светус 3000 я создал. Она моя гордость! И когда-нибудь она станет настоящим человеком!" ]]
            ]

            say [
                p [[ text "— Хорошо, как скажешь, — не поверила Дева Единорожья. — А почему на твоем дереве листья в виде носков?" ]]
                p [[ text "— Так это ж, глупый ты единорог, Носочество же! — с недоумением почесал монитор Агений." ]]
            ]
            say [
                p [[ text "Внезапно в мастерскую, на всех свистящих парах, влетает коричневый Бумеранг — все его детали были обмазаны шоколадом. Агений похлопал по насесту и Бумеранг приземлился на него, выдвинул телескопические ручки и ножки. Агений начал его оттирать." ]]
            ]

            say [
                p [[ text "— Любопытно, — покосилась на капающее машинное масло Единорог. — А почему носки с дерева раньше времени опадают? Еще рано по климатическому календарю." ]]
                p [[ text "— Неприятности у нас в королевстве: завелся рак-отшельник. Он и устраивает экологические аварии. То носки синтетические распространяет, то колготки капроновые, а у нас тут всё натуральное, хлопковое, — и надел зеленые хлопковые колготки на Бумеранг, которые тут же немного сползли." ]]
            ]
            say [
                p [[ text "Единорог вышла из Светлицы и наткнулась на недовольное лицо Лягуха:" ]]
                p [[ text "— Так-так-так. Так тут у нас принц заколдованный. Зря тебя Светус 3000 за принцессу из соседнего сервера королевства не сосватала. Давно бы уже счастливый ходил и не в виде жабы." ]]
                p [[ text "— Мне и так неплохо, — разлегся на прилавке Лягух, закинув лапки за голову <:pepeChill:937277043920478228>. — Нафига мне это бабьё? Ухаживай за ними, цветы им дари, а они мозг выносят потом. Приготовить мух и фрак отутюжить я и сам могу." ]]
                p [[ text "Единорог покивала гривой в стороны и пошла к старому высокому дереву — носочные листья изучать. Плохой опыт тоже опыт, как никак." ]]
            ]
            say [
                p [[ text "Прицокала Единорог к дереву уже в сумерках. На ветке, в тени носочных листьев сидел сыч. Вокруг него скакала Адалиночка, а вокруг пикировала Бульбулечка." ]]
                p [[ text "— Что у вас тут происходит? — потрясала гривой Единорог." ]]
                p [[ text "— Я не в курсе, — вразнобой моргнул глазами сыч." ]]
                p [[ img "https://media.tenor.com/3CIaFl-huc0AAAAC/no-no-way.gif" "owl eyes blink" "owl eyes blink" ]]
            ]
            say [
                p [[ text "— Вот вечно ты лапшу на уши всем вешаешь, — упрекнула его белочка." ]]
                p [[ text "«Готова на отсечение рог дать — женаты», моргнула вразнобой глазами Единорог." ]]
            ]
            say [
                p [[ text "— Да один придурок поймал меня, — начала птичечка, — и давай мне предлагать платья, шубы, сапоги. И ладно бы они нормального качества были, так синтетика одна, еще и цвета стремного, фасоны старческие, а я же молодая." ]]
                p [[ text "Выслушала единорог Булечку и слово негодующе молвит:" ]]
                p [[ text "— Какой же он полуфабрикантный гермафродит, раз позволил себе такое обращение с юной прелестницей?! Укажи мне вектор, птичечка. Пойду справедливость вершить ~~рогом на шашлык пущу~~." ]]
            ]
            say [
                p [[ text "Скачет единорог по сосисочному лесу, птичка впереди летит, дорогу показывает. Долго ли, коротко ли скачет. Но хорошее рано или поздно заканчивается и перед ними вдруг выпрыгивает эксгибиционист, лысый, как коленка, раскрывает плащ, а там… На внутреннем кармане его плаща висят разных размеров и вкусов конфеты: шоколадные батончики, карамельки, ириски, на палочках петушки." ]]
            ]
            say [
                p [[ text "— Это он! — зачирикала Бульбулечка. — Это он мне искусственную шубу хотел впарить!" ]]
                p [[ text "— Спокойно, — спокойно сказала Единорожья Дева. — Сейчас я ему волосы-то повыдергиваю, если найду, — осмотрев маньяка с ног до головы, проржала полулошадь." ]]
            ]
            say [
                p [[ text "— Я просто хотел, чтобы меня понимали и любили! — растерялся лысый маньяк. — а не это вот всё! А вы не хотели даже несчастную пару носков с дерева снять, ведь видели же, что я пришел в чем мать родила — в одном плаще. Мне было холодно и одиноко. — тихо закончил лысый голый маньяк." ]]
                p [[ text "— Тю! Так бы сказал, — села его лысину птичка. И прикрыла его лысину переваренными зернышками и травой." ]]
                p [[ text "— Пошли со мной, одинокий одиночка. Знаю как тебе помочь, — сказала Единорог." ]]
                p [[ text "И поскакали они дружно в сторону Пудингового болота." ]]
            ]
            say [
                p [[ text "До Пудингового болота доскакали очень быстро. Ну очень быстро. Если вы понимаете о чем я." ]]
                p [[ text "Единорог, как и ожидала, увидела откисающую в ванильном пудинге болота Её. Да-да, именно Её — гуру и мастера наук в сводничестве всех и вся — Светус 3000." ]]
            ]
            say [
                p [[ text "— Светулеточка, — залебезил Единорог, — у нас есть для тебя работа." ]]
                p [[ text "Светус 3000 приоткрыла глаз:" ]]
                p [[ text "— Иди, вон, со своим мужиком говори, — кивнула она на лысого, — а то скачешь тут 24 на 7." ]]
                p [[ text "«Никто и не обещал, что будет легко», — вздохнула единорог." ]]
            ]
            say [
                p [[ text "— Светус, Агений просил передать, что ты его гордость, и когда он прикручивал доспехи к твоим… э… глазам, то забыл предупредить, что в доспехах ~~какой-то баг, а не фича~~ заклинание, и на 100500 сосватанном нелюде у тебя откроются чакры и буратино станет ~~настоящим мальчиком~~ ты превратишься в принцессу премудрую." ]]
            ]
            say [
                p [[ text "Светус 3000 сидела верхом на велосипеде:" ]]
                p [[ text "— Мне долго вас еще ждать, зануды?" ]]
            ]
            say [
                p [[ text "И вновь двинулись в путь и не только наши герои. Птичка пыталась спрятаться от назойливых заигрываний эксгибициониста, эксгибиционист пытался одарить птичку вниманием и заботой, и капроновыми гольфами. Ну а Светус 3000 перебирала архив холостяков и холостячек. Единорог просто излучала неадекватность. Всё как обычно." ]]
            ]
            say [
                p [[ text "И вот ~~наконец-таки~~ добрались они до носочного дерева. Было раннее утро, и многие жители Носочества еще спали." ]]
            ]
            say [
                p [[ text "Вот и сыч, последний раз на сегодня моргнув врассинхрон глазами, скукожился в комок пыли и уснул." ]]
            ]
            say [
                p [[ text "Белочка Адалиночка спряталась в дупло готовить ужин, поздний ужин и ранний завтрак." ]]
            ]
            say [
                p [[ text "Булечка взлетела на свою веточку, где располагался ее булечник (ну не скворечник же) — приводить себя в порядок от изнуряющего путешествия." ]]
            ]
            say [
                p [[ text "Светус 3000, Дева Единорожья и лысый капроно-маньяк-эксгибиционист дошли до обители Агения и, собравшись с духом, дружно толкнули доспехи." ]]
            ]
            jungleSay [
                p [[ text "На этом и закончился первый сезон нашей сказки!" ]]
                p [[ img "https://media.tenor.com/eludMOSyaaIAAAAd/santa-barbara-kocci.gif" "santa-barbara" "santa-barbara" ]]
            ]
        ]
    ]

let (scenario: Scenario<NarratorCommonContent, Label, CustomStatement>) =
    [
        label Menu [
            jungleMenu [
                p [[ text "Привет, путник! Какую сказку хочешь послушать?"]]
            ] [
                choice "Про кукол, незнающих меры" [
                    jump StartTaleAboutDolls
                ]
            ]
        ]

        yield! taleAboutDolls
    ]
    |> List.map (fun (labelName, body) -> labelName, (labelName, body))
    |> Map.ofList

type CustomStatementOutput = unit

let customStatementHandler : CustomStatementHandler<NarratorCommonContent,Label,CustomStatement,CustomStatementArg,CustomStatementOutput> =
    CustomStatementHandler.empty

let customOutputView (customOutput: CustomStatementOutput) =
    failwithf "has not implemented"
