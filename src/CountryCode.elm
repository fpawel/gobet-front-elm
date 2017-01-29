module CountryCode exposing (countryName)

import Dict exposing (Dict)


codes : Dict String String
codes =
    [ ( "AE", "ОАЭ" )
    , ( "AF", "Афганистан" )
    , ( "AL", "Албания" )
    , ( "AM", "Армения" )
    , ( "AR", "Аргентина" )
    , ( "AT", "Австрия" )
    , ( "AU", "Австралия" )
    , ( "AZ", "Азербайджан" )
    , ( "BA", "Босния" )
    , ( "BD", "Бангладеш" )
    , ( "BE", "Бельгия" )
    , ( "BG", "Болгария" )
    , ( "BH", "Бахрейн" )
    , ( "BN", "Бруней" )
    , ( "BO", "Боливия" )
    , ( "BR", "Бразилия" )
    , ( "BT", "Бутан" )
    , ( "BW", "Ботсвана" )
    , ( "BY", "Беларусь" )
    , ( "BZ", "Белиз" )
    , ( "CA", "Канада" )
    , ( "CD", "Конго (ДРК)" )
    , ( "CH", "Швейцария" )
    , ( "CI", "Кот-д’Ивуар" )
    , ( "CL", "Чили" )
    , ( "CM", "Камерун" )
    , ( "CN", "Китай" )
    , ( "CO", "Колумбия" )
    , ( "CR", "Коста-Рика" )
    , ( "CS", "Сербия" )
    , ( "CU", "Куба" )
    , ( "CZ", "Чехия" )
    , ( "DE", "Германия" )
    , ( "DJ", "Джибути" )
    , ( "DK", "Дания" )
    , ( "DO", "Доминикано" )
    , ( "DZ", "Алжир" )
    , ( "EC", "Эквадор" )
    , ( "EE", "Эстония" )
    , ( "EG", "Египет" )
    , ( "ER", "Эритрея" )
    , ( "ES", "Испания" )
    , ( "ET", "Эфиопия" )
    , ( "FI", "Финляндия" )
    , ( "FO", "Фареры" )
    , ( "FR", "Франция" )
    , ( "GB", "Великобритания" )
    , ( "GE", "Грузия" )
    , ( "GL", "Гренландия" )
    , ( "GR", "Греция" )
    , ( "GT", "Гватемала" )
    , ( "HK", "Гонконг" )
    , ( "HN", "Гондурас" )
    , ( "HR", "Хорватия" )
    , ( "HT", "Гаити" )
    , ( "HU", "Венгрия" )
    , ( "ID", "Индонезия" )
    , ( "IE", "Ирландия" )
    , ( "IL", "Израиль" )
    , ( "IN", "Индия" )
    , ( "IQ", "Ирак" )
    , ( "IR", "Иран" )
    , ( "IS", "Исландия" )
    , ( "IT", "Италия" )
    , ( "JM", "Ямайка" )
    , ( "JO", "Иордания" )
    , ( "JP", "Япония" )
    , ( "KE", "Кения" )
    , ( "KG", "Киргизия" )
    , ( "KH", "Камбоджа" )
    , ( "KR", "Южная Корея" )
    , ( "KW", "Кувейт" )
    , ( "KZ", "Казахстан" )
    , ( "LA", "Лаосс" )
    , ( "LB", "Ливан" )
    , ( "LI", "Лихтенштейн" )
    , ( "LK", "Шри-Ланка" )
    , ( "LT", "Литва" )
    , ( "LU", "Люксембург" )
    , ( "LV", "Латвия" )
    , ( "LY", "Ливия" )
    , ( "MA", "Марокко" )
    , ( "MC", "Монако" )
    , ( "MD", "Молдова" )
    , ( "ME", "Черногория" )
    , ( "MK", "Македония" )
    , ( "ML", "Мали" )
    , ( "MM", "Мьянма" )
    , ( "MN", "Монголия" )
    , ( "MO", "Макао" )
    , ( "MT", "Мальта" )
    , ( "MV", "Мальдивы" )
    , ( "MX", "Мексика" )
    , ( "MY", "Малайзия" )
    , ( "NG", "Нигерия" )
    , ( "NI", "Никарагуа" )
    , ( "NL", "Нидерланды" )
    , ( "NO", "Норвегия" )
    , ( "NP", "Непал" )
    , ( "NZ", "Новая Зеландия" )
    , ( "OM", "Оман" )
    , ( "PA", "Панама" )
    , ( "PE", "Перу" )
    , ( "PH", "Филиппины" )
    , ( "PK", "Пакистан" )
    , ( "PL", "Польша" )
    , ( "PR", "Пуэрто-Рико" )
    , ( "PT", "Португалия" )
    , ( "PY", "Парагвай" )
    , ( "QA", "Катар" )
    , ( "RE", "Реюньон" )
    , ( "RO", "Румыния" )
    , ( "RS", "Сербия" )
    , ( "RU", "Россия" )
    , ( "RW", "Руанда" )
    , ( "SA", "Саудовская Аравия" )
    , ( "SE", "Швеция" )
    , ( "SG", "Сингапур" )
    , ( "SI", "Словения" )
    , ( "SK", "Словакия" )
    , ( "SN", "Сенегал" )
    , ( "SO", "Сомали" )
    , ( "SV", "Сальвадор" )
    , ( "SY", "Сирия" )
    , ( "TH", "Таиланд" )
    , ( "TJ", "Таджикистан" )
    , ( "TM", "Туркменистан" )
    , ( "TN", "Тунис" )
    , ( "TR", "Турция" )
    , ( "TT", "Тринидад" )
    , ( "TW", "Тайвань" )
    , ( "UA", "Украина" )
    , ( "US", "США" )
    , ( "UY", "Уругвай" )
    , ( "UZ", "Узбекистан" )
    , ( "VE", "Венесуэла" )
    , ( "VN", "Вьетнам" )
    , ( "YE", "Йемен" )
    , ( "ZA", "ЮАР" )
    , ( "ZW", "Зимбабве" )
    , ( "GI", "Гибралтар" )
    , ( "CY", "Кипр" )
    ]
        |> Dict.fromList



-- countryName : получить имя страны по ISO-2 коду


countryName : String -> Maybe String
countryName k =
    Dict.get k codes
