#' @title Dolełniacz typu szkoły
#' @description funkcja tworzaca dopełniacz nazwy typu szkoły
#' @param nazwa Wartość tekstowa opisująca typ szkoły
#' @return zwraca dopełniach typu szkoły w formie tekstowej
dopelniacz <- function(nazwa) {
  switch(nazwa,
         "Liceum ogólnokształcące dla młodzieży" = "Liceów ogólnokształcących dla młodzieży",
         "Liceum ogólnokształcące dla dorosłych" = "Liceów ogólnokształcących dla dorosłych",
         "Branżowa szkoła I stopnia" = "Branżowych szkół I stopnia",
         "Młodociani w Branżowej szkole I stopnia" = "Branżowych szkół I stopnia, którzy kształcili się będąc pracownikami młodocianymi",
         "Niemłodociani w Branżowej szkole I stopnia" = "Branżowych szkół I stopnia, którzy kształcili się będąc pracownikami niemłodocianymi",
         "Szkoła policealna" = "Szkół policealnych",
         "Szkoła specjalna przysposabiająca do pracy" = "Szkół specjalnych przysposabiających do pracy",
         "Technikum" = "Techników",
         "Branżowa szkoła II stopnia" = "Branżowych szkół II stopnia",
         "dolnośląskie" = "województwa dolnośląskiego",
         "kujawsko-pomorskie" = "województwa kujawsko-pomorskiego",
         "lubelskie" = "województwa lubelskiego",
         "lubuskie" = "województwa lubuskiego",
         "łódzkie" = "województwa łódzkiego",
         "małopolskie" = "województwa małopolskiego",
         "mazowieckie" = "województwa mazowieckiego",
         "opolskie" = "województwa opolskiego",
         "podkarpackie" = "województwa podkarpackiego",
         "podlaskie" = "województwa podlaskiego",
         "pomorskie" = "województwa pomorskiego",
         "śląskie" = "województwa śląskiego",
         "świętokrzyskie" = "województwa świętokrzyskiego",
         "warmińsko-mazurskie" = "województwa warmińsko-mazurskiego",
         "wielkopolskie" = "województwa wielkopolskiego",
         "zachodniopomorskie" = "województwa zachodniopomorskiego",
         "branża hotelarsko-gastronomiczno-turystyczna" = "branży hotelarsko-gastronomiczno-turystycznej",
         "branża elektroniczno-mechatroniczna" = "branży elektroniczno-mechatronicznej",
         "branża mechaniczna" = "branży mechanicznej",
         "branża budowlana" = "branży budowlanej",
         "branża rolno-hodowlana" = "branży rolno-hodowlanej",
         "branża motoryzacyjna" = "branży motoryzacyjnej",
         "branża handlowa" = "branży handlowej",
         "branża spedycyjno-logistyczna" = "branży spedycyjno-logistycznej",
         "branża ekonomiczno-administracyjna" = "branży ekonomiczno-administracyjnej",
         "branża fryzjersko-kosmetyczna" = "branży fryzjersko-kosmetycznej",
         "branża spożywcza" = "branży spożywczej",
         "branża audiowizualna" = "branży audiowizualnej",
         "branża poligraficzna" = "branży poligraficznej",
         "branża teleinformatyczna" = "branży teleinformatycznej",
         "branża transportu lotniczego" = "branży transportu lotniczego",
         "branża chemiczna i ochrony środowiska" = "branży chemicznej i ochrony środowiska",
         "branża ochrony i bezpieczeństwa osób i mienia" = "branży ochrony i bezpieczeństwa osób i mienia",
         "branża elektroenergetyczna" = "branży elektroenergetycznej",
         "branża leśna" = "branży leśnej",
         "branża ogrodnicza" = "branży ogrodniczej",
         "branża drzewno-meblarska" = "branży drzewno-meblarskiej",
         "branża opieki zdrowotnej" = "branży opieki zdrowotnej",
         "branża mechaniki precyzyjnej" = "branży mechaniki precyzyjnej",
         "branża przemysłu mody" = "branży przemysłu mody",
         "branża górniczo-wiertnicza" = "branży górniczo-wiertniczej",
         "branża transportu wodnego" = "branży transportu wodnego",
         "branża metalurgiczna" = "branży metalurgicznej",
         "branża transportu kolejowego" = "branży transportu kolejowego",
         "branża transportu drogowego" = "branży transportu drogowego",
         "branża pomocy społecznej" = "branży pomocy społecznej",
         "branża poligraficzno-księgarska" = "branży poligraficzno-księgarskiej",
         "branża ceramiczno-szklarska" = "branży ceramiczno-szklarskiej",
         "branża rybacka" = "branży rybackiej",
         nazwa # Domyślna wartość w przypadku braku dopasowania
  )
}

#' @title Miejscownik nazwy odprzymiotnikowej województwa
#' @description funkcja tworzaca miejscownik nazwy województwa
#' @param WOJ_NAZWA Wartość tekstowa opisująca województwo
#' @return zwraca miejscownik województwa w formie tekstowej
miejscownik <- function(WOJ_NAZWA) {
  switch(WOJ_NAZWA,
         "dolnośląskie" = "dolnośląskim",
         "kujawsko-pomorskie" = "kujawsko-pomorskim",
         "lubelskie" = "lubelskim",
         "lubuskie" = "lubuskim",
         "łódzkie" = "łódzkim",
         "małopolskie" = "małopolskim",
         "mazowieckie" = "mazowieckim",
         "opolskie" = "opolskim",
         "podkarpackie" = "podkarpackim",
         "podlaskie" = "podlaskim",
         "pomorskie" = "pomorskim",
         "śląskie" = "śląskim",
         "świętokrzyskie" = "świętokrzyskim",
         "warmińsko-mazurskie" = "warmińsko-mazurskim",
         "wielkopolskie" = "wielkopolskim",
         "zachodniopomorskie" = "zachodniopomorskim",
         WOJ_NAZWA # Domyślna wartość w przypadku braku dopasowania
  )
}


#' @title Miejscownik nazwy odprzymiotnikowej województwa
#' @description funkcja tworzaca miejscownik nazwy województwa
#' @param WOJ_NAZWA Wartość tekstowa opisująca województwo
#' @return zwraca miejscownik województwa w formie tekstowej
dopelniacz_w <- function(WOJ_NAZWA) {
  switch(WOJ_NAZWA,
         "dolnośląskie" = "dolnośląskiego",
         "kujawsko-pomorskie" = "kujawsko-pomorskiego",
         "lubelskie" = "lubelskiego",
         "lubuskie" = "lubuskiego",
         "łódzkie" = "łódzkiego",
         "małopolskie" = "małopolskiego",
         "mazowieckie" = "mazowieckiego",
         "opolskie" = "opolskiego",
         "podkarpackie" = "podkarpackiego",
         "podlaskie" = "podlaskiego",
         "pomorskie" = "pomorskiego",
         "śląskie" = "śląskiego",
         "świętokrzyskie" = "świętokrzyskiego",
         "warmińsko-mazurskie" = "warmińsko-mazurskiego",
         "wielkopolskie" = "wielkopolskiego",
         "zachodniopomorskie" = "zachodniopomorskiego",
         WOJ_NAZWA # Domyślna wartość w przypadku braku dopasowania
  )
}

branza <- c( "branża hotelarsko-gastronomiczno-turystyczna",
             "branża elektroniczno-mechatroniczna",
             "branża mechaniczna",
             "branża budowlana",
             "branża rolno-hodowlana",
             "branża motoryzacyjna",
             "branża handlowa",
             "branża spedycyjno-logistyczna",
             "branża ekonomiczno-administracyjna",
             "branża fryzjersko-kosmetyczna",
             "branża spożywcza",
             "branża audiowizualna",
             "branża poligraficzna",
             "branża teleinformatyczna",
             "branża transportu lotniczego",
             "branża chemiczna i ochrony środowiska",
             "branża ochrony i bezpieczeństwa osób i mienia",
             "branża elektroenergetyczna",
             "branża leśna",
             "branża ogrodnicza",
             "branża drzewno-meblarska",
             "branża opieki zdrowotnej",
             "branża mechaniki precyzyjnej",
             "branża przemysłu mody",
             "branża górniczo-wiertnicza",
             "branża transportu wodnego",
             "branża metalurgiczna",
             "branża transportu kolejowego",
             "branża transportu drogowego",
             "branża pomocy społecznej",
             "branża poligraficzno-księgarska",
             "branża ceramiczna-szklarska",
             "branża rybacka" )

#' @title Filtruje branże wg minimalnej liczby absolwentów
#' @description Przegląda ramkę wskaźników i zwraca wektor nazw tych branż,
#'   które w danym roczniku absolwentów (wskaźnik 'typ_szk2')
#'   mają co najmniej `prog` absolwentów w wierszu 'OGÓŁEM'.
#'
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników
#'   (np. `pelna_branzowa_ramka_wskaznikow`). Oczekuje kolumn
#'   `branza`, `wskaznik`, `rok_abs` i `wynik`.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param prog Minimalna liczba absolwentów (wartość w `n_SUMA`)
#'   do uwzględnienia branży. Domyślnie 10.
#'
#' @return Wektor znakowy (character vector) z unikalnymi nazwami branż,
#'   które spełniają kryterium.
#'
#' @importFrom dplyr %>% filter select pull distinct
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @export
pobierz_aktywne_branze <- function(ramka_danych, rok_absolwentow, prog = 10) {

  # 1. Filtrujemy ramkę główną do interesujących nas wskaźników
  branze_z_liczebnoscia <- ramka_danych %>%
    filter(
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == rok_absolwentow
    ) %>%
    # 2. Wybieramy tylko kolumnę z branżą i zagnieżdżoną tabelą
    select(branza, wynik) %>%

    # 3. Rozpakowujemy zagnieżdżone tabele z kolumny 'wynik'.
    # To jest kluczowy krok, który tworzy długą ramkę danych.
    # Jeśli 'wynik' jest NULL lub pusty, wiersz zostanie pominięty.
    tidyr::unnest(cols = c(wynik)) %>%

    # 4. Z rozpakowanych danych filtrujemy tylko wiersze "OGÓŁEM"
    filter(.data$typ_szk2 == "OGÓŁEM") %>%

    # 5. Na tym etapie mamy ramkę: | branza | ... | n_SUMA | ... |
    # Filtrujemy ją, zostawiając tylko te branże,
    # które spełniają próg liczebności.
    filter(.data$n_SUMA >= prog) %>%

    # 6. Wybieramy tylko kolumnę z nazwami branż
    pull(.data$branza) %>%

    # 7. Zapewniamy unikalność listy (na wszelki wypadek)
    unique()

  return(branze_z_liczebnoscia)
}

#' @title Generowanie kolejnych numerów tabeli
#' @description Funkcja do wstawiania kolejnych numerów tabeli w renderowanym
#'              dokumencie
#' @return zwraca kolejny nr tabeli
tab_num = function() {
  nr = get0("___nrTabeli___", parent.frame(), ifnotfound = 1)
  assign("___nrTabeli___", nr + 1, envir = parent.frame())
  return(nr)
}
#' @title Generowanie kolejnych numerów wykresów
#' @description Funkcja do wstawiania kolejnych numerów wykresów w renderowanym
#'              dokumencie
#' @return zwraca kolejny nr wykresu
wyk_num = function() {
  nr = get0("___nrWykresu___", parent.frame(), ifnotfound = 1)
  assign("___nrWykresu___", nr + 1, envir = parent.frame())
  return(nr)
}

#' @title Generowanie ciągów tekstowych bez spacji
#' @description Funkcja do łączenia tekstów bez spacji funkcja skopiowana z
#'               https://rdrr.io/cran/rosetta/src/R/cat0.R
#' @param ... Obiekty, które mają zostać połączone.
#' @param sep Separator, który ma zostać użyty. Domyślnie jest to pusty ciąg znaków.
#' @return zwraca ciąg tekstowy
cat0 <- function(..., sep="") {
  return(cat(..., sep=sep));
}

#' @title odwrotność funkcji `%in%`
#' @description odwraca wektor 0/1 gnerowy przez `%in%`
#' @param x Wartości do sprawdzenia.
#' @param y Wartości, w których należy szukać dopasowania.
#' @return zwraca wektor 1/0
`%ni%` <- function(x, y) {
  !(x %in% y)
}

#' @title Oblicza dynamiczną wysokość wykresu (zawody)
#' @description Funkcja pomocnicza obliczająca optymalną wysokość
#'   wykresu w zależności od liczby zawodów w danych. Bazuje na
#'   wskaźniku W1, kryterium 'nazwa_zaw'.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#' @param wartosc_filtrujaca Wartość (np. nazwa branży), której szukamy.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#' @param typ_szkoly Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok kalendarzowy.
#'   Podawana bez cudzysłowu.
#' @return Liczbowa wartość wysokości wykresu.
#' @importFrom dplyr %>% filter pull if_all all_of
#' @importFrom rlang .data
#' @export
oblicz_wysokosc_wykresu_zawody <- function(ramka_danych,
                                           wartosc_filtrujaca,
                                           typ_szkoly,
                                           rok_absolwentow,
                                           rok,
                                           typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "S7",
      .data$kryterium == "nazwa_zaw",
      .data$typ_szk2 == {{typ_szkoly}},
      .data$rok_abs == {{rok_absolwentow}},
      .data$rok == {{rok}}
      ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (!is.null(dane_wejsciowe) && !("Uwaga" %in% colnames(dane_wejsciowe))) {

    liczba_zawodow_raportu <- dane_wejsciowe %>%
      filter(nazwa_zaw != "OGÓŁEM") %>%
      nrow()

  } else {
    liczba_zawodow_raportu <- 0
  }

  if (liczba_zawodow_raportu > 10) {
    liczba_zawodow_raportu <- 10
  }

  dynamic_fig_height <- (4 + liczba_zawodow_raportu * 1.6) / 2

  return(dynamic_fig_height)
}
