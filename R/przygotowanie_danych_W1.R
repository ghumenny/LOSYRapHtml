#' @title Przygotowanie danych do tabeli wynagrodzeń (płeć i zawód)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Tworzy ramkę danych
#'   w formacie będącym wejściem do funkcji gentab_tab_D z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param kryterium Zmienna tekstowa opisująca kryterium płec(sexf) lub
#'        zawód(nazwa_zaw).
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param tylko_tabele parametr TRUE/FALSE przekazywany z głównej funkcji
#'   generującej raport - jeśli FALSE to przycina tabele z zawodami do 10
#'   najliczniejszych zawodów, jeśli TRUE to raport generuje tabele zawodów bez
#'   przycinania
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches
#' @importFrom dplyr ends_with rename_with
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @export
dane_tab_W1 <- function(pelna_finalna_ramka_wskaznikow,
                        typ_szk, kryterium, rok_absolwentow,
                        rok, tylko_tabele) {

  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "W1",
      .data$kryterium == {{kryterium}},
      .data$rok == rok_kal,
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }


  dane_wyjsciowe <- dane_wejsciowe  %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2))) %>%
    rename(`Liczba absolwentów uzyskujących przychód`	= n,
           `Średnie przychody` = sre,
           `Przychody w 5 centylu` = q5,
           `Przychody w 25 centylu`	= q25,
           `Przychody połowy pracujących (mediana)` = med,
           `Przychody w 75 centylu`	= q75,
           `Przychody w 95 centylu` = q95)

  if ({{kryterium}} == "sexf") {
    dane_wyjsciowe <- dane_wyjsciowe  %>%
      dplyr::rename(Płeć = sexf)
  } else if ({{kryterium}} == "nazwa_zaw") {
    if(tylko_tabele == FALSE) {
      dane_wyjsciowe <- dane_wyjsciowe  %>%
        slice(1:10) %>%
        dplyr::rename(Zawód = nazwa_zaw)
    } else {
      dane_wyjsciowe <- dane_wyjsciowe %>%
        dplyr::rename(Zawód = nazwa_zaw)
    }
  }

  return(dane_wyjsciowe)
}
