#' @title Przygotowanie danych do tabeli wynagrodzeń (płeć i zawód)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Tworzy ramkę danych
#'   w formacie będącym wejściem do funkcji gentab_tab_D z tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param kryterium Zmienna tekstowa: "sexf" (dla płci) lub "nazwa_zaw" (dla zawodu).
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches
#'   relocate any_of if_all all_of
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_tab_W1 <- function(ramka_danych,
                        wartosc_filtrujaca,
                        typ_szk, kryterium, rok_absolwentow,
                        rok,
                        typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "W1",
      .data$kryterium == {{kryterium}},
      .data$rok == {{rok}},
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == {{rok_absolwentow}}
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
    dane_wyjsciowe <- dane_wyjsciowe %>%
      dplyr::rename(Zawód = nazwa_zaw)
  }
  dane_wyjsciowe <- dane_wyjsciowe %>%
    dplyr::relocate(any_of(c("Płeć", "Zawód")))

  return(dane_wyjsciowe)
}
