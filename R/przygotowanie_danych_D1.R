#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `dyplom`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresDyplomyPlec z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr filter pull select mutate %>% if_else starts_with
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_sentence
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @export
dane_wyk_D1_plec <- function(ramka_danych,
                             wartosc_filtrujaca,
                             typ_szk, rok_absolwentow,
                             typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "D1",
      .data$kryterium == "sexf",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  # Sprawdzenie, czy dane wejściowe są puste
  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  wiersz_sumy <- dane_wejsciowe %>%
    filter(D1 == "SUMA")
  n_m <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_Mężczyzna else 0
  n_k <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_Kobieta else 0
  n_o <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_OGÓŁEM else 0

  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(D1 != "SUMA", D1 != "Nie dotyczy") %>%
    select(D1, starts_with("pct_")) %>%
    pivot_longer(!D1, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
    filter(
      (plec == "Mężczyzna" & n_m >= 10) |
        (plec == "Kobieta" & n_k >= 10) |
        (plec == "OGÓŁEM" & n_o >= 10)
    ) %>%
    mutate(
      pct = .data$pct / 100,
      dyplom =  str_to_sentence(D1),
      dyplom = factor(.data$dyplom, levels = c(
        "Świadectwo czeladnicze",
        "Dyplom zawodowy",
        "Tylko certyfikat kwalifikacji",
        "Brak certyfikatów i dyplomu")),
      plec = if_else(plec == "Mężczyzna", "Mężczyźni",
                     if_else(plec == "Kobieta", "Kobiety", "Ogółem"))) %>%
    select(plec, dyplom, pct)




  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do tabel
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr filter pull select mutate %>% across rename rename_with matches
#' @importFrom tidyselect starts_with where all_of
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_replace str_to_sentence
#' @importFrom tibble tibble
#' @export
dane_tab_D1_plec <- function(ramka_danych,
                             wartosc_filtrujaca,
                             typ_szk, rok_absolwentow,
                             typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "D1",
      .data$kryterium == "sexf",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  # Sprawdzenie, czy dane wejściowe są puste
  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  wiersz_sumy <- dane_wejsciowe %>% filter(D1 == "SUMA")
  n_m <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_Mężczyzna else 0
  n_k <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_Kobieta else 0
  n_o <- if(nrow(wiersz_sumy) > 0) wiersz_sumy$n_OGÓŁEM else 0

  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(D1 != "SUMA",  n_OGÓŁEM != 0) %>%
    select(D1, n_OGÓŁEM, starts_with("pct_")) %>%
    { if (n_m < 10) select(., -ends_with("Mężczyzna")) else . } %>%
    { if (n_k < 10) select(., -ends_with("Kobieta")) else . } %>%
    { if (n_o < 10) select(., -ends_with("OGÓŁEM")) else . } %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      `Uzyskanie dokumentu potwierdzającego kwalifikacje` = str_to_sentence(D1)) %>%
    rename(N = n_OGÓŁEM) %>%
    rename_with(~ str_replace(., "pct_Kobieta",
                              "pct_Kobiety"),
                matches("pct_Kobieta")) %>%
    rename_with(~ str_replace(., "pct_Mężczyzna",
                              "pct_Mężczyźni"),
                matches("pct_Mężczyzna")) %>%
    rename_with(~ str_replace(., "pct_OGÓŁEM",
                              "pct_Ogółem"),
                matches("pct_OGÓŁEM")) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_")) %>%
    select(all_of("Uzyskanie dokumentu potwierdzającego kwalifikacje"),
           N, starts_with("procent_"))

  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `dyplom`, `nazwa_zaw`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresDyplomyZawod z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate slice starts_with where
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_to_sentence
#' @importFrom stats reorder
#' @importFrom tibble tibble
#' @export
dane_wyk_D1_zaw <- function(ramka_danych,
                            wartosc_filtrujaca,
                            typ_szk, rok_absolwentow,
                            typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "D1",
      .data$kryterium == "nazwa_zaw",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>% `[[`(1)


  if (is.null(dane_wejsciowe) ||
    colnames(dane_wejsciowe)[1] %in% "Uwaga" ||
      nrow(dane_wejsciowe %>%
           filter(nazwa_zaw != "OGÓŁEM",
                  n_SUMA >= 10)) == 0||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }


  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(nazwa_zaw != "OGÓŁEM",
           n_SUMA >= 10) %>%
    slice(1:10) %>%
    select(nazwa_zaw, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
    select(where(~ is.factor(.x) || sum(.x) !=0)) %>%
    pivot_longer(!c(nazwa_zaw, n_SUMA), names_to = "dyplom", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(dyplom = str_to_sentence(dyplom),
           dyplom = factor(.data$dyplom, levels = c(
             "Świadectwo czeladnicze",
             "Dyplom zawodowy",
             "Tylko certyfikat kwalifikacji",
             "Brak certyfikatów i dyplomu")),
           nazwa_zaw = reorder(nazwa_zaw, n_SUMA),
           pct = .data$pct / 100) %>%
    select(nazwa_zaw, dyplom, pct)

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabel
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename rename_with
#' @importFrom tidyselect starts_with matches
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_replace str_to_upper str_sub str_c
#' @importFrom tibble tibble
#' @export
dane_tab_D1_zaw <- function(ramka_danych,
                            wartosc_filtrujaca,
                            typ_szk, rok_absolwentow,
                            typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "D1",
      .data$kryterium == "nazwa_zaw",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga"  ||
      nrow(dane_wejsciowe %>%
           filter(nazwa_zaw != "OGÓŁEM",
                  n_SUMA >= 10)) == 0 ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }


    dane_wyjsciowe <- dane_wejsciowe  %>%
      filter(nazwa_zaw != "OGÓŁEM",
             n_SUMA >= 10) %>%
      select(nazwa_zaw, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
      select(where(~ is.factor(.x) || sum(.x) !=0)) %>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw,
             N = n_SUMA)  %>%
      rename_with(
        ~ str_c("pct_",
                str_to_upper(str_sub(., start = 5, end = 5)),
                str_sub(., start = 6, end = -1)
        ), .cols = starts_with("pct_")
      ) %>%
      rename_with(~ str_replace(., "^pct_(.*)", "\\1 (%)"), matches("^pct_"))

  return(dane_wyjsciowe)
}
