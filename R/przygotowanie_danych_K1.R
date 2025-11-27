#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami  `plec`, `kontynuacja` oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresStatusyPlec z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_wyk_K1_plec <- function(ramka_danych,
                             wartosc_filtrujaca,
                             typ_szk, rok_absolwentow, rok,
                             typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K1",
      .data$kryterium == "sexf",
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

  n_m <- sum(dane_wejsciowe$n_Mężczyzna, na.rm = TRUE)
  n_k <- sum(dane_wejsciowe$n_Kobieta, na.rm = TRUE)
  n_o <- sum(dane_wejsciowe$n_OGÓŁEM, na.rm = TRUE)

  dane_wyjsciowe <- dane_wejsciowe  %>%
    select(`Kontynuacja nauki`, starts_with("pct_")) %>%
    pivot_longer(!`Kontynuacja nauki`, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
    filter(
      (plec == "Mężczyzna" & n_m >= 10) |
        (plec == "Kobieta" & n_k >= 10) |
        (plec == "OGÓŁEM" & n_o >= 10)
    ) %>%
    mutate(
      pct = .data$pct / 100,
      kontynuacja = str_c(str_to_upper(str_sub(`Kontynuacja nauki`, 1, 1)),
                          str_sub(`Kontynuacja nauki`, 2)),
      kontynuacja = factor(.data$kontynuacja, levels = c(
        "W szkole policealnej",
        "Na studiach",
        "W BS II",
        "W liceum dla dorosłych",
        "W ramach KKZ"
      )),
      plec = if_else(plec == "Mężczyzna", "Mężczyźni",
                     if_else(plec == "Kobieta", "Kobiety", "Ogółem")),
      plec = factor(.data$plec, levels = c(
        "Ogółem",
        "Mężczyźni",
        "Kobiety"
        ))) %>%
    select(plec, kontynuacja, pct)
  dane_wyjsciowe$kontynuacja <- droplevels(dane_wyjsciowe$kontynuacja)

  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do tabeli kontynuowania nauki (płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Tworzy ramkę danych
#'   w formacie będącym wejściem do funkcji gentab_tab_S7_plec z tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull mutate across where rename_with matches
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_replace str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @export
dane_tab_K1_plec <- function(ramka_danych,
                             wartosc_filtrujaca,
                             typ_szk, rok_absolwentow, rok,
                             typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K1",
      .data$kryterium == "sexf",
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

  n_m <- sum(dane_wejsciowe$n_Mężczyzna, na.rm = TRUE)
  n_k <- sum(dane_wejsciowe$n_Kobieta, na.rm = TRUE)
  n_o <- sum(dane_wejsciowe$n_OGÓŁEM, na.rm = TRUE)

  if (n_m < 10 && n_k < 10 && n_o < 10) {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Mniej niż 10 absolwentów kontynuujących naukę ogółem."
    ))
  }

  dane_wyjsciowe <- dane_wejsciowe  %>%
    { if (n_m < 10) select(., -ends_with("Mężczyzna")) else . } %>%
    { if (n_k < 10) select(., -ends_with("Kobieta")) else . } %>%
    { if (n_o < 10) select(., -ends_with("OGÓŁEM")) else . } %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      `Kontynuacja nauki` = str_c(str_to_upper(str_sub(`Kontynuacja nauki`, 1, 1)),
                          str_sub(`Kontynuacja nauki`, 2))
) %>%
    rename_with(~ str_replace(., "pct_Kobieta$", "pct_Kobiety"), matches("pct_Kobieta$")) %>%
    rename_with(~ str_replace(., "pct_Mężczyzna$", "pct_Mężczyźni"), matches("pct_Mężczyzna$")) %>%
    rename_with(~ str_replace(., "pct_OGÓŁEM", "pct_Ogółem"), matches("pct_OGÓŁEM")) %>%
    rename_with(~ str_replace(., "^n_", "liczba_"), matches("^n_")) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu statusów (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `status`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresStatusyPlec z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with slice
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_to_upper str_c
#' @importFrom tibble tibble
#' @importFrom stats reorder
#' @export
dane_wyk_K1_zaw <- function(ramka_danych,
                            wartosc_filtrujaca,
                            typ_szk, rok_absolwentow, rok,
                            typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K1",
      .data$kryterium == "nazwa_zaw",
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
    filter(n_SUMA...4 >= 10) |>
    slice(1:10) %>%
    rename(nazwa_zaw = nazwa_zaw...1, n_SUMA = n_SUMA...4) %>%
    select(nazwa_zaw, n_SUMA, starts_with("pct_")) %>%
    pivot_longer(!c(nazwa_zaw, n_SUMA),
                 names_to = "kontynuacja", values_to = "pct",
                 names_prefix = "pct_Kontynuacja nauki ") %>%
    mutate(
      pct = .data$pct / 100,
      kontynuacja = str_c(str_to_upper(str_sub(kontynuacja, 1, 1)),
                          str_sub(kontynuacja, 2)),
      kontynuacja = factor(.data$kontynuacja, levels = c(
        "W szkole policealnej",
        "Na studiach",
        "W BS II",
        "W liceum dla dorosłych",
        "W ramach KKZ")),
      nazwa_zaw = reorder(nazwa_zaw, n_SUMA)) %>%
    select(nazwa_zaw, kontynuacja, pct)
  dane_wyjsciowe$kontynuacja <- droplevels(dane_wyjsciowe$kontynuacja)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu statusów (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `status`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresStatusyPlec z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate rename_with across where rename matches ends_with
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @export
dane_tab_K1_zaw <- function(ramka_danych,
                            wartosc_filtrujaca,
                            typ_szk, rok_absolwentow, rok,
                            typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K1",
      .data$kryterium == "nazwa_zaw",
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
      rename(Zawód = nazwa_zaw...1, N = n_SUMA...4) %>%
      filter(N >= 10) %>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))
      ) %>%
      rename_with(~ str_replace(., "^n_Kontynuacja nauki (.*)",
                                "\\1 (N)"),
                  matches("^n_Kontynuacja nauki ")) %>%
      rename_with(~ str_replace(., "^pct_Kontynuacja nauki (.*)",
                                "\\1 (%)"),
                  matches("^pct_Kontynuacja nauki ")) %>%
      select(Zawód, N, ends_with("(N)"), ends_with("(%)"))

  return(dane_wyjsciowe)
}

