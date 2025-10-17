#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `dyplom`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresDyplomyPlec z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr filter pull select mutate %>% if_else
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_sentence
#' @importFrom rlang .data
#' @export
dane_wyk_D1_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D1",
      .data$kryterium == "sexf",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
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


  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(D1 != "SUMA", D1 != "Nie dotyczy") %>%
    select(D1, starts_with("pct_")) %>%
    pivot_longer(!D1, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
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
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr filter pull select mutate %>% across rename rename_with matches
#' @importFrom tidyselect starts_with where all_of
#' @importFrom rlang .data
#' @importFrom stringr str_replace str_to_sentence
#' @importFrom tibble tibble
#' @export
dane_tab_D1_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D1",
      .data$kryterium == "sexf",
      .data$typ_szk2 == {{typ_szk}},
      .data$rok_abs == rok_absolwentow
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

  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(D1 != "SUMA",  n_OGÓŁEM != 0) %>%
    select(D1, n_OGÓŁEM, starts_with("pct_")) %>%
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
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate slice starts_with where
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#' @importFrom stats reorder
#' @export
dane_wyk_D1_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D1",
      .data$kryterium == "nazwa_zaw",
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
    filter(nazwa_zaw != "OGÓŁEM") %>%
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
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param tylko_tabele parametr TRUE/FALSE przekazywany z głównej funkcji
#'   generującej raport - jeśli FALSE to przycina tabele z zawodami do 10
#'   najliczniejszych zawodów, jeśli TRUE to raport generuje tabele zawodów bez
#'   przycinania
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename rename_with
#' @importFrom tidyselect starts_with matches
#' @importFrom rlang .data
#' @importFrom stringr str_replace str_to_upper str_sub str_c
#' @export
dane_tab_D1_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow, tylko_tabele) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D1",
      .data$kryterium == "nazwa_zaw",
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

  if(tylko_tabele == FALSE) {
    dane_wyjsciowe <- dane_wejsciowe  %>%
      filter(nazwa_zaw != "OGÓŁEM") %>%
      slice(1:10) %>%
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
      rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
  } else {
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
      rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
  }
  return(dane_wyjsciowe)
}
