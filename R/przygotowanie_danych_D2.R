#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `dyplom`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresMaturyPlec z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr filter pull select mutate %>% if_else case_when starts_with
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @export
dane_wyk_D2_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D2",
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
    filter(D2 != "SUMA", D2 != "Nie dotyczy") %>%
    select(D2, starts_with("pct_")) %>%
    pivot_longer(!D2, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(
      pct = .data$pct / 100,
      dyplom = case_when(
        D2 == "Uzyskał świadectwo maturalne" ~ "Uzyskanie świadectwa dojrzałości",
        D2 == "Brak świadectwa maturalnego" ~ "Brak świadectwa dojrzałości",
        TRUE ~ D2),
      plec = if_else(plec == "Mężczyzna", "Mężczyźni",
                     if_else(plec == "Kobieta", "Kobiety", "ogółem"))) %>%
    select(plec, dyplom, pct)




  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabel
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr filter pull select mutate %>% across where rename rename_with matches
#' @importFrom tidyselect starts_with
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tidyr pivot_longer
#' @export
dane_tab_D2_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D2",
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
    filter(D2 != "SUMA", D2 != "Nie dotyczy") %>%
    select(D2, n_OGÓŁEM, starts_with("pct_")) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      `Egzamin maturalny` = case_when(
        D2 == "Uzyskał świadectwo maturalne" ~ "Uzyskanie świadectwa dojrzałości",
        D2 == "Brak świadectwa maturalnego" ~ "Brak świadectwa dojrzałości",
        TRUE ~ D2)) %>%
    rename(N = n_OGÓŁEM) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_")) %>%
  select(`Egzamin maturalny`, N, starts_with("procent_"))

  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `dyplom`, `nazwa_zaw`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji WykresMaturyZawod z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate slice starts_with case_when
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stats reorder
#' @export
dane_wyk_D2_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D2",
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
    pivot_longer(!c(nazwa_zaw, n_SUMA), names_to = "dyplom", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(dyplom = case_when(
      dyplom == "Uzyskał świadectwo maturalne" ~ "Uzyskanie świadectwa dojrzałości",
      dyplom == "Brak świadectwa maturalnego" ~ "Brak świadectwa dojrzałości",
      TRUE ~ dyplom),
      dyplom = factor(.data$dyplom, levels = c(
        "Uzyskanie świadectwa dojrzałości",
        "Brak świadectwa dojrzałości")),
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
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param tylko_tabele parametr TRUE/FALSE przekazywany z głównej funkcji
#'   generującej raport - jeśli FALSE to przycina tabele z zawodami do 10
#'   najliczniejszych zawodów, jeśli TRUE to raport generuje tabele zawodów bez
#'   przycinania
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches rename_with
#' @importFrom tidyselect starts_with
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @export
dane_tab_D2_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow, tylko_tabele) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "D2",
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
      select(nazwa_zaw, n_SUMA, starts_with("pct_"),
             -any_of(c("pct_Nie dotyczy", "pct_SUMA"))) %>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw,
             N = n_SUMA)  %>%
      rename_with(~ str_replace(., "pct_Uzyskał świadectwo maturalne",
                                "pct_Uzyskanie świadectwa dojrzałości"),
                  matches("pct_Uzyskał świadectwo maturalne")) %>%
      rename_with(~ str_replace(., "pct_Brak świadectwa maturalnego",
                                "pct_Brak świadectwa dojrzałości"),
                  matches("pct_Brak świadectwa maturalnego")) %>%
      rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
  } else {
    dane_wyjsciowe <- dane_wejsciowe  %>%
      filter(nazwa_zaw != "OGÓŁEM",
             n_SUMA >= 10) %>%
      select(nazwa_zaw, n_SUMA, starts_with("pct_"),
             -any_of(c("pct_Nie dotyczy", "pct_SUMA"))) %>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw,
             N = n_SUMA)  %>%
      rename_with(~ str_replace(., "pct_Uzyskał świadectwo maturalne",
                                "pct_Uzyskanie świadectwa dojrzałości"),
                  matches("pct_Uzyskał świadectwo maturalne")) %>%
      rename_with(~ str_replace(., "pct_Brak świadectwa maturalnego",
                                "pct_Brak świadectwa dojrzałości"),
                  matches("pct_Brak świadectwa maturalnego")) %>%
      rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
  }
  return(dane_wyjsciowe)
}
