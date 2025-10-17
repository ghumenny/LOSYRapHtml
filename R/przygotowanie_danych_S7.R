#' @title Przygotowanie danych do wykresu statusów (miesiące)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `status`, `miesiac` oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresStatusy z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with
#' @importFrom rlang .data
#' @importFrom stringr str_split_fixed
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @export
dane_wyk_S7_mscrok <- function(pelna_finalna_ramka_wskaznikow,
                               typ_szk, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "mscrok",
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

  skroty_miesiecy <- c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie",
                       "wrz", "paź", "lis", "gru")

 dane_wyjsciowe <- dane_wejsciowe  %>%
   filter(S7 != "SUMA") %>%
    select(S7, starts_with("pct_")) %>%
    pivot_longer(!S7, names_to = "miesiac", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(
      split_date = str_split_fixed(miesiac, "\\.", 2),
      numer_mies_num = as.numeric(split_date[, 1]),
      rok = split_date[, 2],
    ) %>%
   mutate(
     miesiac_label = paste0(skroty_miesiecy[.data$numer_mies_num], "'",
                            str_sub(.data$rok, 3, 4)),
     pct = .data$pct / 100,
     status = S7
   ) %>%
   arrange(.data$rok, .data$numer_mies_num) %>%
   mutate(
     miesiac = factor(.data$miesiac_label, levels = unique(.data$miesiac_label))
   ) %>%
   select(status, miesiac, pct)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabeli statusów (miesiące)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji
#'   gentab_tab_S7_mscrok z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where matches starts_with
#' @importFrom rlang .data
#' @importFrom stringr str_split_fixed
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
dane_tab_S7_mscrok <- function(pelna_finalna_ramka_wskaznikow,
                               typ_szk, rok_absolwentow) {


  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "mscrok",
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

  skroty_miesiecy <- c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie",
                       "wrz", "paź", "lis", "gru")

  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(S7 != "SUMA") %>%
    select(S7, starts_with("pct_")) %>%
    pivot_longer(!S7, names_to = "miesiac", values_to = "pct",
                 names_prefix = "pct_") %>%
    pivot_wider(
      names_from = S7,
      values_from = pct
    ) %>%
    mutate(
      split_date = str_split_fixed(miesiac, "\\.", 2),
      numer_mies_num = as.numeric(split_date[, 1]),
      rok = split_date[, 2],
    ) %>%
    mutate(
      Miesiąc = paste0(skroty_miesiecy[.data$numer_mies_num], "'", .data$rok),
      across(where(is.numeric), ~  round(.,digits = 2))
      ) %>%
    select(Miesiąc, matches("^[A-ZŚŁÓŻŹĆĘĄŃ]"), -miesiac, -numer_mies_num, -rok, -starts_with("split") )


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu statusów (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `status`, `plec`, oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresStatusyPlec z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @export
dane_wyk_S7_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow, rok) {

  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "sexf",
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
    filter(S7 != "SUMA") %>%
    select(S7, starts_with("pct_"), -pct_OGÓŁEM) %>%
    pivot_longer(!S7, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(
      pct = .data$pct / 100,
      status = S7,
      plec = if_else(plec == "Mężczyzna", "Mężczyźni", "Kobiety")) %>%
    select(status, plec, pct)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabeli statusów (płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Tworzy ramkę danych
#'   w formacie będącym wejściem do funkcji gentab_tab_S7_plec z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches
#' @importFrom dplyr ends_with rename_with
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @export
dane_tab_S7_plec <- function(pelna_finalna_ramka_wskaznikow,
                             typ_szk, rok_absolwentow, rok) {

  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "sexf",
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
    filter(S7 != "SUMA") %>%
    select(-ends_with("OGÓŁEM")) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2))) %>%
    rename(Status = S7) %>%
    rename_with(~ str_replace(., "^n_", "liczba_"), matches("^n_")) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))


  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do wykresów
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami `status`, `nazwa_zaw`, oraz `pct` (jako odsetek) w formacie
#'   bedącym wejściem do funkcji wykresStatusyZawod z pakietu LOSYkolory.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate slice starts_with
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom stats reorder
#' @export
dane_wyk_S7_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow, rok) {
  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "nazwa_zaw",
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
    filter(nazwa_zaw != "OGÓŁEM") %>%
    slice(1:10) %>%
    select(nazwa_zaw, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
    pivot_longer(!c(nazwa_zaw, n_SUMA), names_to = "status", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(
      status = factor(.data$status, levels = c(
        "Tylko nauka",
        "Nauka i praca",
        "Tylko praca",
        "Bezrobocie",
        "Brak danych o aktywności")),
      nazwa_zaw = reorder(nazwa_zaw, n_SUMA),
      pct = .data$pct / 100) %>%
    select(status, nazwa_zaw, pct)

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabel
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   w formacie bedącym wejściem do funkcji gentab_tab_S7_zawod z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @param tylko_tabele parametr TRUE/FALSE przekazywany z głównej funkcji
#'   generującej raport - jeśli FALSE to przycina tabele z zawodami do 10
#'   najliczniejszych zawodów, jeśli TRUE to raport generuje tabele zawodów bez
#'   przycinania
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches rename_with
#' @importFrom rlang .data
#' @export
dane_tab_S7_zaw <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow, rok, tylko_tabele) {
  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "nazwa_zaw",
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

if(tylko_tabele == FALSE) {
  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(nazwa_zaw != "OGÓŁEM") %>%
    slice(1:10) %>%
    select(nazwa_zaw, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2))) %>%
    rename(Zawód = nazwa_zaw,
           N = n_SUMA) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
} else {
  dane_wyjsciowe <- dane_wejsciowe  %>%
    filter(nazwa_zaw != "OGÓŁEM",
           n_SUMA > 10) %>%
    select(nazwa_zaw, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2))) %>%
    rename(Zawód = nazwa_zaw,
           N = n_SUMA) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_"))
}

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresów
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   w formacie bedącym wejściem do funkcji gentab_tab_S7_powiat z tego pakietu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate across where rename matches
#' @importFrom dplyr arrange starts_with left_join rename_with
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @export
dane_tab_S7_pow <- function(pelna_finalna_ramka_wskaznikow,
                            typ_szk, rok_absolwentow, rok) {

  rok_kal <- rok
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "S7",
      .data$kryterium == "teryt_pow_szk",
      .data$rok == rok_kal,
      .data$typ_szk2 == typ_szk,
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
    filter(teryt_pow_szk != "OGÓŁEM") %>%
    select(teryt_pow_szk, n_SUMA, starts_with("pct_"), -pct_SUMA) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      teryt_pow = teryt_pow_szk) %>%
    left_join(LOSYRapDocx:::teryt_mapowanie, by = "teryt_pow") %>%
    rename(`Teryt powiatu` = teryt_pow_szk,
           N = n_SUMA) %>%
    rename_with(~ str_replace(., "^pct_", "procent_"), matches("^pct_")) %>%
    select(`Teryt powiatu`, Powiat, N, starts_with("procent_")) %>%
    arrange(`Teryt powiatu`)

  return(dane_wyjsciowe)
}



