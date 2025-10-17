#' @title Przygotowanie danych do tabeli (typy szkół ogółem)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Filtruje dane dla całej Polski, dla wskaźnika
#'   'typ_szk2'. Przekształca dane i oblicza procenty oraz liczebności
#'   w formacie zgodnym z raportowaniem.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów i tabel.
#' @importFrom dplyr %>% filter pull bind_rows select mutate summarise arrange
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_tab_meta_typsz_og <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>%
    # Wynik jest listą ramek danych, więc bierzemy pierwszy element
    # Zakładamy, że jest tylko jeden wiersz spełniający kryteria
    `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  # Wybór odpowiednich kolumn z tabelki
  dane_szkoly <- dane_wejsciowe %>%
    filter(.data$typ_szk2 != "OGÓŁEM") %>%
    select(typ_szk2, n_SUMA, pct_OGÓŁEM)

  # Agregacja dla "Branżowa szkoła I stopnia - ogółem"
  branzowa_ogolem <- dane_wejsciowe %>%
    filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia",
                                 "Niemłodociani w Branżowej szkole I stopnia")) %>%
    summarise(
      typ_szk2 = "Branżowa szkoła I stopnia - ogółem",
      n_SUMA = sum(.data$n_SUMA),
      pct_OGÓŁEM = sum(.data$pct_OGÓŁEM)
    )

  # Łączenie danych
  finalna_tabela <- dane_szkoly %>%
    bind_rows(branzowa_ogolem) %>%
     mutate(
      `Typ szkoły` = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące",
        "Liceum dla dorosłych",
        "Technikum",
        "Branżowa szkoła II stopnia",
        "Szkoła policealna",
        "Branżowa szkoła I stopnia - ogółem",
        "Młodociani w Branżowej szkole I stopnia",
        "Niemłodociani w Branżowej szkole I stopnia",
        "Szkoła specjalna przysposabiająca do pracy"
      )),
      liczba = round(.data$n_SUMA),
      procent = round(.data$pct_OGÓŁEM, 2)
    ) %>%
    select(-typ_szk2, -n_SUMA, -pct_OGÓŁEM) %>%
    arrange(`Typ szkoły`)

  return(finalna_tabela)
}

#' @title Przygotowanie danych do wykresu typów szkół (ogółem)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   z kolumnami `rok_abs`, `typ_szk` oraz `pct` (jako odsetek).
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate bind_rows summarise
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_typsz_og <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

rok_abs <- rok_absolwentow
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == rok_abs
    ) %>%
    pull(.data$wynik) %>% `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }


  branzowa_ogolem <- dane_wejsciowe %>%
    filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia")) %>%
    summarise(
      typ_szk2 = "Branżowa szkoła I stopnia",
      pct_OGÓŁEM = sum(.data$pct_OGÓŁEM, na.rm = TRUE)
    )

  # 3. Wybranie i przefiltrowanie pozostałych danych
  # Usuwamy "OGÓŁEM", "Młodociani..." i "Niemłodociani..."
  pozostale_dane <- dane_wejsciowe %>%
    filter(!.data$typ_szk2 %in% c("OGÓŁEM", "Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia")) %>%
    select(typ_szk2, pct_OGÓŁEM)

  # 4. Łączenie zagregowanych i pozostałych danych
  finalne_dane_surowe <- bind_rows(pozostale_dane, branzowa_ogolem)

  # 5. Ostateczne przekształcenie do formatu wyjściowego
  dane_wyjsciowe <- finalne_dane_surowe %>%
    mutate(
      rok_abs = rok_absolwentow,
      typ_szk = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące",
        "Liceum dla dorosłych",
        "Technikum",
        "Branżowa szkoła II stopnia",
        "Szkoła policealna",
        "Branżowa szkoła I stopnia",
        "Szkoła specjalna przysposabiająca do pracy"
      )),
      pct = .data$pct_OGÓŁEM / 100 # Konwersja procentu na odsetek
    ) %>%
    select(rok_abs, typ_szk, pct)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabeli (typy szkół i płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Filtruje dane dla całej Polski,
#'   dla wskaźnika 'typ_szk2'. Oblicza liczebności i procenty w formacie
#'   zgodnym z raportowaniem.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów i tabel.
#' @importFrom dplyr %>% filter pull bind_rows select mutate summarise arrange
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_tab_meta_typsz_plec <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>%
    # Wynik jest listą ramek danych, więc bierzemy pierwszy element
    # Zakładamy, że jest tylko jeden wiersz spełniający kryteria
    `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  # Wybór odpowiednich kolumn z tabelki
  dane_szkoly <- dane_wejsciowe %>%
    filter(.data$typ_szk2 != "OGÓŁEM") %>%
    select(typ_szk2, n_Mężczyzna, n_Kobieta, pct_Mężczyzna, pct_Kobieta)

  # Agregacja dla "Branżowa szkoła I stopnia - ogółem"
  branzowa_ogolem <- dane_wejsciowe %>%
    filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia")) %>%
    summarise(
      typ_szk2 = "Branżowa szkoła I stopnia - ogółem",
      n_Mężczyzna = sum(.data$n_Mężczyzna),
      n_Kobieta = sum(.data$n_Kobieta),
      pct_Mężczyzna = sum(.data$pct_Mężczyzna),
      pct_Kobieta = sum(.data$pct_Kobieta)
    )

  # Łączenie danych
  finalna_tabela <- dane_szkoly %>%
    bind_rows(branzowa_ogolem) %>%
    mutate(
      `Typ szkoły` = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące",
        "Liceum dla dorosłych",
        "Technikum",
        "Branżowa szkoła II stopnia",
        "Szkoła policealna",
        "Branżowa szkoła I stopnia - ogółem",
        "Młodociani w Branżowej szkole I stopnia",
        "Niemłodociani w Branżowej szkole I stopnia",
        "Szkoła specjalna przysposabiająca do pracy"
      )),
      liczba_Meżczyzna = round(.data$n_Mężczyzna),
      liczba_Kobieta = round(.data$n_Kobieta),
      procent_Mężczyzna = round(.data$pct_Mężczyzna, 2),
      procent_Kobieta = round(.data$pct_Kobieta, 2)
    ) %>%
    select(-typ_szk2, -n_Mężczyzna, -n_Kobieta, -pct_Mężczyzna, -pct_Kobieta) %>%
    arrange(`Typ szkoły`)

  return(finalna_tabela)
}


#' @title Przygotowanie danych do wykresu typów szkół (płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   z kolumnami `rok_abs`, `typ_szk`, `plec` oraz `pct` (jako odsetek).
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate bind_rows summarise starts_with if_else
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_typsz_plec <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

  # 1. Filtrowanie danych zgodnie z wymaganiami
  # WOJ_NAZWA = "Polska"
  # wskaznik = "typ_szk2"
  # rok_abs = podany parametr funkcji
  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "typ_szk2",
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

  # 2. Agregacja dla "Branżowa szkoła I stopnia - ogółem"
  branzowa_ogolem <- dane_wejsciowe %>%
    filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia")) %>%
    summarise(
      typ_szk2 = "Branżowa szkoła I stopnia",
      pct_Mężczyzna = (sum(.data$n_Mężczyzna)/(sum(.data$n_Mężczyzna)+sum(.data$n_Kobieta)))*100,
      pct_Kobieta = (sum(.data$n_Kobieta)/(sum(.data$n_Mężczyzna)+sum(.data$n_Kobieta)))*100
    )

  # 3. Wybranie i przefiltrowanie pozostałych danych
  # Usuwamy "OGÓŁEM", "Młodociani..." i "Niemłodociani..."
  pozostale_dane <- dane_wejsciowe %>%
    filter(!.data$typ_szk2 %in% c("OGÓŁEM", "Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia")) %>%
    select(typ_szk2, pct_Mężczyzna, pct_Kobieta)

  # 4. Łączenie zagregowanych i pozostałych danych
  dane_wyjsciowe <- bind_rows(pozostale_dane, branzowa_ogolem) %>%
    mutate(
      typ_szk = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące",
        "Liceum dla dorosłych",
        "Technikum",
        "Branżowa szkoła II stopnia",
        "Szkoła policealna",
        "Branżowa szkoła I stopnia",
        "Szkoła specjalna przysposabiająca do pracy"
      ))) %>%
    select(typ_szk, starts_with("pct_")) %>%
    pivot_longer(!typ_szk, names_to = "plec", values_to = "pct",
                 names_prefix = "pct_") %>%
    mutate(pct = .data$pct / 100,
           plec = if_else(plec == "Mężczyzna", "Mężczyźni", "Kobiety")
    )


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu zawodów
#' @description Funkcja filtruje dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Zwraca surową ramkę danych
#'   z zagnieżdżonego obiektu.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_typsz_zaw <- function(pelna_finalna_ramka_wskaznikow, rok_absolwentow) {

  dane_wejsciowe <- pelna_finalna_ramka_wskaznikow %>%
    filter(
      .data$WOJ_NAZWA == "Polska",
      .data$wskaznik == "meta_zaw",
      .data$rok_abs == rok_absolwentow
    ) %>%
    pull(.data$wynik) %>% `[[`(1)
  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_wejsciowe)  ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych do wygenerowania tabeli. Zwracam pusty obiekt flextable.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }
  dane_wyjsciowe <- dane_wejsciowe %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 1)))
  return(dane_wyjsciowe)
}
