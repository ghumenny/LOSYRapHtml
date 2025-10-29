#' @title Przygotowanie danych do tabeli (typy szkół ogółem)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Filtruje dane dla całej Polski, dla wskaźnika
#'   'typ_szk2'. Przekształca dane i oblicza procenty oraz liczebności
#'   w formacie zgodnym z raportowaniem.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
##' @param wartosc_filtrujaca Wartość (np. nazwa województwa lub branży), której szukamy.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie.
#' @importFrom dplyr %>% |> filter pull bind_rows select mutate summarise arrange
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @export
dane_tab_meta_typsz_og <- function(ramka_danych,
                                   wartosc_filtrujaca,
                                           rok_absolwentow,
                                           typ = WOJ_NAZWA) {

    dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>%
    `[[`(1)

  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryterii."
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


  finalna_tabela <- dane_szkoly %>%
    bind_rows(branzowa_ogolem)

  if (typ == "WOJ_NAZWA") {
    finalna_tabela <- finalna_tabela %>%
    filter(typ_szk2 %in% c(
      "Liceum ogólnokształcące dla młodzieży",
      "Liceum ogólnokształcące dla dorosłych",
      "Technikum",
      "Branżowa szkoła II stopnia",
      "Szkoła policealna",
      "Branżowa szkoła I stopnia - ogółem",
      "Młodociani w Branżowej szkole I stopnia",
      "Niemłodociani w Branżowej szkole I stopnia",
      "Szkoła specjalna przysposabiająca do pracy"
    )) |>
    mutate(
      `Typ szkoły` = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące dla młodzieży",
        "Liceum ogólnokształcące dla dorosłych",
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
      filter(!is.na(`Typ szkoły`)) %>%
    arrange(`Typ szkoły`)
  } else {
    finalna_tabela <- finalna_tabela %>%
      filter(typ_szk2 %in% szkoly_z_zawodem) |>
      mutate(
        `Typ szkoły` = factor(.data$typ_szk2, levels = c(
          "Branżowa szkoła I stopnia - ogółem",
          "Młodociani w Branżowej szkole I stopnia",
          "Niemłodociani w Branżowej szkole I stopnia",
          "Technikum",
          "Branżowa szkoła II stopnia",
          "Szkoła policealna"
        )),
        liczba = round(.data$n_SUMA),
        procent = round(.data$pct_OGÓŁEM, 2)
      ) %>%
      select(-typ_szk2, -n_SUMA, -pct_OGÓŁEM) %>%
      filter(!is.na(`Typ szkoły`)) %>%
      arrange(`Typ szkoły`)
  }

  return(finalna_tabela)
}



#' @title Przygotowanie danych do wykresu typów szkół (ogółem)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   z kolumnami `rok_abs`, `typ_szk` oraz `pct` (jako odsetek).
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa lub branży), której szukamy.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie.
#' @importFrom dplyr %>% filter pull bind_rows select mutate summarise arrange
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_typsz_og <- function(ramka_danych,
                                   wartosc_filtrujaca,
                                   rok_absolwentow,
                                   typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>%
    `[[`(1)

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

  if (typ == "WOJ_NAZWA") {
  dane_wyjsciowe <- finalne_dane_surowe %>%
    mutate(
      rok_abs = rok_absolwentow,
      typ_szk = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące dla młodzieży",
        "Liceum ogólnokształcące dla dorosłych",
        "Technikum",
        "Branżowa szkoła II stopnia",
        "Szkoła policealna",
        "Branżowa szkoła I stopnia",
        "Szkoła specjalna przysposabiająca do pracy"
      )),
      pct = .data$pct_OGÓŁEM / 100 # Konwersja procentu na odsetek
    ) %>%
    select(rok_abs, typ_szk, pct) %>%
    filter(!is.na(typ_szk))
  } else {
    dane_wyjsciowe <- finalne_dane_surowe %>%
      mutate(
        rok_abs = rok_absolwentow,
        typ_szk = factor(.data$typ_szk2, levels = c(
          "Technikum",
          "Branżowa szkoła II stopnia",
          "Szkoła policealna",
          "Branżowa szkoła I stopnia"
        )),
        pct = .data$pct_OGÓŁEM / 100 # Konwersja procentu na odsetek
      ) %>%
      select(rok_abs, typ_szk, pct) %>%
      filter(!is.na(typ_szk))
  }


  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do tabeli (typy szkół i płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabel. Filtruje dane dla całej Polski,
#'   dla wskaźnika 'typ_szk2'. Oblicza liczebności i procenty w formacie
#'   zgodnym z raportowaniem.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa lub branży), której szukamy.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów i tabel.
#' @importFrom dplyr %>% filter pull bind_rows select mutate summarise arrange if_else
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @export
dane_tab_meta_typsz_plec <- function(ramka_danych,
                                     wartosc_filtrujaca,
                                     rok_absolwentow,
                                     typ = WOJ_NAZWA) {

  kolumna_filtrujaca <- rlang::enquo(typ)

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>%
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
      pct_Mężczyzna = sum(.data$n_Mężczyzna)/(sum(.data$n_Mężczyzna)+sum(.data$n_Kobieta))*100,
      pct_Kobieta = sum(.data$n_Kobieta)/(sum(.data$n_Mężczyzna)+sum(.data$n_Kobieta))*100
    )

  # Łączenie danych
  finalna_tabela <- dane_szkoly %>%
    bind_rows(branzowa_ogolem)

  if (typ == "WOJ_NAZWA") {
    finalna_tabela <- finalna_tabela %>%
    filter(typ_szk2 %in% c(
      "Liceum ogólnokształcące dla młodzieży",
      "Liceum ogólnokształcące dla dorosłych",
      "Technikum",
      "Branżowa szkoła II stopnia",
      "Szkoła policealna",
      "Branżowa szkoła I stopnia - ogółem",
      "Młodociani w Branżowej szkole I stopnia",
      "Niemłodociani w Branżowej szkole I stopnia",
      "Szkoła specjalna przysposabiająca do pracy"
    )) %>%
    mutate(
      `Typ szkoły` = factor(.data$typ_szk2, levels = c(
        "Liceum ogólnokształcące dla młodzieży",
        "Liceum ogólnokształcące dla dorosłych",
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
      filter(!is.na(`Typ szkoły`)) %>%
    arrange(`Typ szkoły`)
  } else {
    finalna_tabela <- finalna_tabela %>%
      filter(typ_szk2 %in% szkoly_z_zawodem) %>%
      mutate(
        `Typ szkoły` = factor(.data$typ_szk2, levels = c(
          "Technikum",
          "Branżowa szkoła II stopnia",
          "Szkoła policealna",
          "Branżowa szkoła I stopnia - ogółem",
          "Młodociani w Branżowej szkole I stopnia",
          "Niemłodociani w Branżowej szkole I stopnia"
        )),
        liczba_Meżczyzna = round(.data$n_Mężczyzna),
        liczba_Kobieta = round(.data$n_Kobieta),
        procent_Mężczyzna = round(.data$pct_Mężczyzna, 2),
        procent_Kobieta = round(.data$pct_Kobieta, 2)
      ) %>%
      select(-typ_szk2, -n_Mężczyzna, -n_Kobieta, -pct_Mężczyzna, -pct_Kobieta) %>%
      filter(!is.na(`Typ szkoły`)) %>%
      arrange(`Typ szkoły`)
  }

  return(finalna_tabela)
}



#' @title Przygotowanie danych do wykresu typów szkół (płeć)
#' @description Funkcja filtruje i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Tworzy ramkę danych
#'   z kolumnami `rok_abs`, `typ_szk`, `plec` oraz `pct` (jako odsetek).
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa lub branży), której szukamy.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów i tabel.
#' @importFrom dplyr %>% filter pull bind_rows select mutate summarise arrange if_else
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_typsz_plec <- function(ramka_danych,
                                     wartosc_filtrujaca,
                                     rok_absolwentow,
                                     typ = WOJ_NAZWA) {

  kolumna_filtrujaca <- rlang::enquo(typ)

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "typ_szk2",
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>%
    `[[`(1)

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

  if (typ == "WOJ_NAZWA") {
    dane_wyjsciowe <- bind_rows(pozostale_dane, branzowa_ogolem) %>%
      mutate(
        typ_szk = factor(.data$typ_szk2, levels = c(
          "Szkoła specjalna przysposabiająca do pracy",
          "Branżowa szkoła I stopnia",
          "Szkoła policealna",
          "Branżowa szkoła II stopnia",
          "Technikum",
          "Liceum ogólnokształcące dla dorosłych",
          "Liceum ogólnokształcące dla młodzieży"
        ))) %>%
      select(typ_szk, starts_with("pct_")) %>%
      pivot_longer(!typ_szk, names_to = "plec", values_to = "pct",
                   names_prefix = "pct_") %>%
      mutate(pct = .data$pct / 100,
             plec = if_else(plec == "Mężczyzna", "Mężczyźni", "Kobiety")
    ) %>%
    filter(!is.na(typ_szk))
  } else {
    dane_wyjsciowe <- bind_rows(pozostale_dane, branzowa_ogolem) %>%
      mutate(
        typ_szk = factor(.data$typ_szk2, levels = c(
          "Branżowa szkoła I stopnia",
          "Szkoła policealna",
          "Branżowa szkoła II stopnia",
          "Technikum"
        ))) %>%
      select(typ_szk, starts_with("pct_")) %>%
      pivot_longer(!typ_szk, names_to = "plec", values_to = "pct",
                   names_prefix = "pct_") %>%
      mutate(pct = .data$pct / 100,
             plec = if_else(plec == "Mężczyzna", "Mężczyźni", "Kobiety")
      ) %>%
      filter(!is.na(typ_szk))
      }


  return(dane_wyjsciowe)
}



#' @title Przygotowanie danych do wykresu zawodów
#' @description Funkcja filtruje dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresów. Zwraca surową ramkę danych
#'   z zagnieżdżonego obiektu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa lub branży), której szukamy.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull mutate across
#' @importFrom rlang .data enquo !!
#' @importFrom tibble tibble
#' @importFrom tidyselect where
#' @export
dane_wyk_meta_typsz_zaw <- function(ramka_danych,
                                    wartosc_filtrujaca,
                                    rok_absolwentow,
                                    typ = WOJ_NAZWA) {

  kolumna_filtrujaca <- rlang::enquo(typ)

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "meta_zaw",
      .data$rok_abs == {{rok_absolwentow}}
    ) %>%
    pull(.data$wynik) %>% `[[`(1)


  if (is.null(dane_wejsciowe)  ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych do wygenerowania tabeli. Zwracam pusty obiekt flextable.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }
  dane_wyjsciowe <- dane_wejsciowe %>%
    filter(if_any(ends_with("_n"), ~ . != 0)) %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 1)))
  return(dane_wyjsciowe)
}



#' @title Przygotowanie danych do wykresu z migracjami
#' @description Funkcja filtruje dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania kartogramu z migracjami.
#'   Zwraca surową ramkę danych z zagnieżdżonego obiektu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param woj_nazwa Zmienna tekstowa nazwa województwa.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów do filtrowania.
#' @param rok Liczba całkowita reprezentująca rok do filtrowania.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull arrange mutate select
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @export
dane_wyk_meta_migracje <- function(ramka_danych,
                                   woj_nazwa, rok_absolwentow, rok) {

  eval_rok <- rok
  dane_wejsciowe <- ramka_danych %>%
    filter(
      .data$WOJ_NAZWA == {{woj_nazwa}},
      .data$wskaznik == "migracje",
      .data$rok_abs == {{rok_absolwentow}},
      .data$rok == {{eval_rok}}
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
    filter(wartość != "ŁĄCZNIE"| is.na(wartość)) %>%
    mutate(pct = round(częstość / 100,digits = 4),
           teryt_woj = as.integer(wartość)) %>%
    arrange(teryt_woj) %>%
    select(teryt_woj, pct)
  return(dane_wyjsciowe)
}

