#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania wykresu. Tworzy ramkę danych
#'   z kolumnami  `plec`, `kontynuacja` oraz `pct` (jako odsetek) w formacie
#'   będącym wejściem do funkcji wykresKontynuacjeDziedzinyPlec z pakietu LOSYkolory.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate if_else arrange desc slice ends_with
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
dane_wyk_K2dzi_plec <- function(ramka_danych,
                                wartosc_filtrujaca,
                                typ_szk, rok_absolwentow,
                                typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dziedziny",
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
    select(sexf, starts_with("procent_")) %>%
    pivot_longer(!sexf, names_to = "dziedzina", values_to = "pct",
                 names_prefix = "procent_") %>%
    mutate(
      pct = .data$pct / 100,
      dziedzina = factor(.data$dziedzina, levels = c(
        "nauk teologicznych",
        "nauk weterynaryjnych",
        "sztuki",
        "nauk rolniczych",
        "nauk ścisłych i przyrodniczych",
        "nauk humanistycznych",
        "nauk inżynieryjno-technicznych",
        "nauk medycznych i nauk o zdrowiu",
        "nauk społecznych"
      )),
      plec = if_else(sexf == "Mężczyzna", "Mężczyźni",
                     if_else(sexf == "Kobieta", "Kobiety", "Ogółem")),
      plec = factor(.data$plec, levels = c(
        "Ogółem",
        "Mężczyźni",
        "Kobiety"
      ))) %>%
    select(plec, dziedzina, pct)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate if_else arrange desc rename
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
dane_tab_K2dzi_plec <- function(ramka_danych,
                                wartosc_filtrujaca,
                                typ_szk, rok_absolwentow,
                                typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dziedziny",
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
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                   if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","Dziedzina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      Dziedzina = if_else(!is.na(Dziedzina), Dziedzina, "ogółem")) |>
    arrange(desc(Ogółem_liczba))


  return(dane_wyjsciowe)
}


#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull select mutate arrange desc across where rename rename_with matches
#' @importFrom rlang .data enquo !!
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @export
dane_tab_K2dzi_zaw <- function(ramka_danych,
                               wartosc_filtrujaca,
                               typ_szk, rok_absolwentow,
                               typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K2",
      .data$kryterium == "nazwa_zaw",
      .data$parametr_K2 == "dziedziny",
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
      arrange(desc(liczba)) %>%
      filter(liczba > 10) %>%
      select(nazwa_zaw, liczba, starts_with("procent_"))%>%
      mutate(
        across(where(is.numeric), ~  round(.,digits = 2))) %>%
      rename(Zawód = nazwa_zaw) %>%
      rename_with(~ str_replace(., "^procent_(.*)", "\\1 (%)"), matches("^procent_"))

  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull mutate if_else across where arrange desc slice
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
dane_tab_K2dys_plec <- function(ramka_danych,
                                wartosc_filtrujaca,
                                typ_szk, rok_absolwentow,
                                typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dyscypliny",
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
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                          if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","Dyscyplina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    mutate(
      across(where(is.numeric), ~  round(.,digits = 2)),
      Dyscyplina = if_else(!is.na(Dyscyplina), Dyscyplina, "ogółem")) %>%
    arrange(desc(Ogółem_liczba)) %>%
    slice(2:11)


  return(dane_wyjsciowe)
}

#' @title Przygotowanie danych do tabeli dyscyplin kontynuacji nauki (w zawodach)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych w formacie
#'   będącym wejściem do funkcji gentab_K2dys_zaw z tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
#' @importFrom dplyr %>% filter pull mutate arrange desc slice rename select across where bind_cols
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom tibble tibble as_tibble
#' @export
dane_tab_K2dys_zaw <- function(ramka_danych,
                               wartosc_filtrujaca,
                               typ_szk, rok_absolwentow,
                               typ = WOJ_NAZWA) {

   dane_wejsciowe <- ramka_danych %>%
     filter(
       if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
       .data$wskaznik == "K2",
       .data$kryterium == "nazwa_zaw",
       .data$parametr_K2 == "dyscypliny",
       .data$typ_szk2 == {{typ_szk}},
       .data$rok_abs == {{rok_absolwentow}}
     ) %>%
     pull(.data$wynik) %>% `[[`(1)

  # --- Krok 2: Sprawdzenie pustych danych (bez zmian) ---
  if (is.null(dane_wejsciowe) ||
      colnames(dane_wejsciowe)[1] %in% "Uwaga") {
    message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
    return(tibble(
      Uwaga = "Brak danych wejściowych dla podanych kryteriów."
    ))
  }

  # --- Krok 3: Sortowanie i NOWE ZABEZPIECZENIE (liczba wierszy) ---

  # Sortujemy dane wejściowe RAZ na początku
  dane_wejsciowe_posortowane <- dane_wejsciowe %>%
    as_tibble() %>%
    arrange(desc(liczba))

  # Sprawdzamy, czy oprócz wiersza "Ogółem" (zakładamy, że jest to wiersz 1)
  # istnieją jakiekolwiek dane o zawodach (wiersz 2 lub więcej).
  if (nrow(dane_wejsciowe_posortowane) < 2) {
    message("Brak danych o zawodach (jest tylko 'Ogółem' lub brak danych). Zwracam pustą ramkę.")
    return(tibble(
      Uwaga = "Brak danych o zawodach do przetworzenia."
    ))
  }

  # --- Krok 4: NOWA, dynamiczna pętla ---

  # Określamy, ile wierszy przetworzyć:
  # Zaczynamy od wiersza 2 (pierwszy zawód).
  # Kończymy na ostatnim DOSTĘPNYM wierszu, ale nie dalej niż wiersz 6 (piąty zawód).
  max_n <- min(nrow(dane_wejsciowe_posortowane), 6)

  # Lista do przechowywania wyników dla każdego zawodu
  lista_wynikow <- list()

  # Pętla przetwarza od pierwszego zawodu (n=2) do ostatniego dostępnego (max_n)
  for (n in 2:max_n) {

    # Wyciągamy dane dla bieżącego zawodu (n)
    nazwa_zawodu_n <- dane_wejsciowe_posortowane[["nazwa_zaw"]][n]
    liczba_zawodu_n <- dane_wejsciowe_posortowane[["liczba"]][n]

    # Tworzymy dynamiczne nazwy na potrzeby `separate` i `colnames`
    separator_name <- paste0(nazwa_zawodu_n, ' (liczba absolwentów ', format(liczba_zawodu_n, big.mark = " "), ')_Dyscyplina')
    final_col_name <- paste0(nazwa_zawodu_n, ' (liczba absolwentów ', format(liczba_zawodu_n, big.mark = " "), ')_procent')

    temp <- dane_wejsciowe_posortowane %>%
      slice(n) %>%
      rename(liczba_Ogółem = liczba,
             procent_Ogółem = procent) %>%
      tidyr::pivot_longer(liczba_Ogółem : ncol(.), # `ncol(.)` odnosi się do 1-wierszowej ramki
                          names_to = "typ2",
                          values_to = "value") %>%
      tidyr::separate(typ2, c('forma', separator_name), sep = '_') %>%
      tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
                         values_from = c("value"),
                         names_sep = "_",
                         names_repair = "unique") %>%
      arrange(desc(.[[2]])) %>% # Sortuj wg liczby (kolumna 2)
      slice(2:6) %>%           # Weź top 5 dyscyplin
      select(1, 3)             # Wybierz dyscyplinę (kol 1) i procent (kol 3)

    # Ustawiamy docelową nazwę kolumny procentowej
    colnames(temp)[2] <- final_col_name

    # --- Zabezpieczenie przed duplikowaniem kolumny dyscyplin ---
    # Jeśli to *nie jest* pierwszy zawód (n > 2), usuwamy kolumnę dyscyplin,
    # ponieważ zostanie ona dodana tylko z pierwszej ramki (n=2).
    # if (n > 2) {
    #   temp <- temp %>% select(2) # Zostaw tylko kolumnę z procentami
    # }

    lista_wynikow[[n - 1]] <- temp # Dodajemy ramkę do listy
  }

  # --- Krok 5: Połączenie wyników ---

  # Łączymy wszystkie ramki z listy w jedną, kolumnami
  dane_wyjsciowe <- dplyr::bind_cols(lista_wynikow) %>%
    mutate(
      across(where(is.numeric), ~  round(., digits = 2))
    )

  return(dane_wyjsciowe)
}

# #' @title Przygotowanie danych do tabeli dyscyplin kontynuacji nauki (w zawodach)
# #' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
# #'   aby przygotować je do generowania tabeli. Tworzy ramkę danych w formacie
# #'   będącym wejściem do funkcji gentab_K2dys_zaw z tego pakietu.
# #' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
# #'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
# #' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
# #' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
# #' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
# #' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
# #'   Podawana bez cudzysłowu.
# #' @return Ramka danych typu tibble w finalnym formacie do zasilania tabel.
# #' @importFrom dplyr %>% filter pull mutate arrange desc slice rename select across where
# #' @importFrom rlang .data enquo !!
# #' @importFrom tidyr pivot_longer pivot_wider separate
# #' @importFrom tibble tibble as_tibble
# #' @export
# dane_tab_K2dys_zaw <- function(ramka_danych,
#                                wartosc_filtrujaca,
#                                typ_szk, rok_absolwentow,
#                                typ = WOJ_NAZWA) {
#
#   dane_wejsciowe <- ramka_danych %>%
#     filter(
#       if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
#       .data$wskaznik == "K2",
#       .data$kryterium == "nazwa_zaw",
#       .data$parametr_K2 == "dyscypliny",
#       .data$typ_szk2 == {{typ_szk}},
#       .data$rok_abs == {{rok_absolwentow}}
#     ) %>%
#     pull(.data$wynik) %>% `[[`(1)
#
#   if (is.null(dane_wejsciowe) ||
#       colnames(dane_wejsciowe)[1] %in% "Uwaga") {
#     message("Brak danych wejściowych dla podanych kryteriów. Zwracam pustą ramkę danych.")
#     return(tibble(
#       Uwaga = "Brak danych wejściowych dla podanych kryteriów."
#     ))
#   }
#
#
#   dane_wyjsciowe <-   dane_wejsciowe %>%
#     arrange(desc(liczba)) %>% #
#     as_tibble() %>%
#     slice(2) %>%
#     rename(liczba_Ogółem = liczba,
#            procent_Ogółem = procent) %>%
#     tidyr::pivot_longer(liczba_Ogółem :ncol(dane_wejsciowe),
#                         names_to = "typ2",
#                         values_to = "value") %>%
#     tidyr::separate(typ2, c('forma', paste0(dane_wejsciowe[["nazwa_zaw"]][2],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][2],big.mark = " "), ')_Dyscyplina')), sep = '_') %>%
#     tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
#                        values_from = c("value"),
#                        names_sep = "_",
#                        names_repair = "unique") %>%
#     arrange(desc(.[[2]])) %>%
#     slice(2:6) %>%
#     select(1,3)
#   colnames(dane_wyjsciowe)[2] <- paste0(dane_wejsciowe[["nazwa_zaw"]][2],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][2],big.mark = " "), ')_procent')
#
#   for (n in 3:6) {
#     temp <- dane_wejsciowe %>%
#       as_tibble() %>%
#       slice(n) %>%
#       rename(liczba_Ogółem = liczba,
#              procent_Ogółem = procent) %>%
#       tidyr::pivot_longer(liczba_Ogółem :ncol(dane_wejsciowe),
#                           names_to = "typ2",
#                           values_to = "value") %>%
#       tidyr::separate(typ2, c('forma', paste0(dane_wejsciowe[["nazwa_zaw"]][n],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][n],big.mark = " "), ')_Dyscyplina')), sep = '_') %>%
#       tidyr::pivot_wider(names_from = c("nazwa_zaw", "forma"),
#                          values_from = c("value"),
#                          names_sep = "_",
#                          names_repair = "unique") %>%
#       arrange(desc(.[[2]])) %>%
#       slice(2:6) %>%
#       select(1,3)
#     colnames(temp)[2] <- paste0(dane_wejsciowe[["nazwa_zaw"]][n],' (liczba absolwentów ',format(dane_wejsciowe[["liczba"]][n],big.mark = " "), ')_procent')
#
#
#
#     dane_wyjsciowe = cbind(
#       dane_wyjsciowe,
#       temp) %>%
#       mutate(
#         across(where(is.numeric), ~  round(.,digits = 2)))
#   }
#
#
#
#   return(dane_wyjsciowe)
# }

#' @title Przygotowanie danych do wykresu kontynuacji nauki (płeć)
#' @description Funkcja wyciąga i przetwarza dane z pełnej ramki wskaźników,
#'   aby przygotować je do generowania tabeli. Tworzy ramkę danych
#'   z kolumnami w formacie będącym wejściem do funkcji gentab_tab_D z
#'   tego pakietu.
#' @param ramka_danych Ramka danych zawierająca pełne wyniki wskaźników.
#'   Oczekuje, że kolumna 'wynik' zawiera zagnieżdżone ramki danych.
#' @param wartosc_filtrujaca Wartość (np. nazwa województwa), której szukamy.
#' @param typ_szk Zmienna tekstowa opisująca typ szkoły.
#' @param rok_absolwentow Liczba całkowita reprezentująca rok absolwentów.
#' @param typ Nazwa kolumny, po której filtrujemy. Domyślnie `WOJ_NAZWA`.
#'   Podawana bez cudzysłowu.
#' @return Ramka danych typu tibble w finalnym formacie do zasilania wykresów.
#' @importFrom dplyr %>% filter pull select mutate if_else arrange desc slice ends_with
#' @importFrom rlang .data enquo !!
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
dane_wyk_K2dys_plec <- function(ramka_danych,
                                wartosc_filtrujaca,
                                typ_szk, rok_absolwentow,
                                typ = WOJ_NAZWA) {

  dane_wejsciowe <- ramka_danych %>%
    filter(
      if_all(all_of(typ), ~ . ==  wartosc_filtrujaca),
      .data$wskaznik == "K2",
      .data$kryterium == "sexf",
      .data$parametr_K2 == "dyscypliny",
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
    mutate(sexf = if_else(sexf == "Mężczyzna", "Mężczyźni",
                          if_else(sexf == "Kobieta", "Kobiety", "Ogółem"))) %>%
    pivot_longer(2:ncol(dane_wejsciowe),
                 names_to = c("liczba","dyscyplina"),
                 values_to = "wartosc",
                 names_sep = "_") %>%
    pivot_wider(names_from = c(sexf,liczba),
                values_from = wartosc,
                names_sep = "_") %>%
    arrange(desc(Ogółem_liczba)) %>%
    select(dyscyplina, ends_with("_procent")) %>%
    slice(2:11)  %>%
    mutate(
      dyscyplina = factor(dyscyplina, levels = unique(rev(dyscyplina)))
    ) %>%
    pivot_longer(2:4,
                 names_to = c("plec"),
                 values_to = "pct") %>%
    mutate(
      plec = if_else(plec == "Kobiety_procent", "Kobiety",
                     if_else(plec == "Mężczyźni_procent", "Mężczyźni", "Ogółem")),
      plec = factor(.data$plec, levels = c(
        "Ogółem",
        "Mężczyźni",
        "Kobiety"
      )),
      pct = pct/100)


  return(dane_wyjsciowe)
}