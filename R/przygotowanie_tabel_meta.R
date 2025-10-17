#' @title Generowanie tabeli typów szkół ogółem w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   funkcję przygotowującą dane. Oczekuje kolumn `Typ szkoły`, `liczba`, `procent`.
#' @param edycja Liczba całkowita reprezentująca rok edycji monitoringu.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs
#' @importFrom flextable set_table_properties align add_footer_lines set_header_labels
#' @importFrom dplyr %>%
#' @export
gentab_typsz_flext_og <- function(dane_do_tabeli, edycja) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli)[1] %in% "Uwaga") {
    message("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób.")
    }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = c("liczba", "procent"), align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", edycja, " r.\n\n"), top = FALSE) %>%
    # Możesz dodać dalsze formatowanie, np. nazwy kolumn
    set_header_labels(
      `Typ szkoły` = "Typy szkół",
      liczba = "Liczba", # Przykładowo, jeśli rok pochodzi z glowna_edycja - 2
      procent = "Procent"
    )

  return(tabela_flextable)
}

#' @title Generowanie tabeli typów szkół i płci w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i płci oraz
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   odpowiednią funkcję przygotowującą dane, zawierająca dane o płci.
#' @param edycja Liczba całkowita reprezentująca rok edycji monitoringu.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs
#' @importFrom flextable set_table_properties align add_footer_lines
#' @importFrom dplyr %>%
#' @export
gentab_typsz_flext_plec <- function(dane_do_tabeli, edycja) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli)[1] %in% "Uwaga") {
    message("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób.")
  }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", edycja, " r.\n\n"), top = FALSE) #%>%

  return(tabela_flextable)
}


#' @title Generowanie tabeli zawodów w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane dotyczące zawodów i
#'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   funkcję `dane_wyk_meta_typsz_zaw()`.
#' @param edycja Liczba całkowita reprezentująca rok edycji monitoringu.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs
#' @importFrom flextable set_table_properties align add_footer_lines
#' @importFrom dplyr %>%
#' @export
gentab_typsz_flext_zaw <- function(dane_do_tabeli, edycja) {

  if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli)[1] %in% "Uwaga") {
    message("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób.")
  } else {

  num_cols <- ncol(dane_do_tabeli)
  x <- num_cols / 3
  kolumny_do_centrowania <- c()
  for (n in 0:(x - 1)) {
    kolumny_do_centrowania <- c(kolumny_do_centrowania, n*3 + 2, n*3 + 3)
  }


  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%  # Jeśli nagłówki są złożone (np. rok na górze)
    theme_booktabs() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = kolumny_do_centrowania, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", edycja, " r.\n\n"), top = FALSE)

  return(tabela_flextable)
  }
}
