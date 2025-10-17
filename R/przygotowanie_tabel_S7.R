#' @title Generowanie tabeli statusów w formacie flextable
#' @description Funkcja przyjmuje przygotowane dane ze wskaźnikiem statusowym i
#'   tworzy z nich sformatowaną tabelę flextable do wstawienia do raportu Word.
#'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#'   `dane_tab_S7_...`.
#' @param edycja Wartość liczbowa - rok edycji monitoringu.
#' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' @importFrom flextable flextable separate_header theme_booktabs colformat_num
#' @importFrom flextable set_table_properties align add_footer_lines
#' @importFrom dplyr %>%
#' @export
gentab_tab_S7 <- function(dane_do_tabeli, edycja) {

  # Sprawdzenie, czy dane wejściowe nie są puste
  if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli)[1] %in% "Uwaga") {
    message("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób.")
    }

  # Tworzenie flextable i stosowanie formatowania
  tabela_flextable <- dane_do_tabeli %>%
    flextable() %>%
    separate_header() %>%
    theme_booktabs() %>%
    colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    align(j = -1, align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
    align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
    add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", edycja, " r.\n\n"), top = FALSE) #%>%

  return(tabela_flextable)
}

### okazało się że wszystkie funkcję są idealnie takie same
#' #' @title Generowanie tabeli w rozdziale Status w formacie flextable
#' #' @description Funkcja przyjmuje przygotowane dane ze wskaźnikim statusowym  i
#' #'   tworzy z nich sformatowaną tabelę flextable do wstawienia do raportu Word.
#' #'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' #' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#' #'   `dane_tab_S7_wscrok` .
#' #' @param glowna_edycja wartość liczbowa - rok edycji monitoringu
#' #' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' #' @importFrom flextable flextable separate_header theme_booktabs
#' #' @importFrom flextable align_nottext_col add_footer_lines set_table_properties
#' #' @export
#' gentab_tab_S7_mscrok <- function(dane_do_tabeli, glowna_edycja) {
#'
#'   # Sprawdzenie, czy dane wejściowe nie są puste
#'   if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli) %in% "Uwaga") {
#'     message("Brak danych do wygenerowania tabeli.")
#'     return(cat0("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób."))
#'   }
#'
#'   # Tworzenie flextable i stosowanie formatowania
#'   tabela_flextable <- dane_do_tabeli %>%
#'     flextable() %>%
#'     separate_header() %>%
#'     theme_booktabs() %>%
#'     colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
#'     set_table_properties(width = 1, layout = "autofit") %>%
#'     align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
#'     align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
#'     add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) #%>%
#'
#'   return(tabela_flextable)
#' }
#'
#' #' @title Generowanie tabeli typów szkół w formacie flextable
#' #' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#' #'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#' #'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' #' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#' #'   `przygotuj_dane_meta_typ_szkoly`.
#' #' @param glowna_edycja wartość liczbowa - rok edycji monitoringu
#' #' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' #' @importFrom flextable flextable separate_header theme_booktabs
#' #' @importFrom flextable align_nottext_col add_footer_lines set_table_properties
#' #' @export
#' gentab_tab_S7_plec <- function(dane_do_tabeli, glowna_edycja) {
#'
#'   # Sprawdzenie, czy dane wejściowe nie są puste
#'   if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli) %in% "Uwaga") {
#'     message("Brak danych do wygenerowania tabeli.")
#'     return(cat0("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób."))
#'   }
#'
#'   # Tworzenie flextable i stosowanie formatowania
#'   tabela_flextable <- dane_do_tabeli %>%
#'     flextable() %>%
#'     separate_header() %>%
#'     theme_booktabs() %>%
#'     colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
#'     set_table_properties(width = 1, layout = "autofit") %>%
#'     align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
#'     align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
#'     add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) #%>%
#'
#'   return(tabela_flextable)
#' }
#'
#'
#' #' @title Generowanie tabeli typów szkół w formacie flextable
#' #' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#' #'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#' #'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' #' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#' #'   `przygotuj_dane_meta_typ_szkoly`.
#' #' @param glowna_edycja wartość liczbowa - rok edycji monitoringu
#' #' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' #' @importFrom flextable flextable separate_header theme_booktabs
#' #' @importFrom flextable align_nottext_col add_footer_lines set_table_properties
#' #' @export
#' gentab_tab_S7_zawod <- function(dane_do_tabeli, glowna_edycja) {
#'
#'   # Sprawdzenie, czy dane wejściowe nie są puste
#'   if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli) %in% "Uwaga") {
#'     message("Brak danych do wygenerowania tabeli.")
#'     return(cat0("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób."))
#'   }
#'   # Tworzenie flextable i stosowanie formatowania
#'   tabela_flextable <- dane_do_tabeli %>%
#'     flextable() %>%
#'     separate_header() %>%
#'     theme_booktabs() %>%
#'     colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
#'     set_table_properties(width = 1, layout = "autofit") %>%
#'     align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
#'     align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
#'     add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) #%>%
#'
#'   return(tabela_flextable)
#' }
#'
#'
#' #' @title Generowanie tabeli typów szkół w formacie flextable
#' #' @description Funkcja przyjmuje przygotowane dane dotyczące typów szkół i
#' #'   tworzy z nich sformatowaną tabelę flextable, gotową do wstawienia do raportu Word.
#' #'   Stosuje styl booktabs, dopasowuje szerokość i dodaje źródło danych.
#' #' @param dane_do_tabeli Ramka danych w formacie przygotowanym przez
#' #'   `przygotuj_dane_meta_typ_szkoly`.
#' #' @param glowna_edycja wartość liczbowa - rok edycji monitoringu
#' #' @return Obiekt typu `flextable` sformatowany zgodnie z wymaganiami.
#' #' @importFrom flextable flextable separate_header theme_booktabs
#' #' @importFrom flextable align_nottext_col add_footer_lines set_table_propertie
#' #' @export
#' gentab_tab_S7_powiat <- function(dane_do_tabeli, glowna_edycja) {
#'
#'   # Sprawdzenie, czy dane wejściowe nie są puste
#'   if (is.null(dane_do_tabeli) || colnames(dane_do_tabeli) %in% "Uwaga") {
#'     message("Brak danych do wygenerowania tabeli.")
#'     return(cat0("Nie udostępnia się zagregowanych wyników monitoringu karier absolwentów obejmujących mniej niż 10 osób."))
#'   }
#'
#'   # Tworzenie flextable i stosowanie formatowania
#'   tabela_flextable <- dane_do_tabeli %>%
#'     flextable() %>%
#'     separate_header() %>%
#'     theme_booktabs() %>%
#'     colformat_num(j = -1, digits = 2, decimal.mark = ",", big.mark = " ") %>%
#'     set_table_properties(width = 1, layout = "autofit") %>%
#'     align(align = "center", part = "header") %>% # Wyśrodkowanie nagłówków
#'     align(j = -1, align = "center", part = "body") %>% # Wyśrodkowanie kolumn liczbowych w ciele tabeli
#'     add_footer_lines(values = paste0("Źródło: Dane z monitoringu karier absolwentów szkół ponadpodstawowych pozyskane w ", glowna_edycja, " r.\n\n"), top = FALSE) #%>%
#'
#'   return(tabela_flextable)
#' }
#'
