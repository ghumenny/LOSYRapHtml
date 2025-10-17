#' @title Generowanie raportu z danymi o absolwentach do pliku DOCX
#' @description Funkcja generuje raport w formacie DOCX, łącząc wstęp z
#'   raportami dla każdego typu szkoły w pętli.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wskaźniki.
#' @param edycja Wartość liczbowa (2025) do użycia w nazwach plików oraz
#'        w podpisach tabel i wykresów.
#' @param rok_abs Rok absolwentów.
#' @param wersja wartosć liczbowa okreslajaca kolejną generowaną w danym roku
#'        wersję pliku .docx.
#' @param tylko_tabele Wartość logiczna (TRUE/FALSE). Jeśli TRUE, generuje raport
#'        zawierający tylko tabele. W przeciwnym wypadku dołącza też wykresy.
#' @return Zapisuje wygenerowany raport w pliku .docx.
#' @importFrom rmarkdown render
#' @importFrom knitr knit_expand knit
#' @importFrom bookdown word_document2
#' @export
generuj_raport_docx <- function(pelna_finalna_ramka_wskaznikow, edycja, rok_abs,
                               wersja, tylko_tabele = FALSE) {

  typ_szkoly <- c(
    "Liceum ogólnokształcące",
    "Liceum dla dorosłych",
    "Branżowa szkoła I stopnia",
    "Młodociani w Branżowej szkole I stopnia",
    "Niemłodociani w Branżowej szkole I stopnia",
    "Technikum",
    "Branżowa szkoła II stopnia",
    "Szkoła policealna",
    "Szkoła specjalna przysposabiająca do pracy"
  )
  if (tylko_tabele == FALSE) {
  nazwa_pliku = paste0("Raport_Polska_ed_",edycja, "_dla_rocznika_",rok_abs, "_v_",wersja,".docx")
  } else {
  nazwa_pliku = paste0("Aneks_tabelatyczny_do_Raportu_Polska_ed_",edycja, "_dla_rocznika_",rok_abs, "_v_",wersja,".docx")
  }

  raport_content <- list()
  if (tylko_tabele == FALSE) {
  raport_content[[1]] <- c(knit_expand(
      system.file("/szablony_raportu/wstep.Rmd", package = "LOSYRapDocx"),
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs),
      knit_expand(
        system.file("/szablony_raportu/meta.Rmd", package = "LOSYRapDocx"),
        pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
        edycja = edycja,
        rok_abs = rok_abs,
        rok = rok_abs))
  } else {
    raport_content[[1]] <- knit_expand(
        system.file("/szablony_raportu/meta.Rmd", package = "LOSYRapDocx"),
        pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
        edycja = edycja,
        rok_abs = rok_abs,
        rok = rok_abs)
  }

  for (i in seq_along(typ_szkoly)) {
    obecny_typ_szk <- typ_szkoly[i]

    # Dodanie wszystkich szablonów dla danego typu szkoły
    raport_content[[length(raport_content) + 1]] <- c(knit_expand(
      system.file('/szablony_raportu/S6.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/D2.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/D1.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/K1.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/W1.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/B1.Rmd', package = "LOSYRapDocx"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    )
   )
  }

  final_raport_text <- paste(unlist(raport_content), collapse = '\n')
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(final_raport_text, temp_rmd)

  # Renderowanie tymczasowego pliku markdown do docx
  rmarkdown::render(input = temp_rmd,
                    output_format = "bookdown::word_document2",
                    output_file = nazwa_pliku,
                    output_dir = "raport",
                    encoding = "windows-1250",
                    quiet = TRUE,
                    clean = TRUE)

  message(paste("Raport został pomyślnie wygenerowany do pliku:", nazwa_pliku))
}