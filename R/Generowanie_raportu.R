#' @title Generowanie raportu z danymi o absolwentach do pliku DOCX
#' @description Funkcja generuje raport w formacie DOCX, łącząc wstęp z
#'   raportami dla każdego typu szkoły w pętli.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wskaźniki.
#' @param woj_nazwa Nazwa województwa (np. "Województwo Małopolskie"). Argument jest obowiązkowy.
#' @param edycja Wartość liczbowa (2025) do użycia w nazwach plików oraz
#'        w podpisach tabel i wykresów.
#' @param rok_abs Rok absolwentów.
#' @param wersja wartosć liczbowa okreslajaca kolejną generowaną w danym roku
#'        wersję pliku .html.
#' @param tylko_tabele Wartość logiczna (TRUE/FALSE). Jeśli TRUE, generuje raport
#'        zawierający tylko tabele. W przeciwnym wypadku dołącza też wykresy.
#' @return Zapisuje wygenerowany raport w pliku .docx.
#' @importFrom rmarkdown render
#' @importFrom knitr knit_expand knit
#' @importFrom bookdown word_document2
#' @export
generuj_raport_html <- function(pelna_finalna_ramka_wskaznikow, woj_nazwa,
                                edycja, rok_abs, wersja, tylko_tabele = FALSE) {

  typ_szkoly <- c(
    "Liceum ogólnokształcące dla młodzieży",
    "Liceum ogólnokształcące dla dorosłych",
    "Branżowa szkoła I stopnia",
    "Młodociani w Branżowej szkole I stopnia",
    "Niemłodociani w Branżowej szkole I stopnia",
    "Technikum",
    "Branżowa szkoła II stopnia",
    "Szkoła policealna",
    "Szkoła specjalna przysposabiająca do pracy"
  )
  if (tylko_tabele == FALSE) {
  nazwa_pliku = paste0("Raport_dla_województwa_", dopelniacz_w(woj_nazwa),"_ed_",edycja, "_dla_rocznika_",rok_abs, "_v_",wersja,".html")
  } else {
  nazwa_pliku = paste0("Aneks_tabelatyczny_do_Raportu_dla_województwa_", dopelniacz_w(woj_nazwa),"_ed_",edycja, "_dla_rocznika_",rok_abs, "_v_",wersja,".html")
  }

  raport_content <- list()
  if (tylko_tabele == FALSE) {
    raport_content[[1]] <- c(
      knit_expand(
        system.file("/szablony_raportu/wstep.Rmd", package = "LOSYRapHtml"),
        pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
        woj_nazwa = woj_nazwa,
        edycja = edycja,
        rok_abs = rok_abs,
        rok = rok_abs),
      knit_expand(
        system.file("/szablony_raportu/meta.Rmd", package = "LOSYRapHtml"),
        pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
        woj_nazwa = woj_nazwa,
        edycja = edycja,
        rok_abs = rok_abs,
        rok = rok_abs))
  } else {
    raport_content[[1]] <- knit_expand(
        system.file("/szablony_raportu/meta.Rmd", package = "LOSYRapHtml"),
        pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
        woj_nazwa = woj_nazwa,
        edycja = edycja,
        rok_abs = rok_abs,
        rok = rok_abs)
  }

  for (i in seq_along(typ_szkoly)) {
    obecny_typ_szk <- typ_szkoly[i]

    # Dodanie wszystkich szablonów dla danego typu szkoły
    raport_content[[length(raport_content) + 1]] <- c(knit_expand(
      system.file('/szablony_raportu/S6.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/D2.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/D1.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/K1.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/W1.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
      edycja = edycja,
      rok_abs = rok_abs,
      rok = rok_abs
    ),knit_expand(
      system.file('/szablony_raportu/B1.Rmd', package = "LOSYRapHtml"),
      typ_szk = obecny_typ_szk,
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj_nazwa,
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
                    output_format = rmarkdown::html_document(),
                    output_file = nazwa_pliku,
                    output_dir = "raport",
                    quiet = TRUE,
                    clean = TRUE)

  message(paste("Raport został pomyślnie wygenerowany do pliku:", nazwa_pliku))
}

#' @title Generowanie 16 raportów wojewódzkich
#' @description Funkcja iteruje przez listę województw i wywołuje
#'   `generuj_raport_docx` dla każdego z nich.
#' @param pelna_finalna_ramka_wskaznikow Ramka danych zawierająca pełne wskaźniki.
#' @param edycja Wartość liczbowa (2025) do użycia w nazwach plików.
#' @param rok_abs Rok absolwentów.
#' @param wersja Wartość liczbowa określająca kolejną generowaną wersję pliku .docx.
#' @param tylko_tabele Wartość logiczna (TRUE/FALSE).
#' @export
generuj_raporty_wojewodzkie <- function(pelna_finalna_ramka_wskaznikow,
                                        edycja, rok_abs, wersja, tylko_tabele = FALSE) {

  wojewodztwa <- c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie",
                   "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie",
                   "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie",
                   "wielkopolskie", "zachodniopomorskie"
  )

  message("--- Rozpoczęto generowanie raportów wojewódzkich ---")

  for (woj in wojewodztwa) {
    generuj_raport_html(
      pelna_finalna_ramka_wskaznikow = pelna_finalna_ramka_wskaznikow,
      woj_nazwa = woj,
      edycja = edycja,
      rok_abs = rok_abs,
      wersja = wersja,
      tylko_tabele = tylko_tabele # Przekazanie nazwy województwa
    )
  }

  message("--- Pomyślnie wygenerowano wszystkie raporty wojewódzkie ---")
}

#' @title Generowanie elastycznego raportu (województwo lub branża)
#' @description Funkcja generuje raport w formacie HTML dla danego województwa lub branży.
#' @param ramka_danych Ramka danych zawierająca pełne wskaźniki (wojewódzka lub branżowa).
#' @param nazwa_agregacji Nazwa województwa lub branży.
#' @param typ_raportu Typ raportu: "woj" (domyślnie) lub "bran".
#' @param edycja Wartość liczbowa (np. 2025) do użycia w nazwach plików.
#' @param rok_abs Rok absolwentów.
#' @param wersja Wartość liczbowa określająca wersję pliku.
#' @param tylko_tabele Wartość logiczna (TRUE/FALSE).
#' @return Zapisuje wygenerowany raport w pliku .html.
#' @importFrom rmarkdown render
#' @importFrom knitr knit_expand
#' @export
generuj_raport <- function(ramka_danych, nazwa_agregacji, typ_raportu = "woj",
                           edycja, rok_abs, wersja, tylko_tabele = FALSE) {

  if (typ_raportu == "bran") {
    typ_rapo <- "branza"
    typ_szkoly <- c(
      "Branżowa szkoła I stopnia",
      "Młodociani w Branżowej szkole I stopnia",
      "Niemłodociani w Branżowej szkole I stopnia",
      "Technikum",
      "Branżowa szkoła II stopnia",
      "Szkoła policealna"
    )
  } else {
    typ_rapo <- "WOJ_NAZWA"
    typ_szkoly <- c(
      "Liceum ogólnokształcące dla młodzieży",
      "Liceum ogólnokształcące dla dorosłych",
      "Branżowa szkoła I stopnia",
      "Młodociani w Branżowej szkole I stopnia",
      "Niemłodociani w Branżowej szkole I stopnia",
      "Technikum",
      "Branżowa szkoła II stopnia",
      "Szkoła policealna",
      "Szkoła specjalna przysposabiająca do pracy"
    )
  }



  prefix <- if (tylko_tabele) "Aneks_tabelatyczny_do_Raportu_dla_" else "Raport_dla_"
  nazwa_pliku <- paste0(prefix, dopelniacz(nazwa_agregacji),
                        "_ed_", edycja, "_dla_rocznika_", rok_abs, "_v_", wersja, ".html")


  raport_content <- list()

  if (tylko_tabele == FALSE) {
    raport_content[[1]] <- c(
      knit_expand(
        system.file("/szablony_raportu/wstep_2.Rmd", package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = "", edycja = edycja,
        rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file("/szablony_raportu/meta_2.Rmd", package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = "", edycja = edycja,
        rok_abs = rok_abs, rok = rok_abs
      )
    )
  } else {
    raport_content[[1]] <- knit_expand(
      system.file("/szablony_raportu/meta_2.Rmd", package = "LOSYRapHtml"),
      ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
      typ_rapo = typ_rapo, typ_szk = "", edycja = edycja,
      rok_abs = rok_abs, rok = rok_abs
    )
  }

  # Główna pętla po typach szkół

  for (obecny_typ_szk in typ_szkoly) {
    raport_content[[length(raport_content) + 1]] <- c(
      knit_expand(
        system.file('/szablony_raportu/S6_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file('/szablony_raportu/D2_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file('/szablony_raportu/D1_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file('/szablony_raportu/K1_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file('/szablony_raportu/W1_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      ),
      knit_expand(
        system.file('/szablony_raportu/B1_2.Rmd', package = "LOSYRapHtml"),
        ramka_danych = ramka_danych, nazwa_jednostki = nazwa_agregacji,
        typ_rapo = typ_rapo, typ_szk = obecny_typ_szk,
        edycja = edycja, rok_abs = rok_abs, rok = rok_abs
      )
    )
  }

  # 4. Renderowanie raportu (bez zmian)
  final_raport_text <- paste(unlist(raport_content), collapse = '\n')
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(final_raport_text, temp_rmd)
  knit_meta(clean = TRUE)
  rmarkdown::render(
    input = temp_rmd,
    output_format = "html_document",
    output_file = nazwa_pliku,
    output_dir = "raport",
    encoding = "windows-1250",
    quiet = TRUE,
    clean = TRUE
  )
  knit_meta(clean = TRUE)
  `___nrTabeli___` = 1
  `___nrWykresu___` = 1
  message(paste("Raport został pomyślnie wygenerowany do pliku:", nazwa_pliku))
}


#' @title Generowanie serii raportów wojewódzkich lub branżowych
#' @description Funkcja, w zależności od parametru `typ`, iteruje przez listę
#'   województw lub branż i wywołuje dla każdego z nich funkcję `generuj_raport`.
#' @param ramka_danych Ramka danych zawierająca pełne wskaźniki (wojewódzkie lub branżowe).
#' @param typ Rodzaj generowanych raportów: "woj" (domyślnie) dla wojewódzkich
#'   lub "bran" dla branżowych.
#' @param edycja Wartość liczbowa (np. 2025) do użycia w nazwach plików.
#' @param rok_abs Rok absolwentów.
#' @param wersja Wartość liczbowa określająca wersję pliku.
#' @param tylko_tabele Wartość logiczna (TRUE/FALSE).
#' @export
generuj_raporty_html <- function(ramka_danych,
                                 typ = "woj",
                                 edycja, rok_abs, wersja, tylko_tabele = FALSE) {

  if (typ == "woj") {
    # --- Blok logiczny dla raportów wojewódzkich ---

    # Sprawdzenie, czy w ramce danych istnieje kolumna 'WOJ_NAZWA'
    if (!"WOJ_NAZWA" %in% names(ramka_danych)) {
      stop("BŁĄD: Dla typ='woj' ramka danych musi zawierać kolumnę 'WOJ_NAZWA'.")
    }

    kategorie <- c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie",
                   "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie",
                   "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie",
                   "wielkopolskie", "zachodniopomorskie")

    message("--- Rozpoczęto generowanie raportów wojewódzkich ---")
    for (kategoria in kategorie) {
      generuj_raport(
        ramka_danych = ramka_danych,
        nazwa_agregacji = kategoria,
        typ_raportu = "woj",
        edycja = edycja, rok_abs = rok_abs, wersja = wersja, tylko_tabele = tylko_tabele
      )
    }
    message("--- Pomyślnie wygenerowano wszystkie raporty wojewódzkie ---")

  } else if (typ == "bran") {
    # --- Blok logiczny dla raportów branżowych ---

    # Sprawdzenie, czy w ramce danych istnieje kolumna 'branza'
    if (!"branza" %in% names(ramka_danych)) {
      stop("BŁĄD: Dla typ='bran' ramka danych musi zawierać kolumnę 'branza'.")
    }

    kategorie <- pobierz_aktywne_branze(
      ramka_danych = ramka_danych,
      rok_absolwentow = rok_abs,
      prog = 10
    )

      # c("branża hotelarsko-gastronomiczno-turystyczna",
      #              "branża elektroniczno-mechatroniczna",
      #              "branża mechaniczna",
      #              "branża budowlana",
      #              "branża rolno-hodowlana",
      #              "branża motoryzacyjna",
      #              "branża handlowa",
      #              "branża spedycyjno-logistyczna",
      #              "branża ekonomiczno-administracyjna",
      #              "branża fryzjersko-kosmetyczna",
      #              "branża spożywcza",
      #              "branża audiowizualna",
      #              #"branża poligraficzna",
      #              "branża teleinformatyczna",
      #              "branża transportu lotniczego",
      #              "branża chemiczna i ochrony środowiska",
      #              "branża ochrony i bezpieczeństwa osób i mienia",
      #              "branża elektroenergetyczna",
      #              "branża leśna",
      #              "branża ogrodnicza",
      #              "branża drzewno-meblarska",
      #              "branża opieki zdrowotnej",
      #              "branża mechaniki precyzyjnej",
      #              "branża przemysłu mody",
      #              "branża górniczo-wiertnicza",
      #              "branża transportu wodnego",
      #              "branża metalurgiczna",
      #              "branża transportu kolejowego",
      #              "branża transportu drogowego",
      #              "branża pomocy społecznej",
      #              "branża poligraficzno-księgarska",
      #              "branża ceramiczna-szklarska",
      #              "branża rybacka" )

    message("--- Rozpoczęto generowanie raportów branżowych ---")
    for (kategoria in kategorie) {
      generuj_raport(
        ramka_danych = ramka_danych,
        nazwa_agregacji = kategoria,
        typ_raportu = "bran",
        edycja = edycja, rok_abs = rok_abs, wersja = wersja, tylko_tabele = tylko_tabele
      )
    }
    message("--- Pomyślnie wygenerowano wszystkie raporty branżowe ---")

  } else {
    stop("Nierozpoznany parametr 'typ'. Dostępne opcje to 'woj' lub 'bran'.")
  }
}