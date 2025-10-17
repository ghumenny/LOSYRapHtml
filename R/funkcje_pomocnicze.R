#' @title Generowanie tabeli statusów w formacie flextable
#' @description funkcja tworzaca dopełniacz nazwy typu szkoły
#' @param typ_szkoly Wartość tekstowa opisująca typ szkoły
#' @return zwraca dopełniach typu szkoły w forie tekstowej
dopelniacz <- function(typ_szkoly) {
  switch(typ_szkoly,
         "Liceum dla dorosłych" = "Liceów dla dorosłych",
         "Liceum ogólnokształcące" = "Liceów ogólnokształcących",
         "Branżowa szkoła I stopnia" = "Branżowych szkół I stopnia",
         "Młodociani w Branżowej szkole I stopnia" = "Branżowych szkół I stopnia, którzy kształcili się będąc pracownikami młodocianymi",
         "Niemłodociani w Branżowej szkole I stopnia" = "Branżowych szkół I stopnia, którzy kształcili się będąc pracownikami niemłodocianymi",
         "Szkoła policealna" = "Szkół policealnych",
         "Szkoła specjalna przysposabiająca do pracy" = "Szkół specjalnych przysposabiających do pracy",
         "Technikum" = "Techników",
         "Branżowa szkoła II stopnia" = "Branżowych szkół II stopnia",
         typ_szkoly # Domyślna wartość w przypadku braku dopasowania
  )
}

#' @title Generowanie kolejnych numerów tabeli
#' @description Funkcja do wstawiania kolejnych numerów tabeli w renderowanym
#'              dokumencie
#' @return zwraca kolejny nr tabeli
tab_num = function() {
  nr = get0("___nrTabeli___", parent.frame(), ifnotfound = 1)
  assign("___nrTabeli___", nr + 1, envir = parent.frame())
  return(nr)
}
#' @title Generowanie kolejnych numerów wykresów
#' @description Funkcja do wstawiania kolejnych numerów wykresów w renderowanym
#'              dokumencie
#' @return zwraca kolejny nr wykresu
wyk_num = function() {
  nr = get0("___nrWykresu___", parent.frame(), ifnotfound = 1)
  assign("___nrWykresu___", nr + 1, envir = parent.frame())
  return(nr)
}

#' @title Generowanie ciągów tekstowych bez spacji
#' @description Funkcja do łączenia tekstów bez spacji funkcja skopiowana z
#'               https://rdrr.io/cran/rosetta/src/R/cat0.R
#' @param ... Obiekty, które mają zostać połączone.
#' @param sep Separator, który ma zostać użyty. Domyślnie jest to pusty ciąg znaków.
#' @return zwraca ciąg tekstowy
cat0 <- function(..., sep="") {
  return(cat(..., sep=sep));
}

#' @title odwrotność funkcji `%in%`
#' @description odwraca wektor 0/1 gnerowy przez `%in%`
#' @param x Wartości do sprawdzenia.
#' @param y Wartości, w których należy szukać dopasowania.
#' @return zwraca wektor 1/0
`%ni%` <- function(x, y) {
  !(x %in% y)
}