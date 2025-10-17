## code to prepare `DATASET` dataset goes here
teryt_mapowanie <- read.csv("d:\\Dokumenty\\IBE\\Losy_za\\LOSY3\\Administracyjne\\Pakiety\\TERC_Urzedowy_2025-09-03.csv", sep = ";") %>%
  filter(NAZWA_DOD %in% c("powiat", "miasto na prawach powiatu",
                          "miasto stołeczne, na prawach powiatu")) %>%
  mutate(
    teryt_pow = str_c(WOJ, str_pad(POW, width = 2, side = "left", pad = "0")),
    Powiat = NAZWA
  ) %>%
  select(teryt_pow, Powiat)

usethis::use_data(teryt_mapowanie, internal = TRUE)
