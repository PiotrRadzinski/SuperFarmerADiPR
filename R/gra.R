#' Pojedyncza gra
#'
#' Funkcja przeprowadza pojedyncza gre zgodnie z zadana strategia
#'
#' @param strategia Zadana strategia zgodnie z ktora przeprowadzimy gre.
#'
#' @return Zwracamy dlugosc trwania gry (liczbe tur).
#'
#' @export

gra <- function(strategia) {
  stado_gracza <- rep(0, times = 7)
  names(stado_gracza) <- c( "krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  licznik_tur <- 0

  while(licznik_tur < 100000 && czy_wygrana(stado_gracza) == 0) {

    stado_gracza <- strategia(stado_gracza)
    if (czy_wygrana(stado_gracza)) break

    na_kostkach <- rzut_kostkami()
    stado_gracza <- wilk_i_lis(stado_gracza, na_kostkach)
    stado_gracza <- rozmnazanie(stado_gracza, na_kostkach)

    licznik_tur <- licznik_tur + 1
  }

  return(licznik_tur)
}

