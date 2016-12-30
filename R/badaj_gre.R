#' Badanie gry
#'
#' Funkcja przeprowadza gre 10.000 razy oraz wylicza statystyki czasu gry.
#'
#' @param strategia Przechowuje funkcje strategii gry ktora bedziemy badac.
#' @param powtorzen Ile razy gra ma byc przeprowadzona
#'
#' @export


badaj_gre <- function(strategia, powtorzen = 10000){
  czasy_gier <- c()

  for(i in 1:powtorzen){
    czasy_gier[i] <- gra(strategia)
  }
  summary(czasy_gier)
}
