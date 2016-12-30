#' Oddawanie zwierzat ze stada gracza
#'
#' Oddaje zwierzeta wybranego rodzaju ze stada gracza
#'
#' @param nr_zwierzecia Indeks zwierzecia, ktore chcemy oddac
#' @param liczba Liczba zwierzat danego gatunku, ktore chcemy oddac
#' @param stado_tmpB Tymczasowy wektor stada gracza
#'
#' @return Zwraca wektor stada po zmianie


oddaj_zwierze <- function(nr_zwierzecia, liczba, stado_tmpB){

  stado_tmpB <- dodaj_do_stada_gracza(nr_zwierzecia, -(liczba), stado_tmpB)
  return(stado_tmpB)
}
