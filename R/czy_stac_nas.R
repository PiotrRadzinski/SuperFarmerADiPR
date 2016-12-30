#' Sprawdzanie czy gracza stac
#'
#' Sprawdza czy gracza stac na dane zwierze na podstawie wektora stada i wektora cen
#'
#' @param stado_tmp Tymczasowy wektor stada gracza
#' @param nr_zwierzecia Nr zwierzecia ktore chcemy kupic
#' @param ceny Wektor cen zwierzat
#'
#' @return Zwraca wektor stada po wymianie

czy_stac_nas <- function (nr_zwierzecia, stado_tmp, ceny){

  k <-nr_zwierzecia
  if(k == 7) k <- 4 #duzy pies jest warty tyle co krowa
  if(k == 6) k <- 2 #maly pies jest warty tyle co owca

  wartosc_tanszych <- sum ( stado_tmp[1:(nr_zwierzecia-1)] * ceny[ 1:(nr_zwierzecia-1)])
  if (stado_tmp[1] > 0){
    wartosc_tanszych <- wartosc_tanszych - 1  #jednego krolika chcemy zachowywac,
  }
  czy_stac <- ( wartosc_tanszych >= ceny[nr_zwierzecia] )
  return(czy_stac)
}
