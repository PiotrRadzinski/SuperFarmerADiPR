#' Przeliczanie stada na kroliki
#'
#' Funkcja przelicza ile nasze stado jest warte w krolikach
#'
#' @param stado_tmp Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return Zwraca wartosc stada w krolikach

wartosc_stada <- function(stado_tmp, wartosc_w_krolikach_tmp){
  wartosc <- 0
  for(j in 1:7){
    wartosc <- wartosc + stado_tmp[j] * wartosc_w_krolikach_tmp[j]
  }
  return(wartosc)
}
