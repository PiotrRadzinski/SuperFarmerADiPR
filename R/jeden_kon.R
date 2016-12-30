#' Strategia gdy mamy jednego konia
#'
#' Funkcja jest uzywana gdy mamy dokladnie jednego konia, i opracowuje strategie jakie zwierze powinno byc zamienione na jakie.
#'
#' @param stado_tmp6 Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp6 Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return ruch_zamiana to wektor zwracajacy jakie zwierze ma byc wymienione na jakie

jeden_kon <- function( stado_tmp6, wartosc_w_krolikach_tmp6 ){
  ruch_zamiana <- c(0,0)
  if ( wartosc_stada(stado_tmp6, wartosc_w_krolikach_tmp6) >= 144 ) {
    ruch_zamiana <- c("kon", 50)
  } else if (wartosc_stada(stado_tmp6, wartosc_w_krolikach_tmp6) >= 127 ) {
    ruch_zamiana <- jeden_kon_wiecej_niz_127(stado_tmp6)
  } else if (wartosc_stada(stado_tmp6, wartosc_w_krolikach_tmp6) < 127) {
    ruch_zamiana <- jeden_kon_mniej_niz_127(stado_tmp6)
  }
  return (ruch_zamiana)
}
