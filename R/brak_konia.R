#' Strategia gdy nie ma konia
#'
#' Funkcja jest uzywana gdy nie mamy konia, i opracowuje strategie jakie zwierze powinno byc zamienione na jakie.
#'
#' @param stado_tmp3 Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp3 Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return ruch_wymiana to wektor zwracajacy jakie zwierze ma byc wymienione na jakie

brak_konia <- function(stado_tmp3, wartosc_w_krolikach_tmp3){
  ruch_zamiana <- c(0,0)

  if (wartosc_stada(stado_tmp3, wartosc_w_krolikach_tmp3) >= 72) {
    ruch_zamiana <- c("kon", 50)
  } else if ((stado_tmp3["maly_pies"] == 0 && stado_tmp3["krolik"] > 9) || (stado_tmp3["maly_pies"] == 1 && stado_tmp3["krolik"] > 25) ) {
    ruch_zamiana<-c("krolik", "maly_pies")
  } else if (stado_tmp3["krolik"]  < 27 && stado_tmp3["swinia"] > 0) {
    ruch_zamiana <- c("swinia", "krolik")
  } else if (stado_tmp3["krolik"]  < 33 && stado_tmp3["owca"] > 0 ) {
    ruch_zamiana <-c ("owca", "krolik")
  } else if (stado_tmp3["krolik"] == 0  && stado_tmp3["maly_pies"] > 0 ) {
    ruch_zamiana <- c("maly_pies", "krolik")
  } else if (45 > stado_tmp3["krolik"] && stado_tmp3["krolik"] > 39  && stado_tmp3["owca"] < 23) {
    ruch_zamiana <- c("krolik", "owca")
  } else if (46 < stado_tmp3["krolik"] && stado_tmp3["swinia"] < 20) {
    ruch_zamiana <- c("krolik", "swinia")
  }
  return(ruch_zamiana)
}
