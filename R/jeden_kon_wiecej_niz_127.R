#' Strategia gdy mamy konia i duzo innych zwierzat
#'
#' Funkcje uruchamiamy gdy mamy jednego konia i na tyle duzo innych zwierzat ze mozliwa jest juz wygrana. Funkcja opracowuje najlepsza dla nas zamiane
#'
#' @param stado_tmp4 Przechowuje wartosc naszego stada
#'
#' @return ruch_zamana zwraca jakie zwierze ma byc wymienione na jakie

jeden_kon_wiecej_niz_127 <- function( stado_tmp4 ){
  ruch_zamiana <- c(0,0)
  if (stado_tmp4["krolik"] == 0 && stado_tmp4["krowa"] > 1 ) {
    ruch_zamiana <- c("krowa", "krolik")
  } else if (stado_tmp4["owca"] == 0 && stado_tmp4["krowa"] > 1 ) {
    ruch_zamiana <- c("krowa", "owca")
  } else if (stado_tmp4["swinia"] == 0 && stado_tmp4["krowa"] > 1 ) {
    ruch_zamiana <- c("krowa", "swinia")
  } else if (stado_tmp4["krolik"] == 0 && stado_tmp4["swinia"] > 1 ) {
    ruch_zamiana <- c("swinia", "krolik")
  } else if (stado_tmp4["owca"] == 0 && stado_tmp4["swinia"] > 1 ) {
    ruch_zamiana <- c("swinia", "owca")
  } else if (stado_tmp4["krolik"] == 0 && stado_tmp4["owca"] > 1 ) {
    ruch_zamiana <- c("owca", "krolik")
  } else if (stado_tmp4["owca"] == 0 && stado_tmp4["krolik"] > 6 ) {
    ruch_zamiana <- c("krolik", "owca")
  } else if (stado_tmp4["swinia"] == 0 ) {
    ruch_zamiana <- c("swinia", 50)
  } else if (stado_tmp4["krowa"] == 0 ) {
    ruch_zamiana <- c("krowa", 50)
  }
  return (ruch_zamiana)
}
