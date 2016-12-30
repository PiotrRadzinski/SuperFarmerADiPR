#' Strategia gdy mamy konia i malo innych zwierzat
#'
#' Funkcje uruchamiamy gdy mamy jednego konia i zbyt malo zwierzat aby wygrac. Funkcja opracowuje najlepsza dla nas zamiane
#'
#' @param stado_tmp5 Przechowuje wartosc naszego stada
#'
#' @return ruch_zamiana zwraca jakie zwierze ma byc wymienione na jakie

jeden_kon_mniej_niz_127 <- function( stado_tmp5 ){
  ruch_zamiana <- c(0,0)
  if (stado_tmp5["maly_pies"] == 0 && stado_tmp5["krolik"] > 9 ) {
    ruch_zamiana <- c("krolik", "maly_pies")
  }else if (stado_tmp5["krolik"] == 0 && stado_tmp5["maly_pies"] > 0 ) {
    ruch_zamiana <- c("maly_pies", "krolik")
  } else if (stado_tmp5["owca"] == 0 && stado_tmp5["krolik"] > 15 ) {
    ruch_zamiana <- c("krolik", "owca")
  } else if (stado_tmp5["owca"] > 0 && stado_tmp5["krolik"] > 20 && stado_tmp5["swinia"] == 0 ) {
    ruch_zamiana <- c("krolik", "swinia")
  }
  return (ruch_zamiana)
}
