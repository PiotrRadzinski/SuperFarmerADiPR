#' Kontrola wilka i lisa
#'
#' Funkcja po otrzymaniu wyniku rzutu i aktualnego stada oblicza stan stada w wypadku gdy jego czesc przepada.
#'
#' @param stado_tmp11 Przechowuje aktualne stado na potrzeby funkcji.
#' @param kostki Przechowuje wynik rzutu kostka
#'
#' @return Zwraca uaktualnione stado.

wilk_i_lis <- function(stado_tmp11, kostki) {
  if (kostki["wilk"] == 1) {
    if (stado_tmp11["duzy_pies"] != 0) {
      stado_tmp11["duzy_pies"] <- stado_tmp11["duzy_pies"] - 1
    } else {
      stado_tmp11["krolik"] <- 0
      stado_tmp11["owca"] <- 0
      stado_tmp11["swinia"] <- 0
      stado_tmp11["krowa"] <- 0
    }
  }
  if (kostki["lis"] == 1) {
    if (stado_tmp11["maly_pies"] > 0) {
      stado_tmp11["maly_pies"] <- stado_tmp11["maly_pies"] - 1
    } else {
      stado_tmp11["krolik"] <- 0
    }
  }
  return(stado_tmp11)
}
