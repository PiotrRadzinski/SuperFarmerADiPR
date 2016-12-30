#' Strategia gry superfarmer
#'
#' Funkcja wykonujaca cala strategie gry superfarmer
#'
#' @param stado_tmp9 Przechowuje wartosc naszego stada.
#'
#' @return Zwraca stado po wymianach.
#'
#' @export

strategia_postMDiPR <- function( stado_tmp9 ){
  wartosc_w_krolikach <- c(1, 6, 12, 36, 72, 6, 36)
  nazwy_zwierzat <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  names(wartosc_w_krolikach) <- nazwy_zwierzat
  names(stado_tmp9) <- nazwy_zwierzat

  if (stado_tmp9["kon"] == 0 || stado_tmp9["kon"] == 1) {
    stado_tmp9 <- brak_lub_1_kon( stado_tmp9, wartosc_w_krolikach )
  }else if (stado_tmp9["kon"] > 1 ){
    stado_tmp9 <- wiecej_niz_1_kon( stado_tmp9 )
  }
  return(stado_tmp9)
}
