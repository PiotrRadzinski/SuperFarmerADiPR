#' Strategia gry superfarmer
#'
#' Funkcja wykonujaca cala strategie gry superfarmer
#'
#' @param stado_tmp9 Przechowuje wartosc naszego stada.
#' @param ceny Przechowuje wartosci zwierzat przeliczone na kroliki
#'
#' @return Zwraca stado po wymianach.
#'
#' @export

strategia_postMDiPR <- function( stado_tmp9, ceny = c("krolik" = 1, "owca" = 6, "swinia" = 12, "krowa" = 36, "kon" = 72, "maly_pies" = 6, "duzy_pies" = 36) ){

  wartosc_w_krolikach <- ceny

  nazwy_zwierzat <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  names(stado_tmp9) <- nazwy_zwierzat

  if (stado_tmp9["kon"] == 0 || stado_tmp9["kon"] == 1) {
    stado_tmp9 <- brak_lub_1_kon( stado_tmp9, wartosc_w_krolikach )
  }else if (stado_tmp9["kon"] > 1 ){
    stado_tmp9 <- wiecej_niz_1_kon( stado_tmp9 )
  }
  return(stado_tmp9)
}
