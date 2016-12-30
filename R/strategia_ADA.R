#' Strategia gry superfarmer
#'
#' Funkcja realizuje strategie gracza
#'
#' @param stado_tmp Tymczasowy wektor stada gracza
#' @param ceny Przechowuje wartosci zwierzat przeliczone na kroliki
#'
#' @return Zwraca stado po wymianach.
#'
#' @export

strategia_ADA <- function (stado_tmp, ceny = c("krolik" = 1, "owca" = 6, "swinia" = 12, "krowa" = 36, "kon" = 72, "maly_pies" = 6, "duzy_pies" = 36) ){


  stado_tmp <- wymiana_na_tansze(stado_tmp)
  if (czy_wygrana(stado_tmp)) return(stado_tmp)

  stado_tmp <- wymiana_na_drozsze(stado_tmp, ceny)

  return(stado_tmp)
}
