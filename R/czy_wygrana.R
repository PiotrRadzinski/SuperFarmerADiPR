#' Czy wygrana
#'
#' Konczy gre jezeli gracz wygrywa
#'
#' @param stado_tmp tymczasowy (tmp=temporary) wektor stada
#'
#' @return Zwraca wartosc 1 jezeli spelnione sa warunki wygranej

czy_wygrana <- function(stado_tmp){

    (stado_tmp["kon"] >= 1) &
    (stado_tmp["krowa"] >= 1) &
    (stado_tmp["swinia"] >= 1) &
    (stado_tmp["owca"] >= 1) &
    (stado_tmp["krolik"] >= 1)

}
