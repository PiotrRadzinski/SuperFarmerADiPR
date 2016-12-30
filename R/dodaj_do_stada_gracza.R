#' Dodawanie zwierzat do stada gracza
#'
#' Dodaje zwierzeta wybranego rodzaju do stada gracza,nie przekraczajac ustalonego limitu zwierzat wystepujacych w grze
#'
#' @param nr_zwierzecia Indeks zwierzecia, ktore chcemy dodac do stada gracza
#' @param liczba Liczba zwierzat danego gatunku, ktore chcemy dodac do stada gracza
#' @param stado_tmpA Tymczasowy wektor stada gracza
#'
#' @return Zwraca wektor stada po zmianie


dodaj_do_stada_gracza <- function(nr_zwierzecia, liczba, stado_tmpA){

  ilosc_zwierzat_w_grze <- c(60, 24, 20, 12, 6, 4, 2)
  names(ilosc_zwierzat_w_grze) <- c( "krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")

  liczba_dodanych <- min(liczba, ilosc_zwierzat_w_grze[nr_zwierzecia] - stado_tmpA[nr_zwierzecia])

  stado_tmpA[nr_zwierzecia] = stado_tmpA[nr_zwierzecia] + liczba_dodanych

  return(stado_tmpA)
}
