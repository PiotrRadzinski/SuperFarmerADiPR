#' Wymiana zwierzaka na zwierzaka
#'
#' Funkcja otrzymujac informacje jakie zwierze ma wymienic na jakie dokonuje tej wymiany
#'
#' @param oddajemy Przechowuje zwierze ktore chcemy wymienic
#' @param dostajemy Przechowuje zwierze ktore chcemy otrzymac
#' @param stado_tmp1 Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp1 Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return Zwraca wektor stada po wymianie

zamien_zwierze_na_zwierze <- function(oddajemy, dostajemy, stado_tmp1, wartosc_w_krolikach_tmp1)
{
  ilosc <- 0
  if( wartosc_w_krolikach_tmp1[oddajemy] >= wartosc_w_krolikach_tmp1[dostajemy] ){
    ilosc <- wartosc_w_krolikach_tmp1[oddajemy]/wartosc_w_krolikach_tmp1[dostajemy]
    stado_tmp1[oddajemy] <- stado_tmp1[oddajemy] -  1
    stado_tmp1[dostajemy] <- stado_tmp1[dostajemy] + ilosc
  }else{
    ilosc <- wartosc_w_krolikach_tmp1[dostajemy]/wartosc_w_krolikach_tmp1[oddajemy]
    stado_tmp1[oddajemy] <- stado_tmp1[oddajemy] -  ilosc
    stado_tmp1[dostajemy] <- stado_tmp1[dostajemy] + 1
  }
  return(stado_tmp1)
}
