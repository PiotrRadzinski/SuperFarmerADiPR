#' Strategia dla wiecej nie jednego konia
#'
#' Funkcja dokonuje wymiany jednego z koni w taki sposob, ze wygrywamy gre
#'
#' @param stado_tmp8 Przechowuje wartosc naszego stada
#'
#' @return zwraca stado po wymianach

wiecej_niz_1_kon <- function( stado_tmp8 ){
  stado_tmp8["kon"] <- stado_tmp8["kon"] - 1
  stado_tmp8["krowa"] <- stado_tmp8["krowa"] + 1
  stado_tmp8["swinia"] <- stado_tmp8["swinia"] + 2
  stado_tmp8["owca"] <- stado_tmp8["owca"] + 1
  if (stado_tmp8["krolik"] < 54){
    stado_tmp8["krolik"] <- stado_tmp8["krolik"] + 6
  } else {
    stado_tmp8["owca"] <- stado_tmp8["owca"] + 1
  }

  return (stado_tmp8)
}
