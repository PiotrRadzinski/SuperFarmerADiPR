#' Wykonanie strategii gdy mamy mniej niz 2 konie
#'
#' Funkcja uzywajac innych funkcji najpierw opracowuje optymalna strategie a nastepnie wykonuje ja zwracajac nam stado po wymianie
#'
#' @param stado_tmp7 Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp7 Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return zwraca stado po wymianach

brak_lub_1_kon <- function( stado_tmp7 , wartosc_w_krolikach_tmp7 ){
  if(stado_tmp7["kon"] == 0){
    ruch_zmiana <- brak_konia( stado_tmp7, wartosc_w_krolikach_tmp7 )
  } else{
    ruch_zmiana <- jeden_kon( stado_tmp7, wartosc_w_krolikach_tmp7 )
  }
#Jezeli ruch_zmiana[1] > 0 to wiemy, że jakiś ruch sie odbyl (jak 0, to nie bylo zadnego ruchu).
  if(ruch_zmiana[1] > 0){
    if(ruch_zmiana[2] == 50){
      stado_tmp7 <- zamien_na_duze(ruch_zmiana[1], stado_tmp7, wartosc_w_krolikach_tmp7 )
    } else {
      stado_tmp7 <- zamien_zwierze_na_zwierze(ruch_zmiana[1], ruch_zmiana[2], stado_tmp7, wartosc_w_krolikach_tmp7)
    }
  }
  return (stado_tmp7)
}
