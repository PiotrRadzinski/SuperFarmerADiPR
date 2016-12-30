#' Wymiana malych zwierzat na duze
#'
#' Funkcja otrzymujac informacje na jakie duze zwierze chcemy wymienic male zwierzaki dokonuje odpowiedniej wymiany
#'
#' @param zwierz przechowuje zwierze (konia, swinie lub krowe) na ktore bedziemy chcieli wymienic wiele malych zwierzat
#' @param stado_tmp2 Przechowuje wartosc naszego stada
#' @param wartosc_w_krolikach_tmp2 Przechowuje ile kazdy garunek jest warty w krolikach
#'
#' @return Zwraca wektor stada po wymianie

zamien_na_duze <- function(zwierz, stado_tmp2, wartosc_w_krolikach_tmp2){

  if(zwierz == "kon"){
    kolejnosc_zamiany <- c("duzy_pies", "krowa", "swinia", "owca", "maly_pies", "krolik")
  }
  if(zwierz == "swinia"){
    kolejnosc_zamiany <- c("owca", "maly_pies", "krolik")
  }
  if(zwierz == "krowa"){
    kolejnosc_zamiany <- c("duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  }
  do_oddania <- - wartosc_w_krolikach_tmp2[zwierz]
  stado_tmp2[zwierz] <- stado_tmp2[zwierz] + 1
#Kolejnosc zwierzat ma znaczenie! Przechodze od najwiekszych wartosci, tak aby wartosc kolejnych #zwierzat dzielila wartosc poprzednich. W ten sposob nie oddam za duzo zwierzat
  for(j in kolejnosc_zamiany){
    while(do_oddania < 0 && stado_tmp2[j] > 0){
      stado_tmp2[j] <- stado_tmp2[j] - 1
      do_oddania <- do_oddania + wartosc_w_krolikach_tmp2[j]
    }
  }
  return(stado_tmp2)
}
