#' Wymiana zwierzaka na mniejszego
#'
#' Funkcja rozmienia na mniejsze największe zwierze, spośród tych, których gracz ma wiecej niz jedno
#'
#' @param stado_tmpC Tymczasowy wektor stada gracza
#'
#' @return Zwraca wektor stada po wymianie

wymiana_na_tansze <- function(stado_tmpC){

  for (i in c(5,4,7,3,2,6)) {   #przechodzimy kolejno od konia do malego psa zgodnie z malejaca wartoscia zwierzat
    if (stado_tmpC[i] > 1){   #sprawdzamy, czy mamy wiecej niz jedno zwierze danego gatunku

      k <- i
      if(i == 7) k <- 4 #duzy pies jest warty tyle co krowa
      if(i == 6) k <- 2 #maly pies jest warty tyle co owca

      if (prod(stado_tmpC[k:5]) > 0){   #jezeli mamy wszystkie drozsze zwierzeta

        stado_tmpC <- oddaj_zwierze(nr_zwierzecia = i, liczba = 1, stado_tmpC)

        for (j in (i-1):1){
          stado_tmpC <- dodaj_do_stada_gracza(nr_zwierzecia = j, liczba = 1, stado_tmpC)
        }
        break
      }
    }
  }

  return(stado_tmpC)
}
