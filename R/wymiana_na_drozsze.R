#' Wymiana zwierzat na drozsze
#'
#' Kupuje najdrozsze zwierze, na ktore stac gracza
#'
#' @param stado_tmpD Tymczasowy wektor stada
#' @param ceny Wektor cen zwierzat
#'
#' @return Zwraca wektor stada po wymianie

wymiana_na_drozsze <- function(stado_tmpD, ceny){
  for (i in c(5, 7, 4:2)){

    if (stado_tmpD[i] == 0){         #sprawdzamy, czy mamy i-te zwierze
      if (czy_stac_nas(nr_zwierzecia = i, stado_tmpD, ceny)){

        stado_tmpD <- dodaj_do_stada_gracza(nr_zwierzecia = i, liczba = 1, stado_tmpD)
        do_splaty <- ceny[i]

        j <- (i - 1)
        while (do_splaty > 0){
          if (stado_tmpD[j] >= 1){   #sprzedajemy zwierzeta kolejno od najdrozszych, te ktorych mamy wiecej niz jedno

            do_splaty <- (do_splaty - ceny[j]) #splacamy wartosc (po jednym zwierzeciu)
            stado_tmpD <- oddaj_zwierze(nr_zwierzecia = j, liczba = 1, stado_tmpD)
          } else j <- (j - 1)
        }
        break
      }
    }
  }
  return(stado_tmpD)
}
