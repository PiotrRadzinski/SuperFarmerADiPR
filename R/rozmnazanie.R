#' Rozmnazanie zwierzat
#'
#' Funkcja przelicza stado rozmnazajace sie zgodnie z rzutem.
#'
#' @param stado_tmp12 Przechowuje aktualne stado na potrzeby funkcji.
#' @param kostki Przechowuje wynik rzutu kostkami.
#'
#' @return Aktualny stan stada.

rozmnazanie <- function(stado_tmp12, kostki) {
  limit <- c(60, 24, 20, 12, 6, 4, 2)
  nazwy_zwierzat <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  names(limit) <- nazwy_zwierzat

  for (j in 1:5)
  {
    if (kostki[j] > 0)
      stado_tmp12[j] <- min(stado_tmp12[j] + as.integer((stado_tmp12[j] + kostki[j]) / 2), limit[j])
  }
  return(stado_tmp12)
}
