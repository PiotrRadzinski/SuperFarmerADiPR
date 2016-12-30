#' Rzut kostkami
#'
#' Funkcja wykonuje losowanie kostkami i zwraca wektor z wynikiem rzutu.
#'
#' @return Zwracany jest wynik losowania w postaci wektora z wyrzuconymi zwierzetami.

rzut_kostkami <- function() {
  #Losowanie kostka.
  kostka_wilk <- sample(1:9, 1, TRUE, prob = c(1/2, 1/4, 1/12, 1/12, 0, 0, 0, 0, 1/12))
  kostka_lis <- sample(1:9, 1, TRUE, prob = c(1/2, 1/6, 1/6, 0, 1/12, 0, 0, 1 / 12, 0))

  #Zmienne przechowujace ilosc poszczegolnych zwierzat na kostce.
  kostki <- rep(0, times = 9)
  names(kostki) <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies", "lis", "wilk")

  kostki[kostka_wilk] <- kostki[kostka_wilk] + 1
  kostki[kostka_lis] <- kostki[kostka_lis] + 1
  return(kostki)
}
