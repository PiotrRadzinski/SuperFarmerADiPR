% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dodaj_do_stada_gracza.R
\name{dodaj_do_stada_gracza}
\alias{dodaj_do_stada_gracza}
\title{Dodawanie zwierzat do stada gracza}
\usage{
dodaj_do_stada_gracza(nr_zwierzecia, liczba, stado_tmpA)
}
\arguments{
\item{nr_zwierzecia}{Indeks zwierzecia, ktore chcemy dodac do stada gracza}

\item{liczba}{Liczba zwierzat danego gatunku, ktore chcemy dodac do stada gracza}

\item{stado_tmpA}{Tymczasowy wektor stada gracza}
}
\value{
Zwraca wektor stada po zmianie
}
\description{
Dodaje zwierzeta wybranego rodzaju do stada gracza,nie przekraczajac ustalonego limitu zwierzat wystepujacych w grze
}

