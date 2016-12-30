test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10,1,1,1,0,1,1)),c(10,1,1,0,1,1,0))
})

test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10,0,0,0,0,0,0)),c(4,0,0,0,0,1,0))
})

test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10,0,1,0,0,1,0)),c(22,0,0,0,0,1,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10,1,0,0,0,1,0)),c(16,0,0,0,0,1,0))
})



test_that("Lis zjada kroliki",{
  stado <- c("krolik" = 5, "owca" = 4, "swinia" = 3, "krowa" = 2, "kon" = 1, "maly_pies" = 0, "duzy_pies" = 1)

  kostki_tmp <- rep(0, times = 9)
  names(kostki_tmp) <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies", "lis", "wilk")
  kostki_tmp["lis"] <- 1

  stado <- wilk_i_lis(stado, kostki_tmp)

  expect_equivalent(stado, c(0, 4, 3, 2, 1, 0, 1))
})

test_that("Lis ucieka przed malym psem",{
  stado <- c("krolik" = 5, "owca" = 4, "swinia" = 3, "krowa" = 2, "kon" = 1, "maly_pies" = 1, "duzy_pies" = 1)

  kostki <- rep(0, times = 9)
  names(kostki) <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies", "lis", "wilk")
  kostki["lis"] <- 1

  stado <- wilk_i_lis(stado, kostki)

  expect_equivalent(stado, c(5 , 4, 3, 2, 1, 0, 1))
})



test_that("Wilk zjada zwierzaki",{
  stado <- c("krolik" = 5, "owca" = 4, "swinia" = 3, "krowa" = 2, "kon" = 1, "maly_pies" = 1, "duzy_pies" = 0)

  kostki <- rep(0, times = 9)
  names(kostki) <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies", "lis", "wilk")
  kostki["wilk"] <- 1

  stado <- wilk_i_lis(stado, kostki)

  expect_equivalent(stado, c(0 , 0, 0, 0, 1, 1, 0))
})

test_that("Wilk ucieka przed duzym psem",{
  stado <- c("krolik" = 5, "owca" = 4, "swinia" = 3, "krowa" = 2, "kon" = 1, "maly_pies" = 1, "duzy_pies" = 1)

  kostki <- rep(0, times = 9)
  names(kostki) <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies", "lis", "wilk")
  kostki["wilk"] <- 1

  stado <- wilk_i_lis(stado, kostki)

  expect_equivalent(stado, c(5 , 4, 3, 2, 1, 1, 0))
})

test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0,0,0,0,0,1,0)),c(6,0,0,0,0,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(40, 1, 0,0,0,2,0)),c(34,2,0,0,0,2,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(47, 0, 0,0,0,2,0)),c(35,0,1,0,0,2,0))
})

#jeden_kon
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(36, 0, 0,1,1,0,0)),c(0,0,0,0,2,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0,3,4,0,1,0,0)),c(12,3,3,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10,0,4,0,1,0,0)),c(10,2,3,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0,10,0,0,1,0,0)),c(6,9,0,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(59,0,0,0,1,0,0)),c(53,1,0,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(26,6,0,0,1,0,0)),c(26,4,1,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(20,6,1,0,1,0,0)),c(20,2,0,1,1,0,0))
})


test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0,2,1,1,1,0,0)),c(36,2,1,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(1,0,2,1,1,0,0)),c(1,6,2,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(1,3,0,1,1,0,0)),c(1,3,3,0,1,0,0))
})

#jeden_kon_mniej_niz_127
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(10, 0, 0,0,1,0,0)),c(4, 0, 0,0,1,1,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0, 0, 0,0,1,1,0)),c(6, 0, 0,0,1,0,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(16, 0, 0,0,1,1,0)),c(10, 1, 0,0,1,1,0))
})
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(21, 1, 0,0,1,1,0)),c(9, 1, 1,0,1,1,0))
})
#wiecej niz jeden kon
test_that("Poprawna wymiana w strategia_postMDiPR",{
  expect_equivalent(strategia_postMDiPR(c(0,0,0,0,2,0,0)),c(6,1,2,1,1,0,0))
})

#inne
test_that("Moze rozmnozyc sie wiele zwierzat naraz",{

  na_kostkach <- c("krolik" = 1, "owca" = 1, "swinia" = 0, "krowa" = 0, "kon" = 0, "maly_pies"= 0, "duzy_pies"= 0, "lis"= 0, "wilk"= 0)

  stado<-c(3,1,0,0,0,1,0)
  stado <- rozmnazanie(stado, na_kostkach)

  expect_equal(stado[1],5)
  expect_equal(stado[2],2)
})
