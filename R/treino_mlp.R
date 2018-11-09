#' treino_mlp
#'
#' Treino da rede neural MLP
#'
#' @author Amita
#'
#' @param redshift data.frame do conj redshift
#'
#' @import tidyverse
#'
#' @import nnet
#'
#' @import kohonen
#'
#' @return Predição dos Sigmas
#'
#' @export
#'
treino_mlp <- function(redshifts){

  Sigmas <-
    data.frame(c(0), c(0), c(0), c(0), c(0), c(0))

  npoints_train <- as.integer(dim(redshifts)[1] * 2 / 3)

  names(Sigmas) <-
    c("neurons",
      "sigma1",
      "sigma2",
      "sigma3",
      "sigma4",
      "sigma5")

  for (neurons in c(5, seq(10, 100, 20))) {
    Sigma <- c()

    for (i in seq(1, 5)) {
      #redshifts <- sample(redshifts)
      samp <- sample(npoints_train)
      nnet.fit <-
        nnet(
          redshift ~ .,
          data = redshifts,
          size = neurons,
          subset = samp,
          maxit = 100,
          trace = TRUE,
          linout = TRUE
        )
      nnet.predict <- predict(nnet.fit, redshifts[-samp, ])
      redshiftsV <- redshifts[-samp, ]
      #MSE <- mean((nnet.predict - redshiftsV$redshift)^2)

      Sigma[i] <-
        1.48 * median(abs((nnet.predict - redshiftsV$redshift) - median(nnet.predict -
                                                                          redshiftsV$redshift))/(1 + redshiftsV$redshift))

      redshiftsV$predicted <- nnet.predict
      # Remove data frame column after plotting!
      redshiftsV$predicted <- NULL
    }
    newrow <- c(neurons, Sigma)

    Sigmas <- rbind(Sigmas, newrow)
  }
  write.csv(Sigmas,
            file = "./inst/extdata/Sigmas_nnet_specz0a1_nnet_result_100_it.csv")
}
