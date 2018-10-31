#' nnet_treino
#'
#' Treino da rede neural MLP
#'
#' @author Amita
#'
#' @param redshit_ggsom data.frame tratado pela função ggsom
#'
#' @param path Caminho para a escrita do arquivo
#'
#' @param it número de iterações
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
nnet_treino <- function(redshift_ggsom, path, it) {
  Sigmas <-
    data.frame(c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0))

  names(Sigmas) <-
    c("unit.class",
      "x",
      "y",
      "neurons",
      "sigma1",
      "sigma2",
      "sigma3",
      "sigma4",
      "sigma5")

  for (grid in unique(redshift_ggsom$unit.class)) {
    # Seleciona os valores do grid X
    new_ggsom <- seleciona_grid(redshift_ggsom, grid)

    # Seleciona os valores do grid
    grade_neuronio <- new_ggsom %>%
      select(unit.class, x, y) %>%
      filter(unit.class == grid) %>%
      unique()

    redshifts <- new_ggsom %>% select(u, g, r, i, z,
                                      redshift)

    # Separa o conjunto de treino
    npoints_train = npoints_train = as.integer(dim(redshifts)[1] * 2 / 3)

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
            maxit = it,
            trace = TRUE,
            linout = TRUE
          )
        nnet.predict <- predict(nnet.fit, redshifts[-samp, ])
        redshiftsV <- redshifts[-samp, ]
        #MSE <- mean((nnet.predict - redshiftsV$redshift)^2)

        Sigma[i] <-
          1.48 * median(abs((nnet.predict - redshiftsV$redshift) - median(nnet.predict -
                                                                            redshiftsV$redshift)
          ) / (1 + redshiftsV$redshift))

        redshiftsV$predicted <- nnet.predict
        # Remove data frame column after plotting!
        redshiftsV$predicted <- NULL
      }
      newrow <- c(
        grade_neuronio$unit.class,
        grade_neuronio$x,
        grade_neuronio$y,
        neurons,
        Sigma
      )

      Sigmas <- rbind(Sigmas, newrow)
    }
  }
  write.csv(Sigmas,
            file = paste(path,
                         "Sigmas_nnet_specz0a1_som_",
                         max(redshift_ggsom$x),"_",
                         max(redshift_ggsom$y),"_",
                         it,
                         "_it_inpe",
                         ".csv",
                         sep = ""))
}

#' treino
#'
#' Função necessário para treinar o conjunto
#'
#' @param lista recebe um tipo list contendo os valores agrupamentos SOMs
#'
#' @param path Caminho para a escrita do arquivo
#'
#' @param it número de iterações
#'
#' @export
treino <- function(lista, class, path, it) {
  for (i in lista) {

    redshift_ggsom <- cria_ggsom(i, class)

    nnet_treino(redshift_ggsom, path, it)
  }
}

#' cria_ggsom
#'
#' Cria um tipo SOMs
#'
#' @param redshift_som Objeto SOMs
#'
#' @param class Classe categórica ou numérica
#'
#' @include aes_som.R
#'
#' @export
cria_ggsom <- function(redshift_som, class) {
  redshift_ggsom <- aes_som(redshift_som,
                            class = class)
  return(redshift_ggsom)
}

#' seleciona_grid
#'
#' Seleciona todos atributos e filtra por um determinado grid
#'
#' @import tidyverse
#'
#' @return Conjunto de dados filtrado por X neurônio
seleciona_grid <- function(conjunto_dados, posicao_grid) {
  novo_conjunto <- conjunto_dados %>%
    select(everything()) %>%
    filter(unit.class == posicao_grid)

  return(novo_conjunto)
}
