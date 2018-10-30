## Pacotes necessários 

```{r}
  devtools::install_github("oldlipe/quickadass")
  install.packages("tidyverse")
  install.packages("kohonen")
  install.packages("nnet")
  install.packages("ggthemes")
```

## Para criar um ggsom

```{r}
# Parte do código da Amita
redshifts <- read.csv("./inst/extdata/redshift_between0and7.csv")

# Corrigindo o nome dos atributos
redshifts <- redshifts[c("u", "g", "r", "i", "z", "redshift")]

# Gerando um SOMs 7x7
redshifts_som_0_7_7_7 <- kohonen::som(as.matrix(redshifts[1:5]),
                                      grid = somgrid(7, 7, "rectangular"))

redshift_ggsom_7_7 <- cria_ggsom(redshifts_som_0_7_7_7,
                                  class=redshift[6])
                                  
# Gerando um SOMs 9x9
redshifts_som_0_7_9_9 <- kohonen::som(as.matrix(redshifts[1:5]),
                                      grid = somgrid(9, 9, "rectangular"))
                                    
```

## Treinamento

```{r}
# Para treinar apenas um SOMs 
nnet_treino(redshift_ggsom_7_7, path = "./inst/extdata/", it = 100)

# Para treinar mais de um SOMs, basta criar uma lista de SOMs
lista_soms <- list(redshifts_som_0_7_7_7,
                    redshifts_som_0_7_9_9)

treino(lista_soms, class = redshifts[6], path  = "./inst/extdata/", it = 1000)
```

## Visualização

```{r}
# Crie um novo som
redshifts_som_0_7_9_9 <- kohonen::som(as.matrix(redshifts[1:5]),
                                      grid = somgrid(9, 9, "rectangular"))
# Transforme para ggsom
redshift_ggsom_9_9 <- cria_ggsom(redshifts_som_0_7_9_9,
                                  class=redshift[6])  
                                  
gridsom_line(redshift_ggsom_9_9)                                  

```


