#' aes_som
#'
#' aesthetics data of som object
#
#' @param model_som object of self-organising maps (SOMs) package
#'
#' @param class Class of dataset
#'
#' @param ts Boolean type to generate plot of timeseries
#'
#' @param ... cutree_value division number of clusters
#'
#' @import tidyverse
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @importFrom stats cutree dist hclust
#'
#' @include utils.R
#'
#' @import kohonen
#'
#' @return Model data that is used in plots
#'
#' @export
aes_som <- function(model_som, class, ts=FALSE, ...) {
  params <- list(...)

  # Check if the variable is a SOMs object
  `%|K|%`(model_som)

  # Creating a data frame from the SOMs object
  model_som_values <-
    data.frame(model_som$data[[1]],
               unit.class = model_som$unit.classif,
               class = class,
               id = (1:nrow(model_som$data[[1]])))



  # Creating a data frame from the SOMs object of grid points
  model_som_pts <- as.data.frame(model_som$grid$pts)
  model_som_pts$unit.class <- c(1:nrow(model_som$grid$pts))
  model_som_pts <-
    dplyr::left_join(model_som_pts, sum_unit_class(model_som_values),
                     by = "unit.class")

  # Set 0 to NA values
  model_som_pts$sum <- tidyr::replace_na(model_som_pts$sum, 0)

  model_result <-
    dplyr::left_join(model_som_values, model_som_pts, by = "unit.class")

  return(model_result)

}

#' sum_unit_class
#'
#' Sum of each grid the neuron
#'
#' @import tidyverse
#'
#' @param model_som_values Model of values that is generate in generate.model.result
#'
#' @return Sum of amount neurons per grid
sum_unit_class <- function(model_som_values) {
  # Receive the unique values of the sorted units
  value_result <-
    data.frame(unit.class = c(unique(model_som_values$unit.class)))

  # In each unique unit sapply the function that sum the numbers of row
  value_result$sum <-
    sapply(value_result$unit.class,
           (
             function(value)
               dplyr::filter(model_som_values,
                             unit.class == as.character(value)) %>% nrow()
           ))

  return(value_result)
}
