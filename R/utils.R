"%|K|%" <- function(a) {
  if (!is.kohonen(a))
    stop(deparse(substitute(a)), " needs to be ", paste0("kohonen object.",
                                                         collapse = ", "), call. = FALSE) else TRUE
}

#' Check if the object is inherits of kohonen
#'
#' @param x Dataframe
#'
#' This is used by aes_som to simplify comparison between objects
#'
#' @return if object inherits kohonen class return TRUE otherwise stop
is.kohonen <- function(x) inherits(x, "kohonen")

"%|C|%" <- function(x) {
  if (!is.cluster(x))
    stop("To view plot cluster method need to set number of clusters.",
         call. = FALSE)
}

#' Check if the parameter has a cluster column
#'
#' @param x Dataframe
#'
#' @return TRUE if cluster in data frame otherwise FALSE
is.cluster <- function(x) "cluster" %in% colnames(x)


"%|SCALE|%" <- function(color = FALSE) {
  if (color) {
    scale_fill_gradient(low = "#3498db", high = "#c0392b")
  }
}


"%|CUTREE|%" <- function(list_params) {
  if("cutree_value" %in% names(list_params)){
    if(list_params$cutree_value < 0){
      stop(
        deparse(substitute(a)),
        " Cluster value must be ",
        paste0("between 1 and 24.",
               collapse = ", "),
        call. = FALSE
      )
    }
    return(TRUE)
  }
  return(FALSE)
}

"%|CLASS|%" <- function(list_params) {
  ifelse("class" %in% names(list_params), TRUE, FALSE)
}


#' aes_color
#'
#' Function that return the base plot with or without color
#'
#' @param aes_som Function aes_som for input data on plot
#'
#' @param color Boolean type to adding color on plot
#'
#' @return Base plot
aes_color <- function(color, aes_som) {
  ifelse(color, return(ggplot(aes_som, aes(var, values, group = id,
                                           colour = class))), return(ggplot(aes_som, aes(var,
                                                                                         values, group = id))))
}

"%|CLUSTER|%" <- function(model_som) {
  ifelse("cluster" %in% names(model_som), TRUE, FALSE)

}

"%|TEXT|%" <- function(a = TRUE) {
  if (a) {
    t <- geom_text(aes(y = y, x = x, label = sum),
                   x = 2.5, y = 3)
    return(t)
  }
}

better_x <- function(number_attributes){
  return(mean(number_attributes))
}
