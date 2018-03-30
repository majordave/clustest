#' Randomly partition a dataset
#'
#' Randomly generate train and test partitions from a dataset.
#' @param dataset Dataset from which to generate partitions.
#' @param train_size Fraction of data to partition for training.
#' @return Returns a list containing:
#' \describe{
#'   \item{\code{train}}{The training data.frame partition.}
#'   \item{\code{test}}{The testing data.frame partition.}
#' }
#' @examples
#' data  <- load("my_data.RData")
#' res   <- bootstrap(dataset = data, train_size = 0.1)
#' train <- res$train
#' test  <- res$test
#' @importFrom magrittr %>% 
#' @export

partition <- function(dataset, train_size){
  partt <- sample(1:2, size = nrow(dataset), replace = TRUE, prob = c(train_size, 1-train_size))
  
  list(
    train = dataset %>% dplyr::filter(partt == 1),
    test = dataset %>% dplyr::filter(partt == 2)
  )
}