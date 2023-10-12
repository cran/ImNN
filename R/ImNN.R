#'@title Neural Networks for Predicting Volume of Forest Trees
#' @param data Datasets
#' @param hidden_neurons_range Number of hidden neurons in neural network's two layers (layer 1 and layer 2)
#' @import stats  MLmetrics ggplot2 neuralnet
#' @return
#' \itemize{
#'   \item results: Results
#' }
#' @export
#'
#' @examples
#' \donttest{
#' library("ImNN")
#' data <- system.file("extdata", "data_test.csv", package = "ImNN")
#' data_test <- read.csv(data)
#' Model<-ImNN(data =data_test,hidden_neurons_range=2)
#' }
#' @references
#' \itemize{
#'\item Jeelani, M.I., Tabassum, A., Rather, K and Gul,M.2023. Neural Network Modeling of Height Diameter Relationships for Himalayan Pine through Back Propagation Approach. Journal of The Indian Society of Agricultural Statistics. 76(3): 169–178
#'\item Tabassum, A., Jeelani, M.I., Sharma,M., Rather, K R ., Rashid, I and Gul,M.2022.  Predictive Modelling of Height and Diameter Relationships of Himalayan Chir Pine . Agricultural Science Digest - A Research Journal. DOI:10.18805/ag.D-5555
#' }


ImNN <- function(data, hidden_neurons_range) {
  # Data normalization
  R2<-NULL
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

  maxmindf <- as.data.frame(lapply(data, normalize))

  results <- data.frame(
    Hidden_Neurons_Layer1 = integer(0),
    Hidden_Neurons_Layer2 = integer(0),
    R2 = numeric(0),
    RMSE = numeric(0),
    MAE = numeric(0),
    PER = numeric(0)
  )

  for (n1 in hidden_neurons_range) {
    for (n2 in hidden_neurons_range) {
      nn <- neuralnet(Volume ~ ., data = maxmindf,
                      hidden = c(n1, n2),
                      rep = 2,
                      act.fct = "logistic",
                      err.fct = "sse",
                      linear.output = FALSE,
                      lifesign = "minimal",
                      stepmax = 1000000,
                      threshold = 0.001)

      # Plot the neural network architecture
      plot(nn)

      predictions <- predict(nn, maxmindf)

      evaluation <- data.frame(
        Hidden_Neurons_Layer1 = n1,
        Hidden_Neurons_Layer2 = n2,
        R2 = R2_Score(predictions, maxmindf$Volume),
        RMSE = RMSE(predictions, maxmindf$Volume),
        MAE = MAE(predictions, maxmindf$Volume),
        PER = RMSE(predictions, maxmindf$Volume / mean(maxmindf$Volume))
      )

      results <- rbind(results, evaluation)
    }
  }

  return(results)
}
