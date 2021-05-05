#' Quantile based Normalization
#'
#' @param log_matrix Log-Transformed Matrix
#'
#' @return Log-Transformed and Normalized Matrix
#' @export
#'
#' @examples
quant_norm = function(log_matrix) {
  rank_matrix = apply(log_matrix, 2, rank)
  sort_matrix = apply(rank_matrix, 2, sort)
  sorted_mean = matrixStats::rowMeans2(sort_matrix)

  mean_sub <- function(idx, mean_vec){
    return(mean_vec[idx])
  }

  norm_matrix = apply(rank_matrix, mean_sub, sorted_mean)
  return(norm_matrix)
}
