#' Mean Intensity based Normalization
#'
#' @param log_matrix Log-Transformed Matrix
#'
#' @return Log-Transformed and Normalized Matrix
#' @export
#'
#' @examples
mean_norm = function(log_matrix) {
  orig_matrix = 2 ** log_matrix

  col_mean = matrixStats::colMeans2(orig_matrix, na.rm = TRUE)
  mean_col_mean = mean(col_mean, na.rm = TRUE)

  norm_matrix = matrix(nrow = nrow(orig_matrix),
                       ncol = ncol(orig_matrix),
                       byrow = TRUE)

  for (col in seq(ncol(orig_matrix))) {
    for (row in seq(nrow(orig_matrix))) {
      norm_matrix[row, col] =
        (orig_matrix[row, col] / col_mean[col]) * mean_col_mean
    }
  }

  norm_matrix = log2(norm_matrix)
  colnames(norm_matrix) = colnames(orig_matrix)
  rownames(norm_matrix) = rownames(orig_matrix)
  return(norm_matrix)
}
