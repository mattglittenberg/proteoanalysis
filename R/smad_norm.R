#' Scaled Median Absolute Deviation based Normalization
#'
#' @param log_matrix Log-Transformed Matrix
#'
#' @return Log-Transformed and Normalized Matrix
#' @export
#'
#' @examples
smad_norm = function(log_matrix) {
  col_med = matrixStats::colMedians(log_matrix, na.rm = TRUE)
  col_mad = matrixStats::colMads(log_matrix, na.rm = TRUE)

  mad_matrix = t(apply(log_matrix, 1,
                       FUN = function(row) ((row-col_med) / col_mad)))

  norm_matrix = mad_matrix + mean(col_med)

  colnames(norm_matrix) = colnames(log_matrix)
  rownames(norm_matrix) = rownames(log_matrix)
  return(norm_matrix)
}
