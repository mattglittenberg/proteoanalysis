#' Variance-Stabilizing Normalization
#'
#' @param log_matrix Log-Transformed Matrix
#'
#' @return VSN Transformed and Normalized Matrix
#' @export
#'
#' @examples
vsn_norm = function(log_matrix) {
  orig_matrix = 2 ** log_matrix

  norm_matrix = justvsn(orig_matrix)

  return(norm_matrix)
}
