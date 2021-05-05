#' Global Robust Linear Model based Normalization
#'
#' @param log_matrix Log-Transformed Matrix
#'
#' @return Log-Transformed and Normalized Matrix
#' @export
#'
#' @examples
global_rlr_norm = function(log_matrix) {

  row_med = matrixStats::rowMedians(log_matrix)

  calc_rlm_col = function(col, row_med, log_matrix) {
    rlm_fit = MASS::rlm(as.matrix(log_matrix[, col]) ~ row_med,
                        na.action = stats::na.exclude)
    coeffs = rlm_fit$coefficients
    coef_int = coeffs[1]
    coef_slope = coeffs[2]
    glob_fit_rlr_col = (log_matrix[, col] - coef_int) / coef_slope

    glob_fit_rlr_col
  }

  norm_matrix = vapply(seq_len(ncol(log_matrix)),
                       calc_rlm_col,
                       rep(0, nrow(log_matrix)),
                       row_med = row_med,
                       log_matrix = log_matrix)

  colnames(norm_matrix) = colnames(log_matrix)
  rownames(norm_matrix) = rownames(log_matrix)
  return(norm_matrix)
}
