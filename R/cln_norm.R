##WRITE OWN FUNCTION##
cln_norm = function(log_matrix) {
  norm_matrix = normalizeCyclicLoess(log_matrix)

  return(norm_matrix)
}
