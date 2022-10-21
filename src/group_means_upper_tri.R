group_means_upper_tri <- function(mat, gr, diag_vals = NA) {
  diag(mat) <- NA
  lt <- mat
  lt[upper.tri(lt, TRUE)] <- NA
  ngroups <- dplyr::n_distinct(gr)
  gr_means <- matrix(NA, nrow = ngroups, ncol = ngroups)
  for (ii in seq(ngroups)) {
    for (jj in seq(ngroups)) {
      gr_means[ii, jj] <- mean(lt[gr == ii, gr == jj], na.rm = TRUE)
    }
  }
  gr_means[upper.tri(gr_means)] <- 0
  gr_means <- gr_means + t(gr_means)
  diag(gr_means) <- diag(gr_means) / 2
  
  for (ii in seq(nrow(lt))) {
    for (jj in ii:ncol(lt)) {
      lt[ii, jj] <- gr_means[gr[ii], gr[jj]]
    }
  }
  diag(lt) <- diag_vals
  lt
}
