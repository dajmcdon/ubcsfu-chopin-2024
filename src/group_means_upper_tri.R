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

othercut = .35
subs = apply(hc_parm, 1, quantile, probs = 4/46) < othercut
sDmat = hc_parm[subs,subs]
sdends = sDmat %>% as.dist %>% hclust %>% as.dendrogram
nclusts = 4
colorthem = TRUE
rind1 <- order.dendrogram(dend_parm)
mat <- hc_parm[rind1, rind1]
main_clusts <- cutree(as.hclust(sdends), k = nclusts)
pos <- match(names(main_clusts), rownames(mat))
all_clusts <- rep(5L, nrow(hc_parm))
all_clusts[pos] <- main_clusts
rind <- sort.int(all_clusts, index.return = TRUE)
mat2 <- mat[rind$ix, rind$ix]

m <- group_means_upper_tri(sqrt(mat2), rind$x)
rownames(m) <- NULL
colnames(m) <- NULL
m <- reshape2::melt(t(m[46:1,]), na.rm = TRUE)
sply <- 46 - cumsum(rle(rind$x)$lengths)[1:4] + .5
splx <- cumsum(rle(rind$x)$lengths)[1:4] + .5
