

ss <- order.dendrogram(sdends)
main_clusts <- main_clusts[ss]
j <- 1
for (i in unique(main_clusts)) {
  main_clusts[main_clusts == i] = letters[j]
  j <- j + 1
}
j <- 1
for (i in unique(main_clusts)) {
  main_clusts[main_clusts == i] = j
  j <- j + 1
}
main_clusts <- as.numeric(main_clusts)

mat2 <- sDmat[ss, ss]
m2 <- group_means_upper_tri(sqrt(mat2), main_clusts)
# rownames(m2) <- NULL
# colnames(m2) <- NULL
m2 <- reshape2::melt(t(m2[nrow(m2):1, ]), na.rm = TRUE)
sply <- length(ss) - cumsum(rle(main_clusts)$lengths)[1:3] + .5
splx <- cumsum(rle(main_clusts)$lengths)[1:3] + .5
ggplot(m2, aes(Var1, Var2, fill = value)) + 
  geom_raster() + 
  scale_fill_viridis_c(direction = -1) +
  #theme_void() +
  coord_equal() +
  xlab("") + ylab("") +
  geom_hline(yintercept = sply, color = "#ffffff", size = .2) +
  geom_vline(xintercept = splx, color = "#ffffff", size = .2) +
  theme(legend.position = "none", plot.margin = margin(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())



if(colorthem) sdends = sdends %>% 
  set('labels_col', value=fivecolors[c(4,3,1,2)], k=nclusts) %>%
  set('branches_lty', 1) %>%
  set('branches_k_color', value=fivecolors[c(4,3,1,2)], k=nclusts)
library(dendextend)
ggplot(as.ggdend(sdends), labels = FALSE)

