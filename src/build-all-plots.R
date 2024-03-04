plots <- vector("list", 4)
lt <- diff(c(tempos$note_onset, 61))
for (i in 1:nrow(pvec_ml)) {
  params <- unlist(pvec_ml[i, ])
  y <- matrix(tempos[, gsub(" ", "_", row.names(pvec_ml)[i])], nrow = 1)
  pmats <- musicModel(
    lt, params[1], params[2:4], c(params[5], 1, 1),
    params[6:12], c(132, 0), c(400, 10)
  )
  beam <- beamSearch(
    pmats$a0, pmats$P0, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    pmats$dt, pmats$ct, pmats$Tt, pmats$Zt,
    pmats$HHt, pmats$GGt, y, pmats$transMat, 400
  )
  bestpath <- beam$paths[which.max(beam$weights), ]
  kal <- kalman(pmats, bestpath, y)
  plots[[i]] <- data.frame(
    measure = tempos$note_onset, tempo = c(y),
    inferred = c(kal$ests), state = convert11to4(bestpath)
  )
}
plots <- bind_rows(plots)
plots$performer <- rep(row.names(pvec_ml), each = length(y))
plots$state <- as.factor(plots$state)
plots <- plots %>% mutate(
  state = factor(state, labels = c("constant", "decel", "accel", "stress"))
)
perfcols <- viridis_pal(begin = .2)(nlevels(plots$state))

perfshapes <- c(20, 17, 18, 8)
plots <- plots %>% mutate(performer, performer = sub("_", " ", performer))
grichter <- ggplot(filter(plots, performer == "Richter 1976")) +
  annotate(
    "rect", xmin = 33, xmax = 45, ymin = -Inf, ymax = Inf,
    fill = "gray90", color = "gray90"
  ) +
  geom_line(aes(x = measure, y = tempo), color = primary) +
  scale_x_continuous(expand = expansion()) +
  geom_point(aes(x = measure, y = inferred, color = state, shape = state), size = 4) +
  scale_color_brewer(palette = "PuOr", direction = -1) +
  scale_shape_manual(values = perfshapes) +
  facet_wrap(~performer) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(hjust = 0)
  )
