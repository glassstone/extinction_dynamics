set.seed(1)
par(mar = c(5,4,1,1))
stripchart(
  x = list(
  	freq_VAlow, 
  	freq_VAmed, 
  	freq_VAhigh, 
  	freq_VAhilo, 
  	freq_leftleft, 
  	freq_unifunif
  	), 
  vertical = TRUE,
  method = "jitter",
  jitter = 0.2,
  pch = 1, col = "black", #bg = "grey",
  cex = 0.8,
  # las = 2,
  ylab = "Cascade frequency",
  ylim = c(0,1),
  xaxt = "n"
  # group.names = c("within tags,\nsingle PCR", "between tags,\nsingle PCR", "within tags,\ndouble PCR", "between tags,\ndouble PCR")
)


axis(
  side = 1,
  at = 1:4,
  tick = FALSE,
  line = 1,
  labels = rep(c("within primer\nindex", "among primer\nindex"), 2)
    # "within tags,\nsingle PCR", "between tags,\nsingle PCR", "within tags,\ndouble PCR", "between tags,\ndouble PCR"
    # paste("within tags,\namong libraries\nN = ", length(BC_within_tag), sep = ""),
    # paste("among tags,\namong libraries\nN = ", length(BC_between_tag_mean), sep = "")
  # )
)



# additional stuff
abline(v = 2.5, lty = 2)
legend("topleft", legend = "single PCR", bty = "n")
# legend(x = 1, y = 1, legend = "single PCR", bty = "n")
legend("topright", legend = "double PCR", bty = "n")
# legend(x = 3, y = 1, legend = "double PCR", bty = "n")
box(lwd = 2)
# dev.off()
