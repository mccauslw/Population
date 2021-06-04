# Plot binary choice probabilitiies associated with different values of alpha

pdf('Figures/bcp.pdf', paper='special', width=12, height=6)
  
pr = seq(0.0, 1.0, by=0.0001)
alpha = c(1, 2, 5, 10, 20)
lty_vals = c(1, 5, 2, 4, 3) # 'solid', 'longdash', 'dashed', 'dotdash', 'dotted')
plot(NULL, xlim=c(0,1), ylim=c(0,5), ylab="Density value", xlab="Choice probability")
for (i in 1:5)
	lines(pr, dbeta(pr, alpha[i]/2, alpha[i]/2), lty = lty_vals[i])
legend(x='topleft', legend = alpha, lty=lty_vals, title='Values of alpha')

dev.off()
