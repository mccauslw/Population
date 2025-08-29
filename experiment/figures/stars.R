N = c(280, 290, 300, 310, 320)
for (i in 1:5) {
	pdf(sprintf('stars%i.pdf', i), paper='special', width=6, height=6)
	par(mar=c(2,2,1,1), pch=20, col.lab='white', col.axis='white', tck=0)
	plot(runif(N[i]), runif(N[i]))
	dev.off()
}