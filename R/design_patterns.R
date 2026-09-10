library(here)
design1.x = c(9, 13, 15, 14, 15)
design1.y = c(50, 70, 70, 75, 80)

design2.x = c(62.5, 65.0, 70.0, 72.5, 75.0)
design2.y = c(8.0, 7.5, 8.5, 10.0, 9.5)

design3.x = c(34, 22, 19, 7, 22)
design3.y = c(4.4, 4.0, 3.9, 3.5, 3.9)

design4.x = c(4.0, 3.4, 3.25, 3.1, 2.5)
design4.y = c(1.0, 1.8, 2.0, 2.2, 3.0)

design5.x = c(0, 0.5, 1, 2, 4)
design5.y = c(200, 245, 255, 315, 465)

design6.x = c(1, 2, 4, 6, 8)
design6.y = c(35, 45, 49, 54, 58)

design7.x = c(3.6, 3.9, 4.2, 4.5, 4.8)
design7.y = c(215, 263, 311, 358, 406)

pch = c('a', 'b', 'c', 'd', 'e')

pdf(here("paper/figures", "design_patterns.pdf"), paper='special', width=6.5, height=4.0)
op = par(
	mfrow=c(2,4),
	mar=c(3.4, 4, 1.6, 0.5),
	mgp=c(2.2, 0.7, 0),
	cex.main=0.9
)
plot(NULL, NULL, main='Beer', xlab='Price/sixpack', ylab='Average quality rating', xlim=c(8,16), ylim=c(40,90))
for (i in 1:5) {
	points(design1.x[i], design1.y[i], pch=pch[i])
}
plot(NULL, NULL, main='Cars', xlab='Would buy again (%)', ylab='Litres per 100 km', xlim=c(60,80), ylim=c(7,10))
for (i in 1:5) {
	points(design2.x[i], design2.y[i], pch=pch[i])
}
plot(NULL, NULL, main='Restaurants', xlab='Transport time (min.)', ylab='Rating', xlim=c(0,40), ylim=c(3,5))
for (i in 1:5) {
	points(design3.x[i], design3.y[i], pch=pch[i])
}
plot(NULL, NULL, main='Layovers', xlab='Flight time (hours)', ylab='Layover time (h)', xlim=c(2,5), ylim=c(0,4))
for (i in 1:5) {
	points(design4.x[i], design4.y[i], pch=pch[i])
}
par(mar=c(3.4, 4, 2.2, 0.5))
plot(NULL, NULL, main='Future', xlab='Delay (years)', ylab='Amount ($)', xlim=c(0,5), ylim=c(100,500))
for (i in 1:5) {
	points(design5.x[i], design5.y[i], pch=pch[i])
}
plot(NULL, NULL, main='Phone plans', xlab='Data (GB)', ylab='Price ($)', xlim=c(0,10), ylim=c(0,60))
for (i in 1:5) {
	points(design6.x[i], design6.y[i], pch=pch[i])
}
plot(NULL, NULL, main='Hotels', xlab='Number of stars', ylab='Price ($)', xlim=c(3.5,5), ylim=c(200,450))
for (i in 1:5) {
	points(design7.x[i], design7.y[i], pch=pch[i])
}
dev.off()
par(op)
