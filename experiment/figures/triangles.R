triangles = list(
	list(c(2,2), c(7,2), c(5,8)),
	list(c(2,4), c(5,1), c(8,8)),
	list(c(1,9), c(4,2), c(9,1)),
	list(c(2,2), c(6,2), c(2,9)),
	list(c(3,2), c(3,9), c(7,6)))

for (i in 1:5) {
	triangle = triangles[[i]]
	
	# Extract points from triangle definition
	x = triangle[[1]]
	y = triangle[[2]]
	z = triangle[[3]]

	# Compute lengths of sides
	a = sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
	b = sqrt((x[1]-z[1])^2 + (x[2]-z[2])^2)
	c = sqrt((z[1]-y[1])^2 + (z[2]-y[2])^2)

	# Compute area according to Heron's formula
	area = 0.25 * sqrt((a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c))
	print(area)
	
	# Plot triangle
	pdf(sprintf('triangle%i.pdf', i), paper='special', width=6, height=6)
	par(mar=c(2,2,1,1), pch=20, col.lab='white', col.axis='white', tck=0)
	plot(c(0, 10), c(0, 10), type = "n")
	polygon(c(x[1],y[1],z[1]), c(x[2],y[2],z[2]), col='green')
	dev.off()
}
