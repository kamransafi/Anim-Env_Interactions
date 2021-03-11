library(move)
data <- leroy

plot(leroy, type="l", lwd=2, bty="n", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
title("Leroy")
title(sub="Our dear fisher")

points(leroy, pch=20)
points(leroy, pch=1, col="firebrick")
#something to commit

