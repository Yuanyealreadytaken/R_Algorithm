library(igraph)
opar <- par(no.readonly=TRUE)
par(cex.axis=0.8)
par(cex.lab=0.8)

#g <- random.graph.game(20, 5/20, directed=TRUE)
#page.rank(g)$vector
#g2 <- graph.star(10)
#page.rank(g2)$vector
# Personalized PageRank
#g3 <- graph.ring(10)
#page.rank(g3)$vector
#reset <- seq(vcount(g3))
#page.rank(g3, personalized=reset)$vector

#mhist <- hist(links$time[links$time!=0], axes=FALSE, breaks=20, col='red', xlab="Link Time", main="Facebook link temporal distribution")
#maxis.x <- mhist$breaks/3600/24/365;

#axis(1, at=mhist$breaks, label=maxis.x, col.axis='red')

#rug(jitter(links$time))
#lines(density(links$time), col='blue', lwd=2)

links <- read.table("D:/R_WORKSPACE/DATA/facebook-wosn-links-clean.txt", header=FALSE, sep=" ", col.names=c("from","to","time"))
edge.length <- length(links$from)
edge.matrix <- matrix(c(links$from,links$to), nrow=edge.length, ncol=2, byrow=FALSE)
mgraph <- graph.edgelist(edge.matrix)


#deg.out <- degree(mgraph, mode='out')
#freq <- as.data.frame(table(deg.out), stringsAsFactors = FALSE)
#plot(log(as.integer(freq$deg.out), base=10), freq$Freq, cex=0.5, axes=FALSE, xlab="Out Degree", ylab="Frequency", col="blue")
#axis(1,at=0:3, labels=10^(0:3))
#axis(2)

#deg.in <- degree(mgraph, mode='in')
#freq <- as.data.frame(table(deg.in), stringsAsFactors = FALSE)
#plot(log(as.integer(freq$deg.in), base=10), freq$Freq, cex=0.5, axes=FALSE, xlab="In Degree", ylab="Frequency", col="blue")
#axis(1,at=0:3, labels=10^(0:3))
#axis(2)

dist.total <- length(degree(mgraph, mode="out"))
deg.dist.out <- degree.distribution(mgraph, mode="out")
plot(log(1:length(deg.dist.out), base=10), log(deg.dist.out*dist.total, base=10), cex=0.5, axes=FALSE, xlab="In Degree (n)", ylab="Frequency", col="blue")
axis(1, at=c(0, log(1:3, base=10)), labels=c(0, 10, 100, 1000))
axis(2, at=0:4, labels=10^(0:4)-1)

deg.dist.cum.out <- degree.distribution(mgraph, mode="in", cumulative=TRUE)
plot(log(1:length(deg.dist.cum.out), base=10), log(deg.dist.cum.out, base=10), type="l", cex=0.5, axes=FALSE, xlab="In Degree (n)", ylab="Cumulative Frequency", col="blue")
axis(1, at=0:3, labels=10^(0:3)-1)
par(yaxp=c(-5, 0, 10))
axis(2)
#par(yaxp=c(0,1,10))
#axis(2, at=-(10^(0:5)), labels=10^(0:-5))

#print("I'm here!")

#par(pch=8)
#par(xlog=TRUE)
#par(usr=c(0,3,0,5))
#par(xaxp=c(0, 1000, 1))
#axis.x <- c(0, 10, 100, 1000)
#axis(1, at=deg.out.freq, label=axis.x)

#plot(mgraph)
#par(opar)
#plot(degree(mgraph, mode="out"))
#degree.distribution(mgraph)
