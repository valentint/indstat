

(x2 <- matrix(1:10, ncol=2))
(x1 <- x2)
rownames(x2) <- rownames(x1) <- 2000+x2[,1]
colnames(x2) <- c("A", "B")
colnames(x1) <- c("C", "D")
x2
x1

bb <- barplot(t(x2), beside=TRUE)
bb <- colSums(bb)/2
bb

lines(bb, x1[,1], col="red")
points(bb, x1[,1], col="red")
lines(bb, x1[,2], col="blue")
points(bb, x1[,2], col="blue")

do.bars.line(x1[,1, drop=FALSE],x2)
