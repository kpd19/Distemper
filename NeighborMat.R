# Note: this only works for square matricies (I should have started with a general rectangle because I likely have a row/column confusion)
# this makes two matricies: one for row values, the other for columns
	rm(list=ls())
	rows <- 3
	cols <- 3
	dimensions <- 2
	dist.arr <- array(data = 0  , dim = c(rows,cols,dimensions))
		for (r in 1:rows){
			dist.arr[r,,1] <- rep(r,rows)
		}
		for (c in 1:cols){
			dist.arr[,c,2] <- rep(c,cols)
		}
	dist.arr

# this finds the max distance between focal point (i,j) and the rest of the elements (m,n)
	k.arr <- array(data = NA , dim=c(rows,cols,rows,cols))
	for (i in 1:rows){
	for (j in 1:cols){
	for (m in 1:rows){
	for (n in 1:cols){
		k.arr[i,j,m,n] <- matrix((max(c(abs(i-m),abs(j-n)))))
		}
		}
		}	
		}
	k.arr

# this multiplies them by a distance
	dist <- 10
	for (i in 1:length(k.arr)){
		k.arr[i] <- k.arr[i]*dist
	}
	k.arr

#this turns them into a vector and adds them into a matrix in the way you had it formatted
	cells <- rows*cols
	for (i in 1:rows){
	for (j in 1:cols){
		k.mat <- as.vector(k.arr)
	}
	}
		k.mat <- matrix(k.arr , nrow = cells)
		k.mat

#creats your matrix
	distance <- matrix(c( 0, 10, 20, 10, 10, 20, 20, 20, 20, 10, 0, 10, 10, 10, 10, 20, 20, 20, 20, 10, 0, 20, 10, 10, 20, 20, 20, 10, 10, 20, 0, 10, 20, 10, 10, 20, 10, 10, 10, 10, 0, 10, 10, 10, 10, 20, 10, 10, 20, 10, 0, 20, 10, 10, 20, 20, 20, 10, 10, 20, 0, 10, 20, 20, 20, 20, 10, 10, 10, 10, 0, 10, 20, 20, 20, 20, 10, 10, 20, 10, 0),nrow=9, ncol=9)

#difference between them--they're a match! (Note: only works for a 3 x 3 comparison though.)
	k.mat-distance