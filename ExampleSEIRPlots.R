### Some initial notes on visuallizing the SEIR data

#make a fake matrix with some differences between some populations at an initial timepoint--this should resemble the output you had from your model with rows as populations and columns as time
	rows <- 9
	cols <- 9
	pops <- rows*cols
	t0 <- runif(pops , 1 , 10) #random draws form a uniform distribution from values 1 to 10 for each pop
	time <- 10 #note we start at t = 0 so the total steps will be time - 1

	matty <- matrix(data = t0 , nrow = length(t0) , ncol = time) # don't worry, the other times will be replaced
	colnames(matty) <- paste("t", seq(from = 0 , to = time-1 , by = 1) , sep="") #just makin' names
	rownames(matty) <- paste("pop", seq(from = 1 , to = pops , by = 1) , sep="") #just makin' names

	# here's some fake data, adding values to a previous one (kind of, I tweaked it to make them correlate and the variance jump just for graphical purposes)
	for (i in 2:time){
	for (j in 1:pops){
		matty[j,i-1] <- matty[1,i-1] + rnorm(n = 1 , mean = 1 , sd = 1)
	}}

	#have a quick look at the data.  Here, we look at the row values, but you can also do the same for the columns.  It may seem trivial now, but it becomes geometrically important as space, time, individuals, and populations increase in size
	matty[1:5,] #sneak peak of the top five rows
	matty[seq(nrow(matty)-5,nrow(matty),1),] #sneak peak of the bottom five rows

#1.1.  first, let's explore just one (S,E,I, or R) using the sample above (how you have your compartments organized), we'll divide is in half (A and B) and plot them
	 #make a blank plot and set axes
	 plot(0 , type = "n" , xlim = c(1,time) , ylim = c(0,max(matty)) , xlab = "time" , ylab = "units")

	#then add the individual population lines
	line.col <- rgb(50,50,50,40,maxColor=255) #you can manually create colors, and I made grey here
	for (i in 1:pops){
		lines(matty[i,] , col = line.col)
	}

	#now add the mean, and, for fun, the variance
	lines(apply(matty , 2 , mean) , col = "red" , lwd = 2 , lty = 2) #lwd is line width, and lty is line type
	lines(apply(matty , 2 , var) , col = "blue" , lwd = 2 , lty = 2) #plots the variance

#1.2.  second, just quilky, let's just see how we could plot two means, since I imagine you'd like to see all 4 (S,E,I, and R) in a plot.
	no.pop.A <- nrow(matty)%/%2 # "%/%" is integer division in the event there is a decimal left over
	no.pop.B <- nrow(matty)-no.pop.A
	matty.A <- matty[1:no.pop.A,]
	matty.B <- matty[no.pop.B:nrow(matty),]

	 plot(0 , type = "n" , xlim = c(1,time) , ylim = c(0,max(matty)) , xlab = "time" , ylab = "units")
	lines(apply(matty.A , 2 , mean) , col = "red" , lwd = 2 , lty = 2)
	lines(apply(matty.B , 2 , mean) , col = "blue" , lwd = 2 , lty = 2)
	#no duh they're not really different, but we wouldn't expect that anyway

#2.1.  second, let's plot it in space (no pattern should be observable here since there are all independently varying) of the final values of the matrix (note that you could do this for any timepoint)
	sq.matty <- matrix(data = matty , nrow = rows , ncol = cols)

	rbPal <- colorRampPalette(c("red" , "yellow" , "white"))
	col.breaks <- 50
	col.pal <- matrix(rbPal(col.breaks)[as.numeric(cut(sq.matty,breaks = col.breaks))] , nrow = rows , byrow = F)

	plot(1:rows , 1:cols , type = "n")
	for (i in 1:rows){
	for (j in 1:cols){

		points(i , j , cex = 5 , col = col.pal[i,j] , pch = 15)
			}}

	#look at the matrix
	sq.matty
	min(sq.matty) #should be red
	max(sq.matty) # should be white
	# the matrix is tilted 90^o counter clockwise.  This can be fixed, but I spent a few minutes too many so I gave up since it is inconsequential at this point.  

#2.2. but that's fugly, right?
	sq.size <- 45/rows # this is just a close fit based on a quick visualization, but you can adjust the grid cells as finely as you want. There's a mathematical way to relate the size of the square, grid size, and plot size, but this is still a quick & dirty fit.
	par(pin = c(4,4) , oma = rep(0,4))
	plot(0 , xlim = c(-1 , rows + 1) , ylim = c(-1 , cols + 1) , type = "n" , frame = F , axes = F , xlab = "" , ylab = "")
	for (i in 1:rows){
	for (j in 1:cols){

		points(i , j , cex = sq.size , col = col.pal[i,j] , pch = 15)
			}}
	mtext("x",side = 1, line = 2) # the argument "line = " moves things closer and farther from the side it is on, and can take negative values.
	mtext("y",side = 2, line = 2 , las = 1) # las rotates the labels 90 degrees
	axis(1 , line = 0)
	axis(2 , line = 0, las = 1)
	#this can be further cleaned up, but the meaningful parts of the operations quickly become overshadowed by the extra arguments--just know that it can easily be done
