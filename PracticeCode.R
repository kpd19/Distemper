#You insist that there is something that a machine can't do. If you will tell me precisely what it is that a machine cannot do, then I can always make a machine which will do just that. Jon von Neumann#

########################################################
######### CANINE DISTEMPER VIRUS IN SEA LIONS ##########
########################################################

#******************************************************#
#                       INDEX                          #
#******************************************************#
#     1. Functions in R                                #
#         1.1. Deterministic logistic growth           #
#         1.2. Plotting multiple values of a parameter #
#         1.3. Plotting N ~ t                          #
#           1.3.1. Deterministic                       #
#           1.3.2. Stochasitc                          #
#     2. Functions in discrete space                   #
#         2.1. Isolated populations                    #
#           2.1.1. One dimension                       #
#          *2.1.2. Two dimensions                      #
#         2.2. Connected populations                   #
#           2.1.1. One dimension                       #
#           2.1.2. Two dimensions                      #
#     3. Plotting in continuous space                  #
#                                                      #
#  * finished up to here                               #
#******************************************************#


#******************************************************#
#*************** 1. Functions in R ********************#
#******************************************************#

	#1.1. Deterministic logistic growth rate (dN/dt)
		# Quick note: entries within the parentheses are refered to as "arguments" and "variable" whereas the entries within the braces are "expressions."
		lg <- function(Nt = 10 , r = 0.1 , K = 25){
			Nt*r*(1-(Nt/K))
		}

	#1.2. Plotting multiple values of a parameter
		#1.2.1. One variable value, unvaried
			#Here's one point, sort of unecessary
			N.samp <- 3
			lg()
			lg(Nt = N.samp)
			plot(x = N.samp , y = lg(Nt = N.samp) , ylab = "dN/dt" , xlab = "N")

		#1.2.2. Value from varying one parameter
			#Plotting multiple values of Nt can be done with creating a list of values
			N.samp <- c(3 , 4 , 10 , 25 , 50)
			plot(N.samp , lg(Nt = N.samp) , ylab = "dN/dt" , xlab = "N")
			plot(N.samp , lg(Nt = N.samp) , ylab = "dN/dt" , xlab = "N" , type = "l")
	
			#or
			start <- proc.time()
			N.samp <- seq(from = 0 , to = 25 , by = 0.000001)
			plot(N.samp , lg(Nt = N.samp) , ylab = "dN/dt" , xlab = "N" , type = "l")
			proc.time()-start # by = 10^c(1:-6), takes 0.051 , 0.048 , 0.077 , 0.096 , 0.083 , 0.276 , 2.167 , 15.243

			#or, a more effective way
			start <- proc.time()
			curve(lg(Nt = x) , from = 0 , to = 25)
			proc.time() - start #smoother graph, contiuous, and only 0.049 seconds
	
			#The whole "time" thing may seem unncessary at this step, but remember that this is also an incredibly simple function and we easily had a 2- and 15-second runtimes of a simple 2-dimensional x-y plot.  This is important because once we have (i) more complicated models (ii) and a 3-dimensional x-y-z plot (x and y are coordinates and z is the value at the coordinate), the times become multiplied, and notice that the computiational time is exponential--that's not good.

			#Just as an example to make sure that the points and the curve match up
			N.samp <- c(3 , 4 , 10 , 25 , 50)
			curve(lg(Nt = x) , from = 0 , to = 25 , ylab = "dN/dt" , xlab = "N")
			points(N.samp , (lg(Nt = N.samp)))

		#1.2.3. Value from varying two parameters
			#We already varied Nt, now we'll vary r
			r.vec <- seq(1,5,.1)
			elements <- length(r.vec)
			curve(lg(Nt = x , r = r.vec[elements]) , type = "n" , from = 0 , to = 25 , xlab = "N" , ylab = "dN/dx")
			for (i in 1:length(r.vec)){
				curve(lg(Nt = x , r = r.vec[i]) , from = 0 , to = 25 , add = T , col = rainbow(elements)[i])
			}

	#1.3. Finding N for logistic function
		#Good coding practices do not include "magic numbers."  These are numbers that are inserted into code without description.  Assigning values as objects like below is a better practice because you can remember their role in the model.
		#1.3.1. Finding N for logistic function
			K <- 50
			r <- 0.2
			t <- 100
			N0 <- 0.1
			N <- numeric(t)
			N[1]
			N[1] <- N0
			N
			for (i in 2:t){
				N[i] <- lg(Nt = N[i-1] , r = r , K = K) + N[i-1]
			}
			plot(1:t , N , type = "l" , xlab = "Time" , ylab = "N" , lwd = 2 , col = "blue")
			abline(h = K , col = "red" , lty = 2)

		#1.3.2. Finding N for stochastic logistic function (same as above, but with variance [sigma] added)
			K <- 50
			r <- 0.2
			t <- 100
			sigma <- 0.5
			N0 <- 10 #raising N0 from 1.3.1. so pop size doesn't become negative
			N <- numeric(t)
			N[1]
			N[1] <- N0
			N
			for (i in 2:t){
				N[i] <- lg(Nt = N[i-1] , r = r , K = K) + N[i-1] + rnorm(1 , 0 , sigma)
			}
			plot(1:t , N , type = "l" , xlab = "Time" , ylab = "N" , lwd = 2 , col = "blue")
			abline(h = K , col = "red" , lty = 2)


#******************************************************#
#********* 2. Functions in discrete space *************#
#******************************************************#
	 #2.1. Isolated populaitons
		#2.1.1. One dimension
			x <- 10
			z <- N0 <- 5
			t <- time <- 50
			seed <- 20
			onedim <- array(data = NA , dim = c(x,t))
			onedim[,1] <- z
			onedim[5,1] <- seed
			K <- 50
			r <- 0.2
			sigma <- 0.75
			onedim #the sincle time filled in (N0) should be the first column, and include the seed at the 5th position

			#SIMULATE!
			for (i in 2:t){
				for (j in 1:x){
				onedim[j,i] <- lg(Nt = onedim[j,i-1] , r = r , K = K) + onedim[j,i-1] + rnorm(1 , 0 , sigma)
				}
			}
			onedim #NA, Inf, or -Inf?  If not, we're golden

			#Plot individual population sizes (grey), mean (blue), and the carrying capacity (K)
			plot(0 , type = "n" , xlim = c(1,t) , ylim = c(min(onedim) , max(onedim)))
			abline(h = K , col = "red" , lty = 2)
			for (i in 1:t){
			lines(1:t , onedim[i,] , col = "#50505050") #note the color is a rgb hex, so each two digit is red, green, blue, then a transparency value (I wanted them to be transparent, so I set it to 50 of the default 255)
			}
			lines(x = 1:t , y = apply(onedim , 2 , mean , na.rm = T) , col = "blue" , type = "l" , lwd = 2)
			#apply() above takes a vector and applies a function to it.  It also has an argument for dimention [in our case we wanted dimention 2, the column].  So we took the vector "onedim" and the mean of each column.

		#2.1.2. Two dimensions
			#Create a lattice
			x <- 10
			y <- 10
			z <- N0 <- 10
			t <- time <- 60
			lat <- array(data = NA , dim =  c(x,y,t))
			lat[,,1] <- z
	
			#Aside from time (t), fill in other parameters
			K <- 50
			r <- 0.2
			sigma <- 1.75
	
			#Simulate stochastic logistic growth at each node
			for (i in 2:t){
				for (j in 1:x){
					for (k in 1:y){
				lat[j,k,i] <- lg(Nt = lat[j,k,i-1] , r = r , K = K) + lat[j,k,i-1] + rnorm(1 , 0 , sigma)
					}
				}
			}
			lat
	
			#Plot all populations with mean atop as a function of time
			plot(0, type = "n" , xlim = c(0,t) , ylim = c(N0-sigma,K*1.25))
			for (i in 1:x){
			for (j in 1:y){
					lines(1:t , lat[i,j,] , type = "l" , col = "#50505025")
			}
			}
			abline(h = K , col = "red" , lty = 2)
			lines(1:t , apply(lat,3,mean) , col = "blue" , lwd = 2)

			#Graph results on a grid
			#Point size, use a cex ratio (1 is default) of individual point over the maximum: cex = lat[i,j,t]/max(lat[,,t])
			#color
			hotcool <- colorRampPalette(c("red" , "blue"))
			par(pin = c(4,4) , oma = rep(0,4)) #I locked the plot size with pin() within par().  par() allows you to change the settings of the display, like the margins, number of plots, etc.
			plot(1:x , 1:y , xlim = c(0,x+1) , ylim = c(0,y+1) , type = "n" , frame = F , axes = F , xlab = "" , ylab = "")
			for (i in 1:x){
			for (j in 1:y){
				points(j , x-i , col = hotcool(x*y)[lat[i,j,t]] , pch =15 , cex = 5)
			}
			}
			mtext("x",side = 1, line = 1)
			mtext("y",side = 2, line = 0 , las = 1)
			lat[,,t]

###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###
###~~~###                                             ###~~~###
###~~~###    beginning of my current working spot     ###~~~###
###~~~###                                             ###~~~###
###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###








	#2.2. Connected populations
			#2.2.1. First, one dimension, isolated popuations
				#2.2.2. One dimension, connected populations
					x <- 10
					z <- N0 <- 10
					t <- time <- 200
					seed <- 20
					onedim <- array(data = NA , dim = c(x,t))
					onedim[,1] <- z
					onedim[2,1] <- seed
					K <- 50
					r <- 0.2
					sigma <- 0
					for (i in 2:t){
						for (j in 2:(x-1)){
						onedim[j,i] <- lg(Nt = mean(onedim[c(j,j-1,j+1),i-1] , na.rm = T) , r = r , K = K) + onedim[j,i-1] + rnorm(1 , 0 , sigma)
						}
					}

		plot(0 , type = "n" , xlim = c(1,t) , ylim = c(0 , max(onedim , na.rm = T)) , ylab = "N" , xlab = "Time")
		abline(h = K , col = "red" , lty = 2)
		for (i in 1:t){
		points(1:t , onedim[i,] , type = "b" , col = rainbow(x)[i] , pch = paste(i) , cex = 0.5)
		}
		lines(x = 1:t , y = apply(onedim , 2 , mean , na.rm = T) , col = "blue" , type = "l" , lwd = 2)









###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###
###~~~###                                             ###~~~###
###~~~###~~~### end of my current working spot  ###~~~###~~~###
###~~~###                                             ###~~~###
###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###~~~###




		mlat <- array(data = NA , dim =  c(x,y,t))
		lat <- array(data = NA , dim =  c(x,y,t))
		lat[,,1] <- z
		mlat[,,1] <- z

		#Aside from time (t), fill in other parameters
		K <- 50
		r <- 0.2
		sigma <- 0.75

		#nearest-eight neighbor function

		#Aimulate stochastic logistic growth at each node
		for (i in 2:t){
			for (j in 1:x){
			for (k in 1:y){
			lat[j,k,i] <- lg(Nt = lat[j,k,i-1] , r = r , K = K) + lat[j,k,i-1] + rnorm(1 , 0 , sigma)

			mlat[j,k,i] <- lg(Nt = lat[j,k,i-1] , r = r , K = K*9) + lat[j,k,i-1] + rnorm(1 , 0 , sigma)


			}
			}
		}
		lat
		mlat


######################################################
##### Continuous dispersal on a discrete lattice #####
######################################################

	lat_disp <- function(pop , kernel , ...)
		{
			lattice_size <- dim(pop)
			new_pop <- array(0 , dim = lattice_size)
			for (i in 1:lattice_size[1])
				{
				for (j in 1:lattice_size[2])
					{
					N <- pop[i , j]
					dist<-kernel(N,...)
					theta<-runif(N,0,2*pi)
					x<-cos(theta)*dist
					y<-sin(theta)*dist
					for(k in 1:N)
						{
							x_ind<-(round(i+x[k])-1) %% lattice_size[1] + 1
							y_ind<-(round(j+y[k])-1) %% lattice_size[2] + 1
							new_pop[x_ind,y_ind]<-new_pop[x_ind,y_ind]+1
						}
					}
				}
			return(new_pop)
	}

	############## Run and plot #######################
	
	## Custom colour ramp
		colours <- c("light blue" , "yellow" , "red")
		cus_col <- colorRampPalette(colors = colours , space = c("rgb" , "Lab"),interpolate = c("linear" , "spline"))

	## Initialize population array
		Time <- 100
		xx <- 50
		yy <- 50
		pop <- array(0 , dim = c(xx , yy , Time))
		pop[1 , 25 , 25] <- 10000
	
	### Normal Kernel ###
		par(mfrow = c(1 , 1))
		for(i in 2:Time)
			{
				image(pop[i-1,,],col=cus_col(100),xaxt='n',yaxt='n')
				pop[i,,]<-lat_disp(pop[i-1,,],kernel=rnorm,mean=0,sd=1)
			}
	 
	## Plot
		# png('normal_kern.png', width = 800, height = 800)
		par(mfrow = c(3 , 3) , pty = "s", omi = c(0.1 , 0.1 , 0.5 , 0.1),mar=c(1 , 0 , 1 , 0))
		times <- c(round(((Time/9)*(1:9)) , digits = 0))
		for(i in times)
		image(pop[i-1,,],
		col=cus_col(100),
		xaxt='n',
		yaxt='n',
		useRaster=TRUE,
		main=paste("time =",i))		 
		mtext("Gaussian Kernel",outer=TRUE)
		# dev.off()
 
 
############################################################


#******************************************************#
#******** 3. Functions in continuous space ************#
#******************************************************#

