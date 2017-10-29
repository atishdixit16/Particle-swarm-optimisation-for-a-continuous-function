randomTrials <- function(count = 100, varCount = 2, range = matrix(c(-5,-5,5,5),2,2)) {
        trialStack <- NULL
        for (i in 1:count) {
                trial <- NULL
                for (j in 1:varCount)
                        trial <- c( trial, runif(1,range[j,1],range[j,2]) )
                trialStack <- rbind(trialStack,trial)
        }
        trialStack
}

inputFunction <- function(values) {
       ( -20*exp(-0.2*sqrt(0.5*(values[1]^2 + values[2]^2))) - exp(0.5*(cos(2*pi*values[1]) + cos(2*pi*values[2])) ) + exp(1) + 20 )
}

output <- function(trialStack, inputFunction=inputFunction) {
	if (is.vector(trialStack))
		trialStack <- t(as.matrix(trialStack))
        light <- NULL
        rows <- nrow(trialStack)
        for (i in 1:rows) {
                light <- c(light, inputFunction(trialStack[i,]))
        }
        light
}

PSO <- function(func=inputFunction, funcStack=output, count=100,varCount=2,range = matrix(c(-5,-5,5,5),2,2), maxIterations=100,weight=1, phi_p=1, phi_g=1, display=FALSE) {
	
	
	trialStack <- randomTrials(count, varCount, range)
	velocity <- NULL
	for (i in 1:varCount)
		velocity <- cbind(velocity,runif(count,-abs(range[1,1]-range[1,2]),abs(range[2,1]-range[2,2])))
        pMin <- funcStack(trialStack,func)
	pMin_arg <- trialStack
	gMin <- min(pMin)
	gMin_index <- which(pMin==gMin)[1]
	for (itr in 1:maxIterations) {
	
		if (display==TRUE) {
			a <- seq(-5,5,0.1)
        	        l <- length(a)
                	mat <- matrix(0,l, l)
	                for (i in 1:l)
        	        	for (j in 1:l)
                	        	mat[i,j] = func(c(a[i],a[j]))
        	        image(a,a,mat)
			points(pMin_arg[,1],pMin_arg[,2],pch='+')
        	}

		for (i in 1:count) {
			trialStack[i,] <- weight*velocity[i,] + phi_p*runif(1)*(pMin_arg[i,] - trialStack[i,]) + phi_g*runif(1)*(pMin_arg[gMin_index,] - trialStack[i,])
			if (func(trialStack[i,]) < pMin[i]) {
				pMin[i] <- func(trialStack[i,])
				pMin_arg[i,] <- trialStack[i,]
				if (pMin[i] < gMin) {
					gMin <- pMin[i]
					gMin_index <- i
				}
			}
		}
	}
	min_f <- min(gMin)
        arg_min_f = pMin_arg[gMin_index,]
        #output
        list( min = min_f , arg_min = arg_min_f )	
}

answer <- PSO(func=inputFunction, funcStack = output, count=100,varCount=2,range = matrix(c(-5,-5,5,5),2,2), maxIterations=100,weight=1, phi_p=1, phi_g=1, display=TRUE)
print(answer)
