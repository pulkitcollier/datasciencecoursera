
corr <- function(directory, threshold = 0){
	m <- complete(directory)
	i <- 0
	for(k in 1:332){
		if(m[k,2] > threshold){
			i <- i+1
		}
	}
	if(i != 0){
	r <- numeric(length(i))
	i <- 1
		for(k in 1:332){
			if(m[k,2] > threshold){
				id <- m[k,1]
				id <- formatC(id, width = 3, flag = "0")
				id <- paste(id, "csv", sep = ".")
				data <- read.csv(paste(directory, id, sep = "/"), header = TRUE, sep = ",", quote="", dec = ".", fill = TRUE, comment.char = "", na.strings = "NA")
				r[i] <- cor(data[,"X.nitrate."], data[,"X.sulfate."], use = "pairwise.complete.obs")
				i <- i + 1
			}
		}
	}
	else r <- numeric(length = 0)
	r
}