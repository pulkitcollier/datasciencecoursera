
complete <- function(directory, id = 1:332){
	i <- 1
	j <- id
	id <- formatC(id, width = 3, flag = "0")
	id <- paste(id, "csv", sep = ".")
	sum <- vector(mode = "numeric", length = length(j))
	for(k in j){
		data <- read.csv(paste(directory, id[i], sep = "/"), header = TRUE, sep = ",", quote="", dec = ".", fill = TRUE, comment.char = "", na.strings = "NA")
		data1 <- complete.cases(data)
		for(l in 1:length(data1)){
			if(data1[l])
			sum[i] <- sum[i] + 1
		}
		i <- i + 1
	}
	d <- data.frame(j, sum)
	names(d) <- list("id","nobs")
	d
}
