
pollutantmean <- function(directory, pollutant, id = 1:332){
	i <- 1
	j <- id
	id <- formatC(id, width = 3, flag = "0")
	id <- paste(id, "csv", sep = ".")
	len <- 0
	pol <- paste("X.", pollutant, ".", sep = "")
	for(k in j){
		data <- read.csv(paste(directory, id[i], sep = "/"), header = TRUE, sep = ",", quote="", dec = ".", fill = TRUE, comment.char = "", na.strings = "NA")
		len <- len + length(data[,pol])
		i <- i + 1
	}
	i <- 1
	data1 <- numeric(length = len)
	len <- 1
	for(k in j){
		data <- read.csv(paste(directory, id[i], sep = "/"), header = TRUE, sep = ",", quote="", dec = ".", fill = TRUE, comment.char = "", na.strings = "NA")
		for(l in 1:length(data[,pol])){
			data1[len] <- data[l,pol]
			len = len + 1
		}
		i <- i + 1
	}
	id <- mean(data1, na.rm = TRUE)
	id
}
