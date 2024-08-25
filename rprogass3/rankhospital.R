rankhospital <- function(state, outcome, num){
	state <- as.character(state)
	outcome <- as.character(outcome)
	outcomeofcare <- read.csv("outcome-of-care-measures.csv")
	poutcome <- list("11" = "heart attack", "17" = "heart failure", "23" = "pneumonia")
	if(!any(outcomeofcare$State == state)){
		stop("invalid state")
	}
	if(!any(poutcome == outcome)){
		stop("invalid outcome")
	}
	outcomei <- as.integer(names(poutcome[which(poutcome == outcome)]))
	los <- split(outcomeofcare,outcomeofcare$State)
	dos <- as.data.frame(los[state])
	dos[,outcomei] <- as.numeric(as.vector((dos[,outcomei])))
	dos[,2] <- as.character(dos[,2])
	if(num == "best"){
		mdos <- which(dos[,outcomei] == min(dos[,outcomei],na.rm=TRUE))
		hdos <- c(length(mdos))
		j =1
		for(i in mdos){
			hdos[j] <- dos[i,2]
			j = j+1
		}
		hdos <- sort(hdos)
		hdos[1]
	}else if(num == "worst"){
		mdos <- which(dos[,outcomei] == max(dos[,outcomei],na.rm=TRUE))
		hdos <- c(length(mdos))
		j =1
		for(i in mdos){
			hdos[j] <- dos[i,2]
			j = j+1
		}
		hdos <- sort(hdos)
		hdos[1]
	}else if (num > length(dos[,2])){
		NA
	}else {
		cos <- data.frame(dos[,2],dos[,outcomei])
		cos <- cos[order(cos[,2], na.last = NA),]
		as.character(cos[num,1])
	}
}