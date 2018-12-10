# File Name: CountingHTN.R
createTeethPresentCol <- function(data) {
  data$presentTeeth <- 0
  data$numHTeeth <- as.numeric(data$numHTeeth)
  for (i in 1:dim(data)[1]) {
    if (is.na(data$numHTeeth[i])) {
      data$presentTeeth[i] <- NA
      next
    }
    else if (data$numHTeeth[i] %/% 7 == 0) {
      data$presentTeeth[i] <- 1
    }
    else if (data$numHTeeth[i] %/% 7 == 1) {
      data$presentTeeth[i] <- 2
    }
    else if (data$numHTeeth[i] %/% 7 == 2) {
      data$presentTeeth[i] <- 3
    }
    else if (data$numHTeeth[i] %/% 7 == 3 | data$numHTeeth[i] == 28) {
      data$presentTeeth[i] <- 4
    }
  }
  return (data)
}

createTeethPresentCol2 <- function(data) {
  data$presentTeeth <- 0
  data$numHTeeth <- as.numeric(data$numHTeeth)
  for (i in 1:dim(data)[1]) {
    if (is.na(data$numHTeeth[i])) {
      data$presentTeeth[i] <- NA
      next
    }
    else if (data$numHTeeth[i] >= 0 & data$numHTeeth[i] <= 10)   {
      data$presentTeeth[i] <- 1
    }
    else if (data$numHTeeth[i] > 10 & data$numHTeeth[i] <= 20) {
      data$presentTeeth[i] <- 2
    }
    else if (data$numHTeeth[i] > 20 & data$numHTeeth[i] <= 28) {
      data$presentTeeth[i] <- 3
    }
    
  }
  return (data)
}








