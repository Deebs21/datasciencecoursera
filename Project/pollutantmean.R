pollutantmean <- function(directory, pollutant, id)
{
  directory <- ("./specdata/")
  pollutant <- ("sulfate")
  id <- 4:50

  mean_pollution <- c()
  allfiles <- list.files(directory, pattern = ".csv")
  selectfiles <- paste(directory, allfiles)
  for (i in id)
    {
      dataset <- read.csv(selectfiles[i])
      head(dataset)
      pollutant
      badrecords <- dataset[!is.na(dataset[,pollutant]), pollutant]
      mean_pollution <- c(mean_pollution, badrecords)
    }
  
  mean_result <- mean(mean_pollution)
  return(mean_result)
  
}

