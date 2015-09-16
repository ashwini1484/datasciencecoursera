pollutantmean <- function(directory,pollutant,id=1:332)
{
  i <- 1
  
  ##Run the loop and read files until all the input files are read
  while (i <= length(id))
  {
    ##Validate id field to check that it does not have invalid file number
    ##There are 332 input files
    if (id[i] < 1 || id[i] > 332)
    {
      print(paste("File number ",id[i]," does not exist"))
      break
    }
    
    ##If the file number contains a single digit, append 2 0's so the file 
    ##name comes right
    if (id[i] < 10)
    {
      fileNumber <- paste("00",id[i],sep="")
    }
    
    ##If the file number contains double digit, append one 0 so the file 
    ##name comes right
    if (id[i] >= 10 & id[i] < 100)
    {
      fileNumber <- paste("0",id[i],sep="")
    }
    
    ##If the file number contains three digits, copy the file name as is
    if (id[i] >= 100)
    {
      fileNumber <- paste(id[i],sep="")
    }
    
    ##Generate the file path to be read
    fileName <- paste(directory,"/",fileNumber,".csv",sep="")
    
    ##Read the file
    fileContent <- read.csv(fileName)
    
    #Append all the files to one another into a single data frame
    if (i==1)
    {
      fileConcat <- fileContent
    }
    else
    {
      fileConcat <- rbind(fileConcat,fileContent)
    }
    
    i <- i+1
  }
  
  ##calculate mean of pollutant
  if(pollutant=="sulfate")
  {
    pollutantMean <- mean(fileConcat$sulfate,na.rm=TRUE)
  }
  else
  {
    pollutantMean <- mean(fileConcat$nitrate,na.rm=TRUE)
  }
  print(pollutantMean)
}