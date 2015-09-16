complete <- function(directory,id=1:332)
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
                        
                         ##Check how many rows are complete
                         completeCase <- complete.cases(fileContent)
                         
                         ##Count the number of rows with complete cases
                         numCompleteCase <- nrow(fileContent[completeCase,])
                         
                         ##Create a matrix with the file number and number of 
                         #complete case observations
                         if (i==1)
                         {
                             fileComplete <- data.frame(id[i],numCompleteCase)
                         }
                         else
                         {
                             tempMatrix <- c(id[i],numCompleteCase)
                             fileComplete <- rbind(fileComplete,tempMatrix)
                         }
                         
                         i <- i+1
                     }
                     
                     if (i==2)
                     {
                         fileComplete <- data.frame(id,numCompleteCase)
                     }
                     
                     ##give row name and column names to the matrix
                     colnames(fileComplete) <- c("id","nobs")
                     
                     ##print output
                     return(fileComplete)
                 }