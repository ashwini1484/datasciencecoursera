corr <- function(directory,threshold=0)
        {
            i <- 1
            corrVector <- vector()
            
            ##Run the loop and read files until all the input files are read
            while (i <= 332)
            {
                ##If the file number contains a single digit, append 2 0's so the file 
                ##name comes right
                if (i < 10)
                {
                    fileNumber <- paste("00",i,sep="")
                }
                
                ##If the file number contains double digit, append one 0 so the file 
                ##name comes right
                if (i >= 10 & i < 100)
                {
                    fileNumber <- paste("0",i,sep="")
                }
                
                ##If the file number contains three digits, copy the file name as is
                if (i >= 100)
                {
                    fileNumber <- paste(i,sep="")
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
                
                point <- 1
                
                if (numCompleteCase > threshold)
                {
                    corrVector[point] <- cor(fileContent[completeCase,]$sulfate,
                                             fileContent[completeCase,]$nitrate)
                    point <- point+1
                }
                
                i <- i+1
            }
            return(corrVector)
            print(i)
        }