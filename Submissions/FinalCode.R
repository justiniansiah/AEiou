#Load Libraries
    if(!require(caTools)){
    install.packages("caTools")
    library(caTools)
  }
  if(!require(randomForest)){
    install.packages("randomForest")
    library(randomForest)
  }
  if(!require(TTR)){
    install.packages("TTR")
    library(TTR)
  } 
############################################  
## Read datasets
  dataset1 <- read.csv("../Data/train_dataset01.csv")
  dataset2 <- read.csv("../Data/train_dataset02.csv")
  dataset3 <- read.csv("../Data/test_dataset.csv")

############################################  
## Split Data
  set.seed(1)
  split <- sample.split(dataset2$ATT_FLAG, SplitRatio = 0.7)
  train <- subset(dataset2, split == TRUE)
  test <- subset(dataset2, split == FALSE)
  
############################################ 
## Random Forest Model
  set.seed(2e4)
  forest_fulldata <- randomForest(data = train, ATT_FLAG~LEVEL_T1+LEVEL_T2+LEVEL_T3+LEVEL_T4+LEVEL_T5+LEVEL_T6+LEVEL_T7+
                                    PRESSURE_J280+PRESSURE_J269+PRESSURE_J300+PRESSURE_J256+PRESSURE_J289+PRESSURE_J415+PRESSURE_J302+PRESSURE_J306+PRESSURE_J307+PRESSURE_J317+PRESSURE_J14+PRESSURE_J422+
                                    FLOW_PU1+FLOW_PU2+FLOW_PU3+FLOW_PU4+FLOW_PU5+FLOW_PU6+FLOW_PU7+FLOW_PU8+FLOW_PU9+FLOW_PU10+FLOW_PU11+FLOW_V2+
                                    STATUS_PU1+STATUS_PU2+STATUS_PU4+STATUS_PU6+STATUS_PU7+STATUS_PU10+STATUS_PU11+STATUS_V2)
  
  RFPred <- predict(forest_fulldata, newdata = dataset3)

############################################ 
## Prepare Training Data 
  #remove unecessary and non numeric data
  sigdata <- subset(dataset1, select=-c(DATETIME,FLOW_PU5,FLOW_PU9,STATUS_PU1,STATUS_PU2,STATUS_PU3,STATUS_PU4,STATUS_PU5,STATUS_PU6,STATUS_PU7,STATUS_PU8,STATUS_PU9,STATUS_PU10,STATUS_PU11,STATUS_V2,ATT_FLAG))
  
  #function to get mean and sd
  meansd <- function(data){
    mean = mean(data)
    sd = sqrt(var(data))
    return(c(mean,sd))
  }
  
  sigdata_mean <- rep(NA,ncol(sigdata))
  sigdata_sd <- rep(NA,ncol(sigdata))
  
  #generate mean and sd
  for (i in 1:ncol(sigdata)){
    sigdata_mean[i] = meansd(sigdata[,i])[1]
    sigdata_sd[i] = meansd(sigdata[,i])[2]
  }
  #sigdata_mean #contains the mean values of each var
  #sigdata_sd   #contains the sd values of each var

############################################ 
## Main Function 
#Function to compare input data with mean and flag is its > a certain distribution away 
  normalmethod <- function(tempdf){
    normpred <- rep(NA,nrow(tempdf))
    normpredmat <- matrix(FALSE,nrow = nrow(tempdf), ncol = ncol(tempdf),byrow = TRUE)
    
    #go through every row
    for(i in 1:nrow(tempdf)){
      #not attack on default
      flag = FALSE
      #go through every column
      for(j in 1:ncol(tempdf)){
        #if > 1sd from mean then flag trigger flag
        if(abs( tempdf[i,j] - sigdata_mean[j] ) > coeff*sigdata_sd[j]){
          flag = TRUE
          normpredmat[i,j] = TRUE
        } 
      }
      normpred[i]=flag
      
    }
    return(normpred)
  }

############################################   
## Do one-sided moving average
  tempdf <- dataset3
  windowsize <- 4
  coeff <- 2.9
  
  MADataset <- data.frame(apply(tempdf[,2:32], 2, EMA, n=windowsize)) #Apply Moving Average
  for (i in 1:windowsize-1){
    for (j in 1:31){
      MADataset[i,j] = MADataset[windowsize,j] #above results it high variation, so i just put all as the mean of the 1st rolling avg
    }
  }
  MADataset <- subset(MADataset, select=-c(FLOW_PU5,FLOW_PU9)) #Remove filtered rows
  
  MAPred <- normalmethod(MADataset) #Run dataset through function

############################################ 
##Merge RF with Norm Dist
  temp <- MAPred
  for(i in 1:length(RFPred)){
    if(RFPred[i] == "True"){
      temp[i] = TRUE #we assume RF is 100% correct
    }
  }

############################################ 
##Merge seperate TRUE blocks together
  size <- 40
  i = 1
  while(i < length(temp)-200){
    if(temp[i] == TRUE && temp[i+1]== TRUE){
      temp_2 = temp[(i+2):(i+size)]
      
      flag = FALSE
      for (j in 1:length(temp_2)){
        if (temp_2[j] == TRUE){
          flag = TRUE
        }
      }
      
      if (flag == TRUE){
        for(j in i:(i+size)){
          temp[j] = TRUE
        }
        i = i + size
      }
      else{
        temp[i] = FALSE
        temp[i+1] = FALSE
        i = i+1
      }
    }
    i = i + 1
  }

############################################ 
## Create dataframe and Output to CSV
write.csv(data.frame(DATETIME=dataset3$DATETIME, ATT_FLAG = temp),"Team17_output.csv",row.names = FALSE)


