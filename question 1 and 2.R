directory <-getwd()
hospital_data<- read.csv(paste(directory,"/","hospital-data.csv",sep=""), as.is = TRUE, header = TRUE)
outcome_data<- read.csv(paste(directory,"/","outcome-of-care-measures.csv",sep=""), as.is = TRUE, header = TRUE)

# 30 day mortality rate for heart attacks

heart_attack <- outcome_data[,11]
correct <- complete.cases(heart_attack)
cleansed <- heart_attack[correct]

hist(cleansed)


# best hospital for a certin outcome

best <- function(state,outcome){
  if(!(state %in% outcome_data$State)){
    result <- "invalid state"
  }
  else if(!(outcome %in% c("heart attack","hearth failure","pneumonia"))){
    result<- "invalid outcome"
  }
  else{
    outcome_index_flags <- c("heart attack"=11, "hearth failure" = 17, "pneumonia" = 23)
    concerned_outcome_index <- outcome_index_flags[outcome]
    
    state_data<- split(outcome_data,outcome_data$State)
    concerned_state_data <- state_data[[state]]
    concerned_state_data<- concerned_state_data[ order(concerned_state_data["Hospital.Name"]),]
    
    concerned_cause<- suppressWarnings(as.numeric(concerned_state_data[,concerned_outcome_index]))
    good <- complete.cases((concerned_cause))
    concerned_cause<- concerned_cause[good]
    
    concerned_state_data<- concerned_state_data[good,]
    
    minimum<- min(concerned_cause)
    index<- match(minimum, concerned_cause)
    
    result<- concerned_state_data[index,2]
  }
  result
}




