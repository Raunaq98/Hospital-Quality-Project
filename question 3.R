dir<- getwd()
data<- read.csv(paste(dir,"/","outcome-of-care-measures.csv",sep=""),as.is=TRUE, header = TRUE)

rankhospital<- function( state, outcome, num = "best"){
  if(!(state %in% data$State)){
    result<- "invalid state"
  }
  else if(!(outcome %in% c("heart attack","heart failure","pneumonia"))){
    result<- "invalid outcome"
  }
  else {
    all_states <- split(data,data$State)
    my_state<- all_states[[state]]
    
    order <- order(my_state["Hospital.Name"])
    my_state<- my_state[order,]
    
    flag2<- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    indices<- flag2[state]
    my_outcome <- as.numeric(my_state[,indices])
    all <- complete.cases(my_outcome)
                        
  }
}

