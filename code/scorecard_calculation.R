#Takes two dataframes and calculate scores for intervals specified in the second one.

scorecard_calculation <- function(data, df_scorecard){ 
  if(!is.data.frame(data)) stop (" data must be a data.frame")
  if((!is.numeric(data[,1]) || !is.integer(data[,1])) || ncol(data) >1) stop ("data must have one column, as numeric or integer")
  if(!is.data.frame(df_scorecard)) stop (" df_scorecard must be a data.frame")
  if(!is.character(df_scorecard[,1])) stop("First  column df_scorecard must be text")
  if(!is.numeric(df_scorecard[,2]) || !is.integer(df_scorecard[,2])) stop("Second column df_scorecard must be numeric or integer")
  if(sum(df_scorecard[,3] %in% c("==", ">", "<", ">=", "<="))<nrow(df_scorecard)) stop ("Third column most specify how to treat the target")
  
  result <- list(rep(0, 100))
  lapply()                     
  for (j in 1:nrow(data)){
    for (i in 1:nrow(df_scorecard)){
      
      result[i] <- ifelse(eval(call(df_scorecard[i,3],as.numeric(data[j,1]), as.numeric(as.character(df_scorecard[i,2])))),df_scorecard[i,1],NA)
    }
  }
  
  return(data.frame(score = c(1:nrow(data))))
  
}
