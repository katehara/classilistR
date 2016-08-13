#'  Export Data if valid and Launch Classilist application
#'
#' @param dataSet classified data having features, class probabilities, Target class and Predicted class for each entry
#' @param location - path of classilist Installation (This should contain data directory for classilist to access data)
#' @param features Vector of feature names
#' @param predicted String Column name of predicted class
#' @param actual String Column name of Target class
#' @param probs Vector of Class Probability column names
#' @return modified dataSet if successful, else the original dataSet
#' @export
#' @examples
#' classilist(data , "/home/user/abc.csv" , c("column1" , "column2" , ..), "predicted_column",
#'      "actual_column" , c("class1" , "class2" , ... ))



classilist <- function(dataSet , location , features  , predicted , actual , probs){
  colN <- names(dataSet)
  num = validateClasses(unique(dataSet[[actual]]) , probs)
  if(num == 1000){
    names(dataSet) <- gsub(x = colN, pattern = "\\.", replacement = " ")
    num = 1
  }
  
  if(validateColumns(colN , features  , predicted , actual , probs) & validateFile(location) & num == 1){
    
    for(i in 1:length(colN)){
      nm <- colN[i]
      if(is.element(nm , features)){
        rep <- paste('F-',nm ,sep='')
        colN[i] <- rep
      }
      
      else if(is.element(nm , probs)){
        rep <- paste('P-',nm ,sep='')
        colN[i] <- rep
      }
      
      else if(nm == actual){
        rep <- paste('A-',nm ,sep='')
        colN[i] <- rep
      }
      
      else if(nm == predicted){
        rep <- "Predicted"
        colN[i] <- rep
      }
      
      else {
        print(paste("Warning : Column", colN[i], "is not specified in any of the categories", sep=" "))
      }
    }
    write.csv(dataSet , file = paste(location , "/data/out.csv" , sep = "") , row.names = FALSE) 
    browseURL(url = paste(location , "/index.html" , sep = ""), browser = getOption("browser"),encodeIfNeeded = FALSE)
  } 
  
  else {
    print("Error : Process Terminated Unexpetedly due to one or more Reasons.")
  }
  return(dataSet)
}



