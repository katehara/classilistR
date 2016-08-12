#'  Validates all column categories
#'
#' @param features Vector of feature names
#' @param predicted String Column name of predicted class
#' @param actual String Column name of Target class
#' @param probs Vector of Class Probability column names
#' @return 
#' True : if the column name format is correct
#' False : otherwise
#' @export
#' @examples
#' validateColumns( c("column1" , "column2" , ..), "predicted_column","actual_column" , c("class1" , "class2" , ... ))

validateColumns <- function(coln , features  , predicted , actual , probs){
  count = 0;
  if(!is.character(features) | !is.vector(features)) {
    count <- count +1
    print("Error : Features Argument should have column names in STRING VECTOR format")
  }
  if(!is.character(predicted)) {
    count <- count +1
    print("Error : Predicted Argument should have column name in STRING format")
  }
  if(!is.character(actual)) {
    count <- count +1
    print("Error : Actual Argument should have column name in STRING format")
  }
  if(!is.character(probs) | !is.vector(features)) {
    count <- count +1
    print("Error : Probability Argument should have column names in STRING VECTOR format")
  }
  
  if(!all(features %in% coln)){
    count <- count+1
    print("Error : One or more features don't exist in Dataset.")
  }
  
  if(!all(predicted %in% coln)){
    count <- count+1
    print("Error : Predicted Column don't exist in Dataset.")
  }
  
  if(!all(actual %in% coln)){
    count <- count+1
    print("Error : Target Column don't exist in Dataset.")
  }
  
  if(!all(probs %in% coln)){
    count <- count+1
    print("Error : One or more Class Probabilities don't exist in Dataset.")
  }
  
  if(length(intersect(features , probs)) != 0){
    count <- count+1
    print("Error : Repeating values in Features and Probability Column Lists.")
  }
  
  if(length(intersect(features , actual)) != 0){
    count <- count+1
    print("Error : Target Column can't be a feature.")
  }
  
  if(length(intersect(features , predicted)) != 0){
    count <- count+1
    print("Error : Predicted Column can't be a feature")
  }
  
  if(length(intersect(actual , probs)) != 0){
    count <- count+1
    print("Error : Target can't be a class probability")
  }
  
  if(length(intersect(predicted , probs)) != 0){
    count <- count+1
    print("Error : Predicted value can't be a class probability")
  }
  
  if(length(intersect(actual , predicted)) != 0){
    count <- count+1
    print("Error : Target and predicted columns must be different")
  }

  if(count != 0) return(FALSE)
  
  return(TRUE)
}