#'  Matches Probability Columns with Classes 
#'
#' @param classes from Target Column of data set
#' @param probs Vector of Class Probability column names
#' @return 
#' True : if all classes have probabilities
#' False : otherwise
#' @export
#' @examples
#' validateClasses( c("class1" , "class2" , ..), c("probClass1" , "probClass2"))

validateClasses <- function(classes , probs){
  count = 0;
  if(length(classes) > length(probs)){
    count <- count+1
    print("Error : Probability of one or more classes missing from Data set")
  }
  
  if(length(classes) < length(probs)){
    count <- count+1
    print("Error : One or more classes(whose probaility is given) missing from Data set")
  }
  
  if(length(classes) == length(probs)){
    if(all(classes != probs)){
      probs <- gsub(x = probs, pattern = "\\.", replacement = " ")
      if(all(classes != probs)){
        count <- count+1
        print("Error : Class Probability Columns must be named same as class name for accurate mapping")
        print("Try to read the data with option \"check.names=FALSE\" to avoid Dots in headers.")
      }
      else return(1000)
    }
  }
  if(count != 0) return(0)
  
  return(1)
}