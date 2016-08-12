#'  Validates File name and path
#'  
#' @param fileName String filename for validation
#' @export
#' @return 
#' False : if path is invalid
#' True : if path exists 
#' @examples
#' validateFile("/home/user/classilist/data")

validateFile <- function(filename){
  a<-nchar(filename)
  if(file.exists(filename)){
      return(TRUE)
  }
  else{
      print(paste("Error : Directory" , filename , "Doesn't Exist" , sep = " ") )
      return (FALSE)
  }
}
