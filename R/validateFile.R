#'  Validates File name and path
#'  
#' @param fileName String filename for validation
#' @export
#' @return 
#' False : if path is invalid and/or doesn't seems to be correct classilist Installation
#' True : if path exists and is correct Installation Directory for classilist
#' @examples
#' validateFile("/home/user/classilist/")

validateFile <- function(filename){
  if(file.exists(filename)){
    if(file.exists(paste(filename , "/data" , sep = "")) & file.exists(paste(filename , "/index.html" , sep = ""))){
     return(TRUE) 
    }
    else{
      print(paste("Error : Directory" , filename , "doesn't seems to be correct classilist Installation" , sep = " "))
      return(FALSE)
      }
  }
  else{
      print(paste("Error : Directory" , filename , "Doesn't Exist" , sep = " ") )
      return (FALSE)
  }
}
