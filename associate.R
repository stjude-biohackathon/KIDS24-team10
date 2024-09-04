
#' Associate: Compare two variables based on their type
#'
#' `associate()` function shows the statistical relationship between two variables/features
#'  with a narrative, plots and references.
#'
#' @param forms The formula that takes the 2 variables.
#' @param data The data set name that contains the 2 variables.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param y.name The 'y' variable name in the formula. Default value is NULL. #ask_Stan
#' @param x.name The 'x' variable name in the formula. Default value is NULL. #ask_Stan
#' @param clr The color(s) in the plot(s). Default value is 'black'.
#' @param line The number of lines in the result. Default value is 1. #ask_Stan
#'
#' @return returns the statistical relationship between the two variables with a narrative, plots and references.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' associate(len~supp, data_frame)
#' associate(len~dose, data_frame)
#'
associate <- function(forms, data,
                      txt= 1, tbl= 1,
                      fig= 1, y.name= NULL,
                      x.name= NULL, clr= NULL,
                      line= 1, rpt=F)
{
  ### get the variables from formula

  #if(class(data) != "data.frame"){ #TODO: delete if below code works
  if(is(data,"data.frame")){
    data <- data.frame(data)
  }

  form_vars <- get.vars(forms)

  ### get variable in y
  depend_y <- form_vars$y.var
  y <- data[, depend_y]

  ### get variable in x
  independ_x <- form_vars$x.var
  x <- data[, independ_x]

  ## if both x and y variable are numerical, then use correlate()

  if ((class(x)[1] %in% c("numeric", "double", "integer")) &&
      (class(y)[1] %in% c("numeric", "double", "integer"))) {

    #print("use correlate method")
    res <- correlate(forms, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, x.name= NULL, clr= "red", line= line)
    class(res) <- "SBP.result"
    return (res)
  }
  ## if both of the variables are not numerical, use compare()
  else {

    #print("use compare method")
    res <- compare(forms, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, grp.name= NULL, clr= "rainbow")
    class(res) <- "SBP.result"
    return (res)

  }

}







