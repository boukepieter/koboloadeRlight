#' @name is_selectmultiple
#' @rdname is_selectmultiple
#' @title  Check that question is select_multiple as definied by XLSform
#' @param df The dataframe object to be processed
#' @param column Column/question name to be checked, as string
#' @return TRUE is question validates, FALSE otherwise
#'
#' @export is_selectmultiple

is_selectmultiple <- function(df, column){
  df <- as.data.frame(df)
  col_index <- match(column, names(df))
  if(is.na(col_index)){stop("Column not in df. Check spelling")}

  if(col_index < ncol(df)){
    if(grepl("\\/", names(df)[col_index+1]) && grepl(names(df)[col_index], names(df)[col_index+1])&&
       (sum(as.numeric(df[,col_index+1]), na.rm = TRUE)/nrow(df)<=1)){
      return(TRUE)
    } else{return(FALSE)}
  }else{
    return(FALSE)
  }
}
#' @name is_selectmultiple_choices
#' @rdname is_selectmultiple_choices
#' @title  Check that question is is_selectmultiple_choices as definied by XLSform

#' @param df The dataframe object to be processed
#' @param column Column/question name to be checked, as string
#' @return TRUE is question validates, FALSE otherwise

#'
#' @author Elliott Messeiller
#'#'
#' @export is_selectmultiple_choices

is_selectmultiple_choices <- function(df, column){
  df <- as.data.frame(df)
  col_index <- match(column, names(df))
  if(is.na(col_index)){stop("Column not in df. Check spelling")}
  if(grepl("\\/", names(df)[col_index]) && sum(grepl(paste0("^",strsplit(column, "\\/")[[1]][1], "$"), names(df)))==1&&
     (sum(as.numeric(df[,col_index]), na.rm = TRUE)/nrow(df)<=1)){
    return(TRUE)
  }else{return(FALSE)}
}
#' @name is_selectone
#' @rdname is_selectone
#' @title  Check that question is is_selectone as definied by XLSform

#' @param df The dataframe object to be processed
#' @param column Column/question name to be checked, as string
#' @return TRUE is question validates, FALSE otherwise

#'
#' @author Elliott Messeiller
#'#'
#' @export is_selectone


is_selectone <- function(df, column, num_fact = 100){
  df <- as.data.frame(df)
  col_index <- match(column, names(df))
  if(is.na(col_index)){stop("Column not in df. Check spelling")}
  if(is_selectmultiple(df, column)== FALSE){
    if(class(df[,col_index][[1]][1])=="character"){
      if(as.integer(nlevels(as.factor(df[,col_index]))) < num_fact){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }else{return(FALSE)}
  }else{return(FALSE)}
}

#' @name is_decimal
#' @rdname is_decimal
#' @title  Check that question is decimal as definied by XLSform

#' @param df The dataframe object to be processed
#' @param column Column/question name to be checked, as string
#' @return TRUE is question validates, FALSE otherwise

#'
#' @author Elliott Messeiller
#'#'
#' @export is_decimal

is_decimal <- function(df, column){
  col_index <- match(column, names(df))
  if(is.na(col_index)){stop("Column not in df. Check spelling")}
  if(is_selectmultiple_choices(df, column)!= TRUE){
    if(sum(is.na(as.numeric(as.character(df[,col_index][[1]])))) <= nrow(df)*0.999){
      return(TRUE)
    }else{return(FALSE)}
  }else{return(FALSE)}
}

#' @name col_type
#' @rdname col_type
#' @title  Check the type of question

#' @param df The dataframe object to be processed
#' @param column Column/question name to be checked, as string
#' @return "select_one", "select_multiple" or "decimal" depending on the type of question.
#'
#' @author Elliott Messeiller
#'#'
#' @export is_decimal

col_type <- function(df, column, num_fact = 100){
  if(is_selectone(df, column, num_fact) == TRUE){
    return("select_one")
  }else if(is_selectmultiple(df, column)== TRUE){
    return("select_multiple")
  }else if(is_decimal(df, column)==TRUE){
    return("decimal")
  }else if(is_selectmultiple_choices(df, column) == TRUE){
    return("choice")
  }else{return(NA)}
}
