#' @name kobo_label
#' @rdname kobo_label
#' @title  Label Variable
#'
#' @description Insert the full label in data frame based on dictionnary
#'
#'
#' @param data Data where labels must be inserted
#' @param dico ( generated from kobo_dico)
#'
#' @return A "data.table" with the full data.label, and choices labels. To be used for graphs generation.
#'
#' @author Edouard Legoupil
#'
#' @export kobo_label
#'

kobo_label <- function(data, dico) {
  data.label <- as.data.frame(names(data))
  names(data.label)[1] <- "name"
  data.label <- join (x=data.label, y=dico, by="name", type="left" )

  for (i in 1:nrow(data.label)) {
    attributes(data)$variable.labels[ i] <- as.character(data.label[ i, c("label")])

    if(data.label$type[i] %in% c("select_one", "select_multiple_d")){
      variablename <- data.label$name[i]
      variableLabel <- as.character(data.label[ i, c("label")])
      listName <- as.character(data.label[data.label$name == variablename, "listname"])
      choicesLabel <- unlist(dico[dico$listname == listName & dico$formpart == "answers", "label"])
      choicesName <- unlist(dico[dico$listname == listName & dico$formpart == "answers", "name"])
      data[[variablename]] <- mapvalues(data[[variablename]], from = as.character(choicesName), to = as.character(choicesLabel), warn_missing = FALSE)
    }
  }
  test <- data.label[ !(is.na(data.label$name)), ]
  if (nrow(data.label) > nrow(test)) {
    cat (paste0("you have ",nrow(data.label), " variables in you frame but only ",nrow(test) ," were relabeled.\n"))
    cat(" You may double check that the form and the data are matching \n")
    cat("Double check as well that you did download the data with the correct header (i.e. full path with point delimiters) \n")
  }
  return(data)
}
