#' @name kobo_to_xlsform
#' @rdname kobo_to_xlsform
#' @title  Generate xlsfrom skeleton from a dataframe
#'
#' @description Creates and save a xlsform skeleton from a data.frames in your data folder
#' The form.xls will be saved in the data folder of your project.
#' The generated xlsfrom will need to be manually edited to configure your analysis
#'
#' Note that this function only works with \code{data.frames}. The function
#' will throw an error for any other object types.
#'
#' @param df The dataframe object to be processed.
#' @param form The full filename of the form to be accessed (xls or xlsx file). It is assumed that the form is stored in the working directory, another folder path can be inserted here.
#' @param num_fact number of levels for a factor to be considered as a text.
#'
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @export kobo_to_xlsform
#'

kobo_to_xlsform <- function(df,form = "form.xls",
                            num_fact=8) {

  df <- as.data.frame(df)
  stopifnot(is.data.frame(df))

  ## build survey sheet
  survey <- data.frame( type = rep(as.character(NA), ncol(df)),
                        name = names(df),
                        label = names(df),
                        disaggregation = rep(as.character(NA), ncol(df)),
                        stringsAsFactors = FALSE)




  type  <- data.frame()
  for (i in names(df)) {
    type[i, "type"] <- col_type(df, i, num_fact)
    type[i, "name"] <- i
    if(type[i,"type"] == "select_one"){ df[,i] <- as.factor(df[,i])}
  }
  type <- type[!is.na(type$type),]
  choices_all <- type[type$type == "choice",]
  type_select <- type[type$type != "choice",]

  choices_split <- str_split(choices_all$name, "\\/", simplify = TRUE)
  if(ncol(choices_split)>3){stop("You probably have a forward slash in a choice (e.g. 'N/A'). Please remove it from the raw data.")}
  choices_one <- stack(plyr::compact(lapply(df, levels)))
  names(choices_one) <- c("label", "list_name")
  choices_one$order <- NA
  choices_one$name <- choices_one$label

  choices <- as.data.frame(cbind(choices_split, choices_split[,2]))
  choices$order <- NA
  names(choices) <- c("list_name", "name", "label", "order")
  choices <- full_join(choices, choices_one)

  choices <- choices[rowSums(is.na(choices)) != ncol(choices),]

  survey <- left_join(type_select, survey, by = c("type", "name"))
  survey$type <- ifelse(survey$type %in% c("select_one", "select_multiple"), paste(survey$type, survey$name), survey$type)
  survey$label <- survey$name
  survey <- survey[rowSums(is.na(survey)) != ncol(survey),]


  #create begin_groups

  wb <- createWorkbook(type = "xls")
  sheetname <- "survey"
  surveySheet <- createSheet(wb, sheetname)
  addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE)

  sheetname <- "choices"
  choicesSheet <- createSheet(wb, sheetName=sheetname)
  addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)


  mainDir <- gsub("/inst/shiny_app", "",  getwd())
  form_tmp <- paste(mainDir, form, sep = "/", collapse = "/")

  if (file.exists(form_tmp)) file.remove(form_tmp)
  saveWorkbook(wb, form_tmp)
}
NULL
