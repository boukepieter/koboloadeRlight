#' @name kobo_xlsform_relabel
#' @rdname kobo_xlsform_relabel
#' @title  Relabel XLSform based on an original dataframe
#'
#' @description Match labels from an XLSform created with the kobo_to_xlsform function with a prexisting XLSform.
#' It can then be used to match and relabel cleaned datasets with original forms.
#'
#'
#' @param new_form The full filename of the form to be relabeled (xls or xlsx file).
#' @param original_form The full filename of the form to match the assessed form.
#'
#' @author Elliott Messeiller
#'
#' @examples
#' data(iris)
#' str(iris)
#' kobo_to_xlsform(iris)
#'
#' @export kobo_xlsform_relabel


kobo_xlsform_relabel <- function(new_form, original_form, name_form = "form.xls"){
  new_name <- read_excel(new_form, sheet = "survey")[,"name"]
  original_survey <- read_excel(original_form, sheet = "survey")

  joined_survey <- as.data.frame(left_join(new_name, original_survey, by = "name"))

  choices_names <- filter(joined_survey, str_detect(type, "select_one|select_multiple"))
  choices_lists <- unlist(str_split(choices_names$type, " "))
  choices_lists <- unique(choices_lists[!(choices_lists %in% c("select_one", "select_multiple"))])

  original_choices <- read_excel(original_form, sheet = "choices")
  joined_choices <- as.data.frame(filter(original_choices, list_name %in% choices_lists))

  if(sum(unique(joined_choices$list_name) %in% unique(choices_lists)) < length(choices_lists)){
    warning("Some questions might be missing from the form. Please check that all columns from the dataframe have a corresponding question in the form.")
  }

  wb <- createWorkbook(type = "xls")
  sheetname <- "survey"
  surveySheet <- createSheet(wb, sheetname)
  addDataFrame(joined_survey, surveySheet, col.names=TRUE, row.names=FALSE)

  sheetname <- "choices"
  choicesSheet <- createSheet(wb, sheetName=sheetname)
  addDataFrame(joined_choices, choicesSheet, col.names=TRUE, row.names=FALSE)


  mainDir <- gsub("/inst/shiny_app", "",  getwd())
  form_tmp <- paste(mainDir, name_form, sep = "/", collapse = "/")

  if (file.exists(form_tmp)) file.remove(form_tmp)
  saveWorkbook(wb, form_tmp)


}
NULL
