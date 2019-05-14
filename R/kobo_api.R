#' @name kobo_form
#' @rdname kobo_form
#' @title  Download form from the platform
#'
#' @description Download form from the platform
#'
#' @param formid The ID of the form to be accessed (as a character string).
#' @param user Optional. A single string indicating the username
#' @param pwd Optional. A single string indicating the password
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#'
#' @return Downloaded form path.
#'
#' @author Elliott Messeiller
#'
#' @export kobo_form
#'


kobo_form <- function(formid, user, pwd, api="https://kobo.humanitarianresponse.info") {

  url_form <- paste0(api, "/assets/", formid, "/")
  raw_form <- GET(url_form, authenticate(user, pwd))
  raw_form_text <- content(raw_form, "text", encoding = "UTF-8")
  raw_form_text_json <- fromJSON(raw_form_text)

  languages <- as.vector(raw_form_text_json$content$translations)
  languages_labels <- paste0("label::", languages)
  choices <- as.data.frame(raw_form_text_json$content$choices)
  survey <- as.data.frame(raw_form_text_json$content$survey)
  survey$label <- nullToNA(survey$label)

  if(length(languages)>1){
    choices_labels <- as.data.frame(do.call(rbind, choices$label))
    names(choices_labels) <- languages_labels
    choices <- cbind(choices, choices_labels) %>%
      select(-label)

    survey_labels <- as.data.frame(do.call(rbind, survey$label))
    names(survey_labels) <- languages_labels
    survey <- cbind(survey, survey_labels) %>%
      select(-label)
  }


  wb <- createWorkbook(type = "xls")
  sheetname <- "survey"
  surveySheet <- createSheet(wb, sheetname)
  addDataFrame(survey, surveySheet, col.names=TRUE, row.names=FALSE)

  sheetname <- "choices"
  choicesSheet <- createSheet(wb, sheetName=sheetname)
  addDataFrame(choices, choicesSheet, col.names=TRUE, row.names=FALSE)


  mainDir <- gsub("/inst/shiny_app", "",  getwd())
  form_tmp <- paste(mainDir, paste0(raw_form_text_json$name, ".xls"), sep = "/", collapse = "/")

  if (file.exists(form_tmp)) file.remove(form_tmp)
  saveWorkbook(wb, form_tmp)

  return(form_tmp)
}

#' @name kobo_all_forms
#' @rdname kobo_all_forms
#' @title  Returns a dataframe with all forms available for the user
#'
#' @description Download form from the platform
#'
#' @param user Optional. A single string indicating the username
#' @param pwd Optional. A single string indicating the password
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#'
#' @return Dataframe with all forms available for the user
#'
#' @author Elliott Messeiller
#'
#' @export kobo_all_forms
#'
#'
kobo_all_forms <- function(user, pwd, api = "https://kobo.humanitarianresponse.info"){

  all_forms <- GET(paste0(api, "/assets/"), authenticate(user, pwd))
  all_forms_text <- content(all_forms, "text", encoding = "UTF-8")
  all_forms_text_json <- fromJSON(all_forms_text)

  all_forms_df <-  as.data.frame(all_forms_text_json$results)%>%
    filter(has_deployment == TRUE)%>%
    select(name, uid, date_created, date_modified, deployment__submission_count)

  api <- gsub("kobo.", "kc.", api)
  all_forms_old <- GET("https://kc.humanitarianresponse.info/api/v1/data", authenticate(user, pwd))
  all_forms_old <- content(all_forms_old, "text", encoding = "UTF-8")
  all_forms_old <- fromJSON(all_forms_old)
  all_forms_old <- as.data.frame(all_forms_old)%>%
    mutate(old_id = id)%>%
    select(old_id, id_string)

  all_forms_df <- left_join(all_forms_df, all_forms_old, by = c("uid" = "id_string"))
  names(all_forms_df)[6] <- "submission_count"


  return(all_forms_df)
}

#' @name kobo_data
#' @rdname kobo_data
#' @title  Download data from the platform
#'
#' @description Download data from the platform
#'
#' @param formid The ID of the form to be accessed (as a character string). It must in the API V1 format: see kobo_all_forms old_id output.
#' @param user Optional. A single string indicating the username
#' @param pwd Optional. A single string indicating the password
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#' @return The file path where the csv file was saved.
#'
#' @author Elliott Messeiller
#'
#' @export kobo_data
#'


kobo_data <- function(formid, user, pwd, api="https://kobo.humanitarianresponse.info") {
  api <- gsub("kobo", "kc", api)
  url_data <- paste0(api, "/api/v1/data/",formid, ".csv")
  raw_data <- GET(url_data, authenticate(user, pwd))
  raw_data <- content(raw_data, "raw", encoding = "UTF-8")
  raw_data <- read_csv(raw_data)

  return(raw_data)


}
