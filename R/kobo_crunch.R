#' @name kobo_crunching_all
#' @rdname kobo_crunching_all
#' @title  Crunch all questions in the dictionnary
#' @description Run kobo_crunching on all the questions present in the dictionnary and the dataframe.
#'
#' @param data Dataframe containing data to analyse
#' @param dicofile Name of the file containing the dictionnary. "dico.csv" is used by default.
#'
#' @return Returns a list containing four to five named elements:
#' \itemize{
#'   \item variable : The name of the question (variable) analysed
#'   \item plot: A ggplot2 object with a basic bar or density graph depending on the question
#'   \item freq_tbl: A frequency table of the question of the question
#'   \item summary: summary statistics (quartiles and mean). Only for decimal/integer questions.
#'   \item other: Other information on the question: # respondents answered the form, # and % of respondents to the question.
#'}
#'
#' @export kobo_crunching_all
#'
kobo_crunching_all <- function(data, dicofile = "dico.csv") {

  dico <- read.csv(dicofile)
  all_questions <- as.character(dico[dico$formpart == "questions" & dico$name %in% names(data), "name"])

  all_questions_result <- all_questions %>%
    map(kobo_crunching, data = data, dicofile = dicofile)

  names(all_questions_result) <- all_questions

  return(all_questions_result)

}

NULL


#' @name kobo_crunching
#' @rdname kobo_crunching
#' @title Pick and run analysis function for question type.
#' @description This function pick and run the right analysis function depending on the type of question for data crunching.
#'
#' @param question Question to be treated by the function. It should be the "name" of the  questionas in the dictionnary or XLSform.
#' @param data Dataframe containing data to analyse
#' @param dicofile Name of the file containing the dictionnary. "dico.csv" is used by default.
#'
#' @return Returns a list containing four to five named elements:
#' \itemize{
#'   \item variable : The name of the question (variable) analysed
#'   \item plot: A ggplot2 object with a basic bar or density graph depending on the question
#'   \item freq_tbl: A frequency table of the question of the question
#'   \item summary: summary statistics (quartiles and mean). Only for decimal/integer questions.
#'   \item other: Other information on the question: # respondents answered the form, # and % of respondents to the question.
#'}
#'
#' @export kobo_crunching
#'
kobo_crunching <- function(question, data, dicofile = "dico.csv") {

  if (is.na(question) == TRUE | is.character(question) == FALSE) stop("Please enter a question.")

  dico <- read.csv(dicofile)
  # Select question in dico
  selectquestion <- filter(dico, name == question)
  qtype <- as.character(selectquestion$type)

  if (qtype == "select_one") {
    result <- kobo_bar_one_indiv(data, question, dicofile)
  }else if (qtype == "select_multiple") {
    result <- kobo_bar_multi_indiv(data, question, dicofile)
  }else if (qtype == "integer" | qtype == "decimal" | qtype == "calculate") {
    result <- kobo_histo_indiv(data, question, dicofile)
  }else {
    result <- list(question, "Question not treatable", NULL, NULL)
  }

  return(result)
}

NULL

#' @name kobo_bar_one_indiv
#' @rdname kobo_bar_one_indiv
#' @title  Generate bar Chart for select_one questions
#' @description Generate basic data exploration analysis for kobo select_one question.
#'
#' @param data Dataframe containing data to analyse
#' @param question Question to be treated by the function. It should be the "name" of the question as in the dictionnary or XLSform.
#' The question has to be a select_one question in the dictionary.
#' @param dicofile Name of the file containing the dictionnary. "dico.csv" is used by default.
#'
#' @return Returns a list containing four elements:
#' \itemize{
#'   \item variable : The name of the question (variable) analysed
#'   \item plot: A ggplot2 object with a basic bar or density graph depending on the question
#'   \item freq_tbl: A frequency table of the question of the question
#'   \item other: Other information on the question: # respondents answered the form, # and % of respondents to the question.
#'}
#'
#' @author Edouard Legoupil, Elliott Messeiller

#' @export kobo_bar_one_indiv
#'
kobo_bar_one_indiv <- function(data, question, dicofile = "dico.csv") {

  dico <- read.csv(dicofile)
  # Select question in dico
  selectquestion <- filter(dico, name == question)
  # Checks

  ## Check that variable are in the dataset
  if (nrow(selectquestion) != 1) stop("Question not in dictionnary", call. = FALSE)
  ## Check that it is a select_one question
  if ((selectquestion$type %in% c("select_one")) == FALSE) stop("Not a select_one question")

  ## Select disaggregation if exists
  selectfacet <- as.character(selectquestion$disaggregation)
  if(selectfacet %in% names(data) == FALSE) stop ("Disaggregation not in columns. Please enter a column as disaggregation")

  ## Getting choices
  selectlistname <- as.character(selectquestion$listname)
  selectchoices <- dico[dico$type  == "select_one_d" & dico$listname == selectlistname, c("listname", "name", "labelchoice")]

  ## Matching data from question
  dataquestion <- data[question]

  ## Checking that question was answered

  if (sum(is.na(dataquestion)) == nrow(dataquestion)) {
    stop("No answer to question")
  }

  ## Labeling dataframe
  dataquestion <- kobo_label(dataquestion, dico)

  ## Replacing empty cells by NAs
  # dataquestion[,dataquestion==""]<-NA

  ### Now let's create bar graphs
  variablename <- as.character(names(dataquestion))
  title <- attributes(dataquestion)$variable.labels
  ordinal <- as.character(selectquestion$ordinal)

  if (is.na(selectfacet) == F && selectfacet != "") {
    dataquestion <- cbind(dataquestion, data[selectfacet])

  }

  frequ <- data.frame(table(dataquestion))

  if (ncol(frequ) == 2) {
    names(frequ) <- c("Var1", "Freq")
  } else {
    names(frequ) <- c("Var1", "facet", "Freq")
    frequ$facet <- as.character(frequ$facet)
    listname_f <- as.character(dico[dico$name == selectfacet & dico$formpart == "questions", "listname" ])
    choices_f <- unique(dico[dico$listname == listname_f & dico$formpart == "answers", c("name", "label")])
    choices_f$name <- as.character(choices_f$name)
    frequ <- frequ %>%
      left_join(choices_f, by = c("facet"="name"))%>%
      select(Var1, label, Freq)
    frequ <- droplevels(frequ)
    names(frequ) <- c("Var1", "facet", "Freq")
  }

  frequ$freqper <- as.numeric(frequ$Freq / (sum(!is.na(dataquestion[1]))))
  frequ$Var1 <- str_wrap(frequ$Var1, width = 15)

  totalanswer <- nrow(dataquestion)

  count_replied <- (sum(!is.na(dataquestion[1])))

  percentresponse <- paste(round((count_replied / totalanswer * 100), digits = 2), "%", sep = "")

  frequ$Var1 <- as.factor(frequ$Var1)

  if (is.na(ordinal) == T) {
    frequ$Var1 <- factor(frequ$Var1, levels = unique(frequ$Var1[order(frequ$freqper)]))
  } else {
    ordinal_choices <- as.character(selectchoices[, c("labelchoice")])
    frequ$Var1 <- gdata::reorder.factor(frequ$Var1, new.order = ordinal_choices)
    frequ %>% arrange(Var1)
  }

  color <- "#2a87c8"
  background_rect <- data.frame(unique(frequ[, c("Var1")]))
  names(background_rect) <- c("Var1")
  background_rect$freqper <- 1

  theme_set(theme_gray(base_size = 10))


  ## and now the graph
  plotfreq <- ggplot(frequ, aes(x = Var1, y = freqper))
  if ("facet" %in% colnames(frequ)) {
    plotfreq <- plotfreq +
      geom_bar(data = background_rect, aes(x = Var1), stat = "identity", alpha = 0.2) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = facet)) +
      geom_text(aes(label = paste(round(freqper * 100), "%", sep = ""), fill = facet, hjust = -0.5), position = position_dodge(width = 0.8))
  } else {
    plotfreq <- plotfreq +
      geom_bar(fill = color, colour = color, stat = "identity") +
      geom_text(aes(label = paste(round(frequ$freqper * 100), "%", sep = ""), hjust = -0.5))
  }
  plotfreq <- plotfreq +
    scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
    scale_fill_brewer(palette = "PuBu", name = "Disaggregation") +
    labs(x = "", y = "")+
    coord_flip() +
    labs(x = "", y = "")+
    ggtitle(str_wrap(title, width = 50)) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )



  ## Formating the frequ table
  frequ_nice <- frequ
  frequ_nice$freqper <- round(frequ$freqper * 100, 2)

  if(ncol(frequ_nice) == 4){
    names(frequ_nice) <- c("Choices", "Disaggregation", "# answered", "% answered")
  }else{
  names(frequ_nice) <- c("Choices", "# answered", "% answered")
  }
  frequ_nice$Choices <- gsub(pattern = "\\n", " ", frequ_nice$Choices)

  other_info <- data.frame(
    title = c("# Answered form", "# Answered question", "% Answered question"),
    info = c(totalanswer, count_replied, percentresponse)
  )

  ## Saving outputs to a list
  out_indiv <- list(variable = variablename, plot = plotfreq, freq_tbl = frequ_nice, other = other_info)

  return(out_indiv)
}
NULL

#' @name kobo_bar_multi_indiv
#' @rdname kobo_bar_multi_indiv
#' @title  Generate bar Chart for select_multiple question

#' @description Generate basic data exploration analysis for kobo select_multiple question.
#'
#' @param data Dataframe containing data to analyse
#' @param question Question to be treated by the function. It should be the "name" of the questions as in the dictionnary or XLSform.
#' The question has to be a select_multiple question in the dictionary.
#' @param dicofile Name of the file containing the dictionnary. "dico.csv" is used by default.
#'
#' @return Returns a list containing four elements:
#' \itemize{
#'   \item variable : The name of the question (variable) analysed
#'   \item plot: A ggplot2 object with a basic bar or density graph depending on the question
#'   \item freq_tbl: A frequency table of the question of the question
#'   \item other: Other information on the question: # respondents answered the form, # and % of respondents to the question.
#'}
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#'
kobo_bar_multi_indiv <- function(data, question, dicofile = "dico.csv") {

  dico <- read.csv(dicofile)

  # Select question in dico
  selectquestion <- filter(dico, name == question)

  # Checks

  ## Check that variable are in the dataset
  if (nrow(selectquestion) != 1) stop("Question not in dictionnary", call. = FALSE)
  ## Check that it is a select_one question
  if (selectquestion$type != "select_multiple") stop(paste0("Not a select_multiple question. Question type in dico for this question : ", selectquestion$type))

  ## Select disaggregation if exists
  if(selectfacet %in% names(data) == FALSE) stop ("Disaggregation not in columns. Please enter a column as disaggregation")

  ## Getting choices
  selectlistname <- as.character(selectquestion$listname)
  selectchoices <- dico[dico$type == "select_multiple_d" & dico$listname == selectlistname, c("listname", "name", "labelchoice")]

  ## Matching data from question and formatting
  dataquestion <- data[, grep(question, names(data))]
  dataquestion <- dataquestion %>%
    select(-question) %>%
    mutate_all(as.numeric)


  ## Checking that question was answered

  if (sum(is.na(dataquestion)) == nrow(dataquestion) * ncol(dataquestion)) { stop("No answer to question") }

  ### Now let's create bar graphs
  variablename <- as.character(selectquestion$name)
  title <- selectquestion$label
  ordinal <- as.character(selectquestion$ordinal)
  # if (usedweight == "sampling_frame") {
  #   ### TO BE FIXED ###
  #   frequ <- data.frame(svytable(~dataquestion, surveydesign))
  # } else {
    if (is.na(selectfacet) == FALSE && selectfacet != "") {
      ### With a disaggregation
      dataquestion <- cbind(dataquestion, data[selectfacet])
      names(dataquestion)[ncol(dataquestion)] <- "facet"

      labelschoices <- selectchoices %>% select(-listname)

      frequ <- dataquestion %>%
        gather(name, freq, -facet) %>%
        dplyr::group_by(facet, name) %>%
        dplyr::summarise(freq = sum(freq, na.rm = TRUE))

      ### Relabel CHOICES

      frequ$name <- gsub(paste0(question, "."), "", frequ$name)
      labelschoices$name <- as.character(labelschoices$name)
      labelschoices$labelchoice <- as.character(labelschoices$labelchoice)
      frequ <- frequ %>%
        left_join(labelschoices, by = "name") %>%
        select(labelchoice, facet, freq)

      names(frequ) <- c("Var1", "facet", "Freq")
      listname_f <- as.character(dico[dico$name == selectfacet & dico$formpart == "questions", "listname" ])
      choices_f <- unique(dico[dico$listname == listname_f & dico$formpart == "answers", c("name", "label")])
      choices_f$name <- as.character(choices_f$name)
      frequ$facet <- as.character(frequ$facet)


      frequ <- frequ %>%
        left_join(choices_f, by = c("facet"="name"))%>%
        ungroup()%>%
        select(Var1, label, Freq)
      frequ <- droplevels(frequ)
      names(frequ) <- c("Var1", "facet", "Freq")


    } else {
      ### Without disaggregation
      labelschoices <- selectchoices %>%
        select(-listname) %>%
        mutate_all(as.character)


      frequ <- dataquestion %>%
        gather(name, freq) %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(freq = sum(freq, na.rm = TRUE))

      frequ$name <- gsub(paste0(question, "."), "", frequ$name)

      #### Relabel CHOICES
      frequ <- frequ %>%
        left_join(labelschoices, by = "name") %>%
        select(labelchoice, freq)
    }
  #}

  if (ncol(frequ) == 2) {
    names(frequ) <- c("Var1", "Freq")
  } else {
    names(frequ) <- c("Var1", "facet", "Freq")
  }

  frequ$freqper <- as.numeric(frequ$Freq / (sum(!is.na(dataquestion[1]))))
  frequ$Var1 <- str_wrap(frequ$Var1, width = 15)

  totalanswer <- nrow(dataquestion)

  count_replied <- (sum(!is.na(dataquestion[1])))

  percentresponse <- paste(round((count_replied / totalanswer * 100), digits = 2), "%", sep = "")

  frequ$Var1 <- as.factor(frequ$Var1)

  if (is.na(ordinal) == T) {
    frequ$Var1 <- factor(frequ$Var1, levels = unique(frequ$Var1[order(frequ$freqper)]))
  } else {
    ordinal_choices <- as.character(selectchoices_questions[selectchoices_questions$qname == variablename, c("labelchoice")])
    frequ$Var1 <- gdata::reorder.factor(frequ$Var1, new.order = ordinal_choices)
    frequ <- frequ %>% arrange(Var1)
  }

  theme_set(theme_gray(base_size = 15))
  color <- "#2a87c8"

  background_rect <- data.frame(unique(frequ[, c("Var1")]))
  names(background_rect) <- c("Var1")
  background_rect$freqper <- 1


  ## and now the graph
  plotfreq <- ggplot(frequ, aes(x = Var1, y = freqper))
  if ("facet" %in% colnames(frequ)) {
    plotfreq <- plotfreq +
      geom_bar(data = background_rect, aes(x = Var1), stat = "identity", alpha = 0.2) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = facet)) +
      geom_text(aes(label = paste(round(freqper * 100), "%", sep = ""), fill = facet, hjust = -0.5), position = position_dodge(width = 0.8))
  } else {
    plotfreq <- plotfreq +
      geom_bar(fill = color, colour = color, stat = "identity") +
      geom_text(aes(label = paste(round(frequ$freqper * 100), "%", sep = ""), hjust = -0.5))
  }
  plotfreq <- plotfreq +
    scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
    scale_fill_brewer(palette = "PuBu", name = "Disaggregation") +
    labs(x = "", y = "")+
    coord_flip() +
    ggtitle(str_wrap(title, width = 50)) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )

  ## Formating the frequ table
  frequ_nice <- frequ
  frequ_nice$freqper <- round(frequ$freqper * 100, 2)
  if(ncol(frequ_nice) == 4){
    names(frequ_nice) <- c("Choices", "Disaggregation", "# answered", "% answered")
  }else{
    names(frequ_nice) <- c("Choices", "# answered", "% answered")
  }
  frequ_nice$Choices <- gsub(pattern = "\\n", " ", frequ_nice$Choices)

  other_info <- data.frame(
    title = c("# Answered form", "# Answered question", "% Answered question"),
                           info = c(totalanswer, count_replied, percentresponse)
                             )

  ## Saving outputs to a list
  out_indiv <- list(variable = variablename, plot = plotfreq, freq_tbl = frequ_nice, other = other_info)
  return(out_indiv)
}
NULL
#' @name kobo_histo_indiv
#' @rdname kobo_histo_indiv
#' @title  Generate histograme for individual integer questions
#'
#' @description  Automatically generate histogrammes for each of the integer questions in the dataset. ggplot2 is used.
#'
#' @param data Dataframe containing data to analyse
#' @param question Question to be treated by the function. It should be the "name" of the questions as in the dictionnary or XLSform.
#' The question has to be a decimal or integer question in the dictionary.
#' @param dicofile Name of the file containing the dictionnary. "dico.csv" is used by default.
#'
#' @return Returns a list containing four elements:
#' \itemize{
#'   \item variable : The name of the question (variable) analysed
#'   \item plot: A ggplot2 object with a basic bar or density graph depending on the question
#'   \item freq_tbl: A frequency table of the question of the question
#'   \item summary: summary statistics (quartiles and mean). Only for decimal/integer questions.
#'   \item other: Other information on the question: # respondents answered the form, # and % of respondents to the question.
#'}
#'
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#' @examples
#' kobo_histo_indiv()
#' @export kobo_histo_indiv
#'
#' @examples
#' \dontrun{
#' kobo_histo_indiv()
#' }
#'
kobo_histo_indiv <- function(data, question, dicofile = "dico.csv") {

  dico <- read.csv(dicofile)

  selectquestion <- filter(dico, name == question)

  # Checks

  ## Check that variable are in the dataset
  if (nrow(selectquestion) != 1) stop("Question not in dictionnary", call. = FALSE)
  ## Check that it is a select_one question
  if ((selectquestion$type %in% c("integer", "decimal", "calculate")) == FALSE) stop("Not a  question")

  ## Select disaggregation if exists
  selectfacet <- as.character(selectquestion$disaggregation)
  if(selectfacet %in% names(data) == FALSE) stop ("Disaggregation not in columns. Please enter a column as disaggregation")

  ## Getting choices
  ## Matching data from question
  dataquestion <- data[question]

  ## Checking that question was answered

  if (sum(is.na(dataquestion)) == nrow(dataquestion)) { stop("No answer to question") }

  ## Labeling dataframe
  dataquestion <- kobo_label(dataquestion, dico)

  ## Replacing empty cells by NAs
  # dataquestion[,dataquestion==""]<-NA

  ### Now let's create bar graphs
  variablename <- as.character(names(dataquestion))
  title <- attributes(dataquestion)$variable.labels
  ordinal <- as.character(dico[dico$name == variablename, c("ordinal")])

  if (is.na(selectfacet) == F && selectfacet != "") {
    dataquestion <- cbind(dataquestion, data[selectfacet])
  }

  #if (usedweight == "sampling_frame") {
    ### TO BE FIXED ###
  #   frequ <- data.frame(svytable(~dataquestion, surveydesign))
  # } else {
  #   frequ <- dataquestion
  # }
frequ <- dataquestion


  if (ncol(frequ) == 1) {
    names(frequ) <- "Var1"
    frequ$Var1 <- as.numeric(as.character(frequ$Var1))
  } else {
    names(frequ) <- c("Var1", "name")
    listname_f <- as.character(dico[dico$name == selectfacet & dico$formpart == "questions", "listname" ])
    choices_f <- unique(dico[dico$listname == listname_f & dico$formpart == "answers", c("name", "label")])
    choices_f$name <- as.character(choices_f$name)
    frequ$name <- as.character(frequ$name)
    frequ <- frequ %>%
      left_join(choices_f, by = "name")%>%
      select(Var1, label)
    frequ <- droplevels(frequ)
    names(frequ) <- c("Var1", "facet")

  }

  totalanswer <- nrow(dataquestion)

  count_replied <- (sum(!is.na(dataquestion[1])))

  percentresponse <- paste(round((count_replied / totalanswer * 100), digits = 2), "%", sep = "")

  color <- "#2a87c8"

  theme_set(theme_gray(base_size = 10))

  ## and now the graph
  plotfreq <- ggplot(frequ, aes(x = Var1))
  if ("facet" %in% colnames(frequ)) {
    plotfreq <- plotfreq +
      geom_density(aes(fill = facet), adjust = 2, alpha = 0.2)
  } else {
    plotfreq <- plotfreq +
      geom_density(adjust = 2, alpha = 0.2)
  }
  plotfreq <- plotfreq +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "PuBu", name = "Disaggregation") +
    labs(x = "", y = "")+
    ggtitle(str_wrap(title, width = 50)) +
    theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )

  ## Formating the frequ table
  frequ_nice <- frequ %>%
    table() %>%
    data.frame() %>%
    mutate(freqper = paste0(round(Freq / sum(Freq) * 100), "%"))


  if(ncol(frequ_nice) == 4){
    names(frequ_nice) <- c("Variable", "Disaggregation", "# answered", "% answered")
  }else{
    names(frequ_nice) <- c("Variable", "# answered", "% answered")
  }

  other_info <- data.frame(
    title = c("# Answered form", "# Answered question", "% Answered question"),
    info = c(totalanswer, count_replied, percentresponse)
  )

  ## Saving outputs to a list
  out_indiv <- list(variable = variablename, plot = plotfreq, freq_tbl = frequ_nice, summary = summary(frequ), other = other_info)
  return(out_indiv)
}
NULL
