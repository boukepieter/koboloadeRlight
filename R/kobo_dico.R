#' @name kobo_dico
#' @rdname kobo_dico
#' @title  Data dictionnary
#'
#' @description  Produce a data dictionnary based on the xlsform for the project
#'
#' @param form The full filename of the form to be accessed (xls or xlsx file).
#' It is assumed that the form is stored in the working folder.
#' @param dicofile Name of the csv file to be created. It must end with ".csv". Path to another folder in the working directory can be inserted.
#'
#'
#' @return Write a csv file in the working directory with the name given in the dicofile argument. Path to another folder in the working directory can be inserted in the dicofile argument.
#'
#' @author Edouard Legoupil, Elliott Messeiller
#'
#'
#' @export kobo_dico
#'

kobo_dico <- function(form, dicofile = "dico.csv") {

  if(grepl("*?.csv$", dicofile)==FALSE){stop("Must end in '.csv'")}
  form_tmp <- paste(form, sep = "/", collapse = "/")


  ### First review all questions from survey sheet #################################################
  survey <- read_excel(form_tmp, sheet = "survey")

  ## Rename the variable label
  names(survey)[names(survey) == "label::English"] <- "label"
  names(survey)[names(survey) == "label::english"] <- "label"
  cat("Checking now for additional information within your xlsform. Note that you can insert them in the xls and re-run the function! \n \n ")



  ### add column if not present #################################################
  if ("disaggregation" %in% colnames(survey))
  { } else{
    survey$disaggregation <- ""}


  ## Avoid columns without names
  survey <- survey[ ,c("type",   "name" ,  "label",
                       "disaggregation"
  )]

  ## need to delete empty rows from the form
  survey <- as.data.frame(survey[!is.na(survey$type), ])


  ### We can now extract the id of the list name to reconstruct the full label fo rthe question
  survey$listname <- ""


  ## Extract for select_one
  survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,
                                         paste0( substr(survey$type , (regexpr("select_one", survey$type , ignore.case=FALSE, fixed=TRUE))+10,250)),survey$listname))

  survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),survey$type))

  ## Extract for select multiple & clean type field
  survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                                          paste0( substr(survey$type , (regexpr("select_multiple", survey$type , ignore.case=FALSE, fixed=TRUE))+16,250)),survey$listname ))


  survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple"),survey$type))

  ## handle case where we have "or_other"
  survey$listname <- with(survey, ifelse(grepl("or_other", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$listname) ,
                                         paste0( substr(survey$listname , 1, (nchar(survey$listname)-8 ))),
                                         survey$listname))

  ## Remove trailing space
  survey$listname <- str_trim(survey$listname)
  survey$label <- str_trim(survey$label)

  ## Now creating full name in order to match with data variables name

  ### identify Repeat questions with nest levels
  cat("\n Be careful! The current function only support 2 levels of nested repeat - for instance household / Case / Individual. \n \n")
  survey$qrepeat <- ""
  for(i in 2:nrow(survey))
  {
    #Check based on repeat type
    if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "")                  {survey[ i, c("qrepeat")]  <- "repeatnest1"}
    else if (survey[ i, c("type")] %in% c("begin repeat","begin_repeat") && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest1")       {survey[ i, c("qrepeat")]  <-  "repeatnest1"}
    else if (!(survey[ i, c("type")] %in% c("end repeat","end_repeat"))  && survey[ i - 1, c("qrepeat")] == "repeatnest2")       {survey[ i, c("qrepeat")]  <-  "repeatnest2"}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest1" )      {survey[ i, c("qrepeat")]  <-  ""}
    else if (survey[ i, c("type")] %in% c("end repeat","end_repeat")     && survey[ i - 1, c("qrepeat")] == "repeatnest2" )      {survey[ i, c("qrepeat")]  <-  "repeatnest1"}

    else   {survey[ i, c("qrepeat")]  <-  ""}
  }

  ### identify Repeat questions
  survey$qrepeatlabel <- "household"
  nestable <- survey[survey$type %in% c("begin_repeat","begin repeat") , c("name","qrepeat","type")]
  nestable$name <- as.character(nestable$name)
  for(i in 2:nrow(survey))
  {
    # Now insert the repeat label based on name
    # i <-230
    if ( survey[ i, c("type")] == "begin repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] !="end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] !="end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "household"}
    else if ( survey[ i, c("type")] == "end repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
    nestabovenum <- as.integer(which(nestable$name == nestabove ) -1)
    survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    ## Sometimes it seems that we get an underscore for type
    else if ( survey[ i, c("type")] == "begin_repeat" )                                                {survey[ i, c("qrepeatlabel")]  <- survey[ i, c("name")]}
    else if ( survey[ i, c("type")] !="end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] !="end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2" )   {survey[ i, c("qrepeatlabel")]  <- survey[ i - 1, c("qrepeatlabel")] }
    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest1")    {survey[ i, c("qrepeatlabel")]  <-  "household"}
    else if ( survey[ i, c("type")] == "end_repeat"   && survey[ i - 1, c("qrepeat")] == "repeatnest2")    { nestabove <- as.character(survey[ i - 1, c("qrepeatlabel")])
    nestabovenum <- as.integer(which(nestable$name == nestabove ) -1)
    survey[ i, c("qrepeatlabel")]  <-  as.character( nestable[ nestabovenum , 1] ) }

    else   {survey[ i, c("qrepeatlabel")]  <-  "household"}
  }

  ### Get question levels in order to match the variable name
  survey$qlevel <- ""
  for(i in 2:nrow(survey))
  {      if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "" )      {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level4"}

    else if (survey[ i, c("type")] == "begin group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}
    else if (survey[ i, c("type")] == "begin_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level5"}

    ## Now end of group

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level1") {survey[ i, c("qlevel")] <- "" }

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level2") {survey[ i, c("qlevel")]  <-  "level1"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level3") {survey[ i, c("qlevel")]  <-  "level2"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level4") {survey[ i, c("qlevel")]  <-  "level3"}

    else if (survey[ i, c("type")] == "end group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}
    else if (survey[ i, c("type")] == "end_group" && survey[ i - 1, c("qlevel")] == "level5") {survey[ i, c("qlevel")]  <-  "level4"}

    else   {survey[ i, c("qlevel")]  <-  survey[ i - 1, c("qlevel")]}
  }

  ### Get question groups in order to match the variable name
  ## Concatenation ofqlevel & qrepeat & type
  survey$type2 <- survey$type
  survey$type2[survey$type2 %in% c("begin_group","begin group","end_group","end group")]
  ## We need to handle situation with both repeat & group
  ## set <- as.data.frame(unique(dico[c("qlevel","qrepeat", "type")]))
  ## So 12 cases to handle

  survey$qgroup <- ""
  for(i in 2:nrow(survey))
  {
    #i <- 54
    #i <- 20
    #survey[ 113, c("qgroup")]
    if (survey[ i, c("qlevel")]  %in% c("level1","level2","level3","level4","level5") &&
        survey[ i, c("qrepeat")] %in% c("", "repeatnest1", "repeatnest2") &&
        !(survey[ i, c("type")]   %in% c("begin_group","begin group","end_group","end group","begin_repeat","begin repeat","end_repeat","end repeat")) )

    {survey[ i, c("qgroup")] <- survey[ i - 1, c("qgroup")]


    } else if (survey[ i, c("qlevel")]   %in% c("level1") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_group","begin group")  )

    {survey[ i, c("qgroup")] <- survey[ i, c("name")]

    } else if (survey[ i, c("qlevel")]   %in% c("level2","level3","level4","level5") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_group","begin group") )

    {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("name")],sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5")  &&
               survey[ i, c("qrepeat")]  %in% c("repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("begin_repeat","begin repeat")   )

    {survey[ i, c("qgroup")] <- paste(survey[ i - 1, c("qgroup")], survey[ i, c("qrepeatlabel")], sep = ".")

    } else if (survey[ i, c("qlevel")]   %in% c("level1","level2","level3","level4","level5") &&
               survey[ i, c("qrepeat")]  %in% c("", "repeatnest1", "repeatnest2") &&
               survey[ i, c("type")]     %in% c("end_group","end group","end_repeat","end repeat") )

    {survey[ i, c("qgroup")] <- substr(survey[ i - 1, c("qgroup")] ,0, regexpr("\\.[^\\.]*$", survey[ i - 1, c("qgroup")] ) - 1)

    } else  {survey[ i, c("qgroup")]  <- ""}
  }



  ## a few colummns to adjust to match questions & choices
  survey$labelchoice <- survey$label
  survey$ordinal <- ""
  survey$score <- ""
  survey$recategorise <- ""



    ####
  #### Now looking at choices --#########################################################################################################
  #rm(choices)
  choices <- read_excel(form_tmp, sheet = "choices")
  names(choices)[names(choices) == "label::English"] <- "label"
  names(choices)[names(choices) == "label::english"] <- "label"
  names(choices)[names(choices) == "list name"] <- "listname"
  names(choices)[names(choices) == "list_name"] <- "listname"

  ## Remove trailing space
  choices$listname <- str_trim(choices$listname)
  choices$label <- str_trim(choices$label)

  if ("ordinal" %in% colnames(choices))
  {
    cat("12 -  Good: You have a column `ordinal` in your `choices` worksheet.\n");
  } else
  {cat("12 -  No column `ordinal` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$ordinal <- ""}

  if ("disaggregation" %in% colnames(choices))
  {
    cat("12 -  Good: You have a column `disaggregation` in your `choices` worksheet.\n");
  } else
  {cat("12 -  No column `disaggregation` in your `choices` worksheet. Creating a dummy one for the moment...\n");
    choices$disaggregation <- ""}


  choices <- choices[,c("listname",  "name",  "label", "ordinal")]
  names(choices)[names(choices) == "label"] <- "labelchoice"
  #rm(choices)
  choices <- plyr::join(x = choices, y = survey, by = "listname")

  choices$type <- with(choices, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_one_d"),choices$type))

  choices$type <- with(choices, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices$type),
                                       paste0("select_multiple_d"),choices$type))


  names(choices)[9] <- "nameq"
  names(choices)[10] <- "labelq"
  choices$labelfull <- paste0(choices$labelchoice)
  choices$qrepeat <- ""
  choices$'qrepeatlabel' <- ""


  #### Now Row bing questions & choices########################################################################################################
  #
  choices2 <- choices[ ,c("type", "name", "labelfull", "disaggregation",
                          "listname", "qrepeat","qrepeatlabel",  "qlevel", "qgroup", "labelchoice",
                          "ordinal")]


  names(choices2)[names(choices2) == "labelfull"] <- "label"


  survey2 <-    survey[,c("type", "name",  "label", "disaggregation",
                          "listname", "qrepeat","qrepeatlabel",  "qlevel",   "qgroup", "labelchoice",
                          "ordinal")]

  survey2$formpart <- "questions"
  choices2$formpart <- "answers"

  dico <- rbind(survey2,choices2)


  ## Remove trailing space
  dico$listname <- str_trim(dico$listname)


  ## Trim long label...
  dico$label <- substring(dico$label, 0, 85)

  ## A few fix on the dico
  dico <- dico[ !is.na(dico$name), ]
  dico <- dico[ !is.na(dico$type), ]

  write_csv(dico,path = dicofile, na = "")

}
NULL

