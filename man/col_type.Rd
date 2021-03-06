% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo_Qtypes.R
\name{col_type}
\alias{col_type}
\title{Check the type of question}
\usage{
col_type(df, column, num_fact = 100)
}
\arguments{
\item{df}{The dataframe object to be processed}

\item{column}{Column/question name to be checked, as string}
}
\value{
"select_one", "select_multiple" or "decimal" depending on the type of question.
}
\description{
Check the type of question
}
\author{
Elliott Messeiller
#'
}
