% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo_api.R
\name{kobo_form}
\alias{kobo_form}
\title{Download form from the platform}
\usage{
kobo_form(formid, user, pwd,
  api = "https://kobo.humanitarianresponse.info")
}
\arguments{
\item{formid}{The ID of the form to be accessed (as a character string).}

\item{user}{Optional. A single string indicating the username}

\item{pwd}{Optional. A single string indicating the password}

\item{api}{The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"}
}
\value{
Downloaded form path.
}
\description{
Download form from the platform
}
\author{
Elliott Messeiller
}
