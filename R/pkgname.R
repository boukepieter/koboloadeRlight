#' @details
#'
#'  \bold{Prerequisite}\cr
#'      To use koboloadeRlight, you will need at least a \bold{dataframe} that follows basic principales of Kobotoolbox exported files:\cr
#'   \itemize{
#'    \item No group names in the column headers
#'    \item Download the data with XLM values and headers
#'    \item No special characters in column headers or choices, expect for select_multiple questions (see below). To do so, make sure that there is sepcial symbols in your XLSform before export.
#'    \item Select_multiple questions should appear in the dataframe in at least two columns (this is the default option when exporting data from kobo):
#'    \itemize{
#'    \item One generic column with the name of the question as header and all choices that were picked by respondents
#'    \item One column per choice in the question. The name of the column should be the name of the question, followed by a forward slash, and the name of the choice (e.g. ws_multi/pomp).
#'        }
#'}
#' \bold{Walk through} \cr
#' koboloadeRlight can handle two main scenarii of data exploration \cr
#' \itemize{
#'  \item Data downloaded right from a Kobotoolbox server. Data and form are available.
#'  \item Data from a kobotoolbox server modified. Typically cleaned data.
#' }
#'
#' For the first scenario:
#' \itemize{
#'      \item Import your data as a dataframe (with [read_csv()] or [read_excel] for instance).
#'      \item Run [kobo_dico()] to build a dictionnary linking your form and your data.
#'      \item Run a choice of [kobo_crunching_all()], [kobo_crunching()] or other analysis functions.
#' }
#'
#' If you don't have access to the updated form:
#' \itemize{
#' \item If you added questions to your dataframe (e.g. made additional calculations that you stored as columns), make sure to add them to your form.
#' \item Run [kobo_to_xlsform()] to generate a new form from the data
#' \item If you have a XLSform that genereated your data, you can use [kobo_xlsform_relabel()] to relabel your choices and questions according to the form.
#' \item Run [kobo_dico()] to build a dictionnary linking your form and your data.
#' \item Run a choice of [kobo_crunching_all()], [kobo_crunching()] or other analysis functions.
#' }
#' @keywords internal
#' @references This package is based on koboloadeR <https://github.com/unhcr/koboloadeR>. It provides a lighter alternative to koboloadeR, focusing on core functions.\cr
#'      The package is built for data exported from KoboToolbox. Full documentation for KoboToolbox is available here : <https://www.kobotoolbox.org/>
#'
"_PACKAGE"
#> [1] "_PACKAGE"
