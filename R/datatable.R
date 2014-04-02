

#' @export
datatable <- function(x, 
                      digits = getOption("digits"),
                      row.names = NA,
                      align = NULL,
                      ...) {
  
  # create random/unique id for the table
  id <- paste("datatable", as.integer(stats::runif(1, 1, 10000)), sep="-") 
  
  # generate an html version of the table that includes the id
  html_table <- knitr::kable(
    x, 
    format = "html", 
    digits = digits,
    row.names = NA,
    align = align,
    output = FALSE,
    table.attr = paste("id=\"", id, "\" ", 
                        "class=\"table table-bordered\" ",
                        sep = ""),
    ...)
  
  # create the script which binds the datatable
  script <- paste(
    "<script>",
      "$(document).ready(function() {",
        "$('#", id ,"').dataTable();",
      "});",
   "</script>", sep = "")
  
  # return html
  html <- paste(html_table, script, sep = "")
  structure(class = "datatable_html", html)
}

#' @export
print.datatable_html <- function(x, ...) {
  viewer_html_output(x, datatables_dependencies())
}

#' @export
knit_print.datatable_html <- function(x) {
  knitr_html_output(x, datatables_dependencies())
}

# list of html dependencies for datatables
datatables_dependencies <- function() {
  list(
    html_dependency(
      name = "jquery",
      version = "1.11.0",
      path = system.file("www/libs/jquery-1.11.0", package = "RmdExamples"),
      script = "jquery.min.js"
    ),
    html_dependency(
      name = "datatables",
      version = "1.9.4",
      path = system.file("www/libs/datatables-1.9.4", package = "RmdExamples"),
      stylesheet = "css/jquery.dataTables.css",
      script = "js/jquery.dataTables.min.js"
    )
  )
}