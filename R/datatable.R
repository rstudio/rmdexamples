

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
  
  # return as object 
  structure(html_table, 
            tableid = id,
            class = "datatable")
}

#' @export
knit_print.datatable <- function(x) {
  
  # get the table id
  id <- attr(x, "tableid")
  
  # create the script which binds the datatable
  script <- paste("<script>",
                    "$(document).ready(function() {",
                       "$('#", id ,"').dataTable();",
                    "});",
                  "</script>", sep = "")
  
  # create html by combining table and script
  html <- paste(x, script, sep = "")
  
  # return html and dependencies
  structure(class = "knit_asis",
    html,
    knit_meta = datatables_dependency()
  )
}

datatables_dependency <- function() {
  structure(class = "html_dependency", list(
    name = "datatables",
    version = "1.9.4",
    path = system.file("www/libs/datatables-1.9.4", package = "RmdExamples"),
    stylesheet = "css/DT_bootstrap.css",
    script = c("js/jquery.dataTables.min.js",
               "js/DT_bootstrap.js")
  ))
}