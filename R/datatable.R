
#' @export
datatable <- function(data, align = NULL) {

  # create random/unique id for the table
  id <- paste("datatable", as.integer(stats::runif(1, 1, 10000)), sep="-")

  colformat <- if (length(align) == 0) {
    as.character(tags$td("%s"))
  } else {
    sapply(rep_len(align, ncol(data)), function(x) {
      as.character(tags$td(align=x, "%s"))
    })
  }

  # generate an html version of the table that includes the id
  table <- tagList(
    tags$table(id = id,
      tags$thead(
        tags$tr(
          lapply(colnames(data), function(name) { tags$th(name) })
        )
      ),
      tags$tbody(
        HTML(paste(sep = "", collapse = "\n",
          "<tr>",
          apply(data, 1, function(row) {
            paste(sprintf(colformat, row), collapse = "")
          }),
          "</tr>"
        ))
      )
    ),
    tags$div(HTML("&nbsp;")),
    tags$script(HTML(
      "$(document).ready(function() {",
      sprintf("$('#%s').dataTable();", id),
      "});"
    ))
  )

  browsable(attachDependencies(table, datatable_dependencies()))
}

datatable_dependencies <- function() list(
  htmltools::htmlDependency(
    name = "jquery",
    version = "1.11.0",
    src = system.file("www/libs/jquery-1.11.0", package = "rmdexamples"),
    script = "jquery.min.js"
  ),
  htmltools::htmlDependency(
    name = "datatables",
    version = "1.9.4",
    src = system.file("www/libs/datatables-1.9.4", package = "rmdexamples"),
    stylesheet = "css/jquery.dataTables.css",
    script = "js/jquery.dataTables.min.js"
  )
)


