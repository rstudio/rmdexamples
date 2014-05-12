
#' @export
datatable <- function(data, align = NULL) {
  structure(class = "datatable", list(
    data = data,
    align = align
  ))
}

#' @export
print.datatable <- function(x, ...) {
  htmltools::html_print(datatable_html(x), datatable_dependencies())
}

#' @export
knit_print.datatable <- function(x, options, ...) {
  htmltools::html_knit_print(datatable_html(x), datatable_dependencies())
}

datatable_html <- function(x) {
  
  # create random/unique id for the table
  id <- paste("datatable", as.integer(stats::runif(1, 1, 10000)), sep="-") 
  
  # generate an html version of the table that includes the id
  data <- x$data
  align = if (is.null(align <- x$align)) '' else {
    sprintf(' align="%s"', c(l = 'left', c = 'center', r = 'right')[align])
  }
  html <- paste(c(
    sprintf("<table id = \"%s\">", id),
    c('<thead>', '<tr>', 
      sprintf('<th>%s</th>', htmltools::html_escape(colnames(data))), 
      '</tr>', '</thead>'),
    '<tbody>',
    paste(
      '<tr>',
      apply(data, 1, function(z) {
        z = htmltools::html_escape(z)
        paste(sprintf('<td%s>%s</td>', align, z), collapse = '')
      }),
      '</tr>', sep = ''
    ),
    '</tbody>',
    '</table>'
  ), sep = '', collapse = '')
   
  # append the script which binds the datatable
  html <- paste(html,
                "<div>&nbsp;</div>",
                "<script>",
                "$(document).ready(function() {",
                "$('#", id ,"').dataTable();",
                "});",
                "</script>", sep = "")
    
  # return html
  html
}

datatable_dependencies <- function() {
  list(
    htmltools::html_dependency(
      name = "jquery",
      version = "1.11.0",
      path = system.file("www/libs/jquery-1.11.0", package = "rmdexamples"),
      script = "jquery.min.js"
    ),
    htmltools::html_dependency(
      name = "datatables",
      version = "1.9.4",
      path = system.file("www/libs/datatables-1.9.4", package = "rmdexamples"),
      stylesheet = "css/jquery.dataTables.css",
      script = "js/jquery.dataTables.min.js"
    )
  )
}


