
#' @export
justgage <- function(title, value, min, max, 
                     label = NULL, width = 200, height = 160) {
  
  structure(class = "justgage_html", list(
    title = title,
    label = label,
    value = value,
    min = min,
    max = max,
    width = width,
    height = height
  ))
}

#' @export
knit_print.justgage_html <- function(x) {
  
  # create random/unique id to bind the div and script
  id <- paste("justgage", as.integer(stats::runif(1, 1, 10000)), sep="-") 
  
  # generate html for the justgage
  html <- paste("<div id='", id, "' ", 
                     "style='width:", x$width, "px;",
                            "height:", x$height, "px'>",
                "</div>", 
                "<script>",
                "var g = new JustGage({",
                " id: '", id, "',", 
                " value: ", x$value, ",", 
                " min: ", x$min, ",", 
                " max: ", x$max, ",", 
                " title: '", x$title, "',", 
                " label: '", x$label, "'",
                "});",
                "</script>",
                sep = "")
  
  # return html and dependencies
  structure(class = "knit_asis",
    html,
    knit_meta = list(raphael_dependency(), justgage_dependency())
  )
}

justgage_dependency <- function() {
  structure(class = "html_dependency", list(
    name = "justgage",
    version = "1.0.1",
    path = system.file("www/libs/justgage-1.0.1", package = "RmdExamples"),
    script = "justgage.1.0.1.min.js"
  ))
}

raphael_dependency <- function() {
  structure(class = "html_dependency", list(
    name = "raphael",
    version = "2.1.2",
    path = system.file("www/libs/raphael-2.1.2", package = "RmdExamples"),
    script = "raphael.js"
  ))
}
