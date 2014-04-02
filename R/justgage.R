
#' @export
justgage <- function(title, value, min, max, 
                     label = NULL, width = 200, height = 160) {
  
  # create random/unique id to bind the div and script
  id <- paste("justgage", as.integer(stats::runif(1, 1, 10000)), sep="-") 

  # create a style attribute for the width and height
  style <- paste("width:", width, "px;height:", height, "px", sep = "")
  
  # create a list representing the parameters to JustGage
  options <- list(id = id,
                  title = title,
                  value = value,
                  min = min,
                  max = max,
                  label = label)

  # generate html for the justgage
  html <- paste(
    "<div id=\"", id, "\" ",  "style=\"", style, "\">", "</div>", 
    "<script>var g = new JustGage(", RJSONIO::toJSON(options), ");</script>",
    sep = "")
  
  # return
  structure(class = "justgage_html", html)
}

#' @export
print.justgage_html <- function(x, ...) {
  viewer_html_output(x, justgage_dependencies())
}

#' @export
knit_print.justgage_html <- function(x) {
  knitr_html_output(x, justgage_dependencies())
}

# list of html dependencies for justgage
justgage_dependencies <- function() {
  list(
    html_dependency(
      name = "justgage",
      version = "1.0.1",
      path = system.file("www/libs/justgage-1.0.1", package = "RmdExamples"),
      script = "justgage.1.0.1.min.js"
    ),
    html_dependency(
      name = "raphael",
      version = "2.1.2",
      path = system.file("www/libs/raphael-2.1.2", package = "RmdExamples"),
      script = "raphael.js"
    )
  )
}
