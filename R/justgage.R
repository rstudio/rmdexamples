
#' @export
justgage <- function(title, value, min, max, label = NULL) {
  structure(class = "justgage", list(
    title = title,
    label = label,
    value = value,
    min = min,
    max = max
  ))
}

#' @export
print.justgage <- function(x, ...) {
  htmltools::html_print(
    justgage_html(x, 450, 350),
    justgage_dependencies()
  )
}

#' @export
knit_print.justgage <- function(x, options, ...) {
  htmltools::html_knit_print(
    justgage_html(x, options$out.width.px, options$out.height.px),
    justgage_dependencies()
  )
}

justgage_html <- function(x, width, height) {

  # create random/unique id to bind the div and script
  id <- paste("justgage", as.integer(stats::runif(1, 1, 10000)), sep="-")

  # create a style attribute for the width and height
  style <- paste("width:", width, "px;height:", height, "px", sep = "")

  # create a list representing the parameters to JustGage
  options <- list(id = id,
                  title = x$title,
                  value = x$value,
                  min = x$min,
                  max = x$max,
                  label = x$label)

  # generate html for the justgage
  html <- paste(
    "<div id=\"", id, "\" ",  "style=\"", style, "\">", "</div>",
    "<script>var g = new JustGage(", RJSONIO::toJSON(options), ");</script>",
    sep = "")

  # return html
  html
}

justgage_dependencies <- function() {
  list(
    htmltools::html_dependency(
      name = "raphael",
      version = "2.1.2",
      path = system.file("www/libs/raphael-2.1.2", package = "rmdexamples"),
      script = "raphael.js"
    ),
    htmltools::html_dependency(
      name = "justgage",
      version = "1.0.1",
      path = system.file("www/libs/justgage-1.0.1", package = "rmdexamples"),
      script = "justgage.1.0.1.min.js"
    )
  )
}
