

# declare an html dependency (library + included script/stylesheets/etc)
html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = version,
    path = path,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

# generate html output for knitr
knitr_html_output <- function(x, dependencies) {
  structure(class = "knit_asis",
    x,
    knit_meta = dependencies
  )
}

# generate html output for the rstudio viewer or a standalone browser
viewer_html_output <- function(x, dependencies) {
  
  # define temporary directory for output
  www_dir <- tempfile("viewhtml")
  dir.create(www_dir)
    
  # build the web-page
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<head>",
            "<meta charset=\"utf-8\"/>",
            dependencies_as_html(dependencies, file.path(www_dir, "lib")),
            "</head>",
            "<body>",
            x,
            "</body>",
            "</html>")
  
  # write it
  index_html <- file.path(www_dir, "index.html")
  writeLines(html, index_html, useBytes = TRUE)
  
  # show it
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(index_html)
  else
    utils::browseURL(index_html)
  
  invisible(NULL)
}

# generate the HTML required to include the dependencies
dependencies_as_html <- function(dependencies, lib_dir) {
  
  html <- c()
  
  for (dep in dependencies) {
    
    # copy library files if necessary
    if (!is.null(lib_dir)) {
     
      if (!file.exists(lib_dir))
        dir.create(lib_dir)
      
      target_dir <- file.path(lib_dir, basename(dep$path))
      if (!file.exists(target_dir))
        file.copy(from = dep$path, to = lib_dir, recursive = TRUE)
      
      dep$path <- file.path(basename(lib_dir), basename(target_dir))
    }
    
    # add meta content
    for (name in names(dep$meta)) {
      html <- c(html, paste("<meta name=\"", name, 
                            "\" content=\"", dep$meta[[name]], "\" />", 
                            sep = ""))
    }
    
    # add stylesheets
    for (stylesheet in dep$stylesheet) {
      stylesheet <- file.path(dep$path, stylesheet)
      html <- c(html, paste("<link href=\"", stylesheet, "\" ",  
                            "rel=\"stylesheet\" />", 
                           sep = ""))
    }
    
    # add scripts
    for (script in dep$script) {
      script <- file.path(dep$path, script)
      html <- c(html,
        paste("<script src=\"", script, "\"></script>", sep = ""))
    }
    
    # add raw head content
    html <- c(html, dep$head)
  }
  
  html
}
