

#' @export
dataset_view <- function(csv, output_file) {
  
  # get the path to the report
  report <- system.file("reports/dataset-view/dataset-view.Rmd", 
                        package = "rmdexamples")
  
  # render to the output file using the specified csv
  rmarkdown::render(input = report, 
                    output_file = output_file,
                    params = list(dataset = csv), 
                    envir = new.env())
} 
