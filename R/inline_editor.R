
#' Inline Code Editor Widget
#'
#' Creates an inline code editor using the \pkg{shinyAce} package that allows
#' users to dynamically enter and evaluate R code and see either a plot or
#' textual output.
#' @param type Either \code{'plot'} or \code{'text'}, depending on what you want
#'   to output.
#' @param codeWidth Out of 12, how many columns should be occupied by the code,
#'   the rest will be used for the output.
#' @param initialVal The initial text to put in the editor. 
#' @param height The height of the editor (in pixels).
#' @export
inlineEditor <- function(type=c("plot", "text"), codeWidth=8, initialVal="",
                          height=300) { 
  
  library(shinyAce)
  
  id <- as.character(round(runif(1, 10000, 99999)))
  inputId <- paste0("ace", id)
  
  type <- match.arg(type)
  if (type == "plot"){
    outputId <- paste0("plot", id)
    renderFun <- renderPlot
    
    output <- plotOutput(
      outputId, 
      height=paste0(height-25, "px"))
  } else if (type == "text"){
    outputId <-paste0("text", id)
    renderFun <- renderText
    
    output <- verbatimTextOutput(outputId)
  }
  
  shinyApp(
    ui = fluidPage(responsive=FALSE,
                   fluidRow(style="padding-bottom: 20px;",
                            column(codeWidth, 
                                   aceEditor(inputId, 
                                     value=initialVal, 
                                     mode="r", 
                                     height=(paste0(height-60,"px")))),
                            column(12-codeWidth, 
                                   output)
                   )
    ),
    server = function(input, output){
      output[[outputId]] <- renderFun({
        eval(parse(text=input[[inputId]]))
      })
    },
    options = list(height = height)
  )
}

