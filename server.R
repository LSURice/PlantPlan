library(plotly)
library(DT)
library(openxlsx)
source("helperFunctions.R")


# Read in authentication information from .Renviron file 
# add this file to .gitignore
readRenviron(".Renviron")
con <- Sys.getenv("DB")
user <- Sys.getenv("USERNAME")
passwd <- Sys.getenv("PASSWORD")

function(input, output, session){

  # Initialize a reactive value for the layout df
  layout_info <- reactiveVal()

  # Load in trials using BrAPI
  showModal(modalDialog("Importing trials...", footer=NULL))
  token <- get_token(user, passwd, con)
  study_dict <- get_study_named_list(con, token)
  updateSelectInput(session,
		            "trialName",
		             choices=names(study_dict))
  removeModal()
  
  # Load a trial spatial layout when button is pushed
  observeEvent(input$layoutImport,{
    studyDbId <- study_dict[input$trialName]
    showModal(modalDialog("Importing layout...", footer=NULL))
    token <- get_token(user, passwd, con)
    layout <- get_layout_info(con, studyDbId, token)
    removeModal()
    updateSelectInput(session, "layout", choices=c("None", "Side-by-Side", "Stacked"))
    if(!is_null_layout(layout)){  # If no layout create a default layout
      layout_info(layout)
    } else{
      showModal(modalDialog(
       title=NULL,
       "No spatial grid was present for this trial. A psuedo-grid was generated!",
       easyClose=TRUE,
       footer=NULL
      ))
      layout_info(make_default_layout(layout))
    }
  })

  # If layout changes load new dimensions
  observeEvent(layout_info(),{
    valueRow <- max(as.numeric(layout_info()$Y))
    updateNumericInput(session, "nRows",value=valueRow)
    valueCol <- max(as.numeric(layout_info()$X))
    updateNumericInput(session, "nCols", value=valueCol)
  })

  # When user hits update button, update the layout as long as it's valid 
    observeEvent(input$update, {
      if(is.na(input$nRows)){
        # If not trial loaded return error
         showModal(modalDialog(
           title="Trial not loaded",
           "Please load a trial layout first!",
           easyClose=TRUE,
           footer=NULL
      ))
      } else if (input$layout == "None"){
        # If no layout type specified return error
         showModal(modalDialog(
           title="Missing layout type",
           "Please choose a design type first!",
           easyClose=TRUE,
           footer=NULL
      ))
      } else{
        # Check if layout is valid
        if(is_valid_design(input$nRows, input$nCols, layout_info, input$layout)){
          # Update design
          if(input$layout == "Side-by-Side"){
          	layout_info(make_side_to_side_layout(layout_info(), input$nRows, input$nCols))
          } else{
          	layout_info(make_stacked_layout(layout_info(), input$nRows, input$nCols)) 
          }
        } else{
        # Return error message if not valid
          showModal(modalDialog(
            title= "Design not valid",
            "Design is not valid. Please try different dimensions for tier and pass.",
            easyClose=TRUE,
            footer=NULL
          ))
        }
      }
    })
  

  # Plot layout
  output$plot <- renderPlotly({
    if(is.null(layout_info())){
    	return()
    }
    layoutPlot(layout_info(), input)
  })
  
  # Show planting plan table
  output$plantPlan <- DT::renderDataTable({
    if(is.null(layout_info())){
      return()
    }
  	get_plant_plan(layout_info(), input$border, input$metadata)
  	},
  	rownames=FALSE)

  # Downloads planting plan
  output$plantDownload <- downloadHandler(
  	filename = function() {paste0(input$trialName,"_", "planting_plan",
  	                              "_", Sys.Date(), ".csv")},
  	content = function(file) {
      data <- get_plant_plan(layout_info(), input$border, input$metadata)
      write.csv(data, file, row.names=FALSE, quote=TRUE)
  	}
  )

  # Downloads spatial grid
  output$spatialDownload <- downloadHandler(
  	filename = function() {paste0(input$trialName,"_spatial_grid_",
  	                              Sys.Date(), ".txt")},
  	content = function(file){
  	  data <- layout_info()
  	  data <- data[, c("observationUnitName", "Y", "X")]
  	  sorted_data <- data[order(as.numeric(data$Y), as.numeric(data$X)),]
  	  colnames(data) <- c("plot_name", "row_number", "col_number")
  	  write.table(data, file, sep="\t", row.names=F, quote=F)
  	}
  )
  # Download field grid
  output$gridDownload <- downloadHandler(
  	filename = function() {paste0(input$trialName, "_field_grid_",
  	                              Sys.Date(), ".xlsx")},
  	content = function(file){
  	  data <- layout_info()
  	  grid <- get_field_grid(data, input$border)
  	  write.xlsx(grid, file, row.names=F, col.names=F)
  	}
  ) 
}
