library(shinycustomloader)
library(shinythemes)
library(plotly)
library(DT)


fluidPage(
  title="PlantPlan",
  theme=shinytheme("flatly"),
  titlePanel("PlantPlan"),
    sidebarLayout(
      sidebarPanel(
        selectInput(label="Trials", inputId="trialName",
		    choices=NULL),
		actionButton(label="Load Layout", inputId="layoutImport"),
		fileInput("metadata", "Upload Metadata",
                   multiple = FALSE,
                   accept = c(".xlsx")),
	    numericInput(label="Number of Tiers", inputId="nRows", value=NULL, min=1),
	    numericInput(label="Number of Passes", inputId="nCols", value=NULL, min=1),S
	    checkboxGroupInput(label="Borders", inputId="border",
	                       inline=T, choices=c("Left", "Right")),
	    selectInput(label="Layout Type", inputId="layout",
		    choices=c("None", "Side-by-Side", "Stacked")),
	   actionButton(label="Update Layout", inputId="update")),
    mainPanel(
      tabsetPanel(
      	tabPanel("Layout",
      	  withLoader(plotlyOutput("plot")),
      	  downloadButton("spatialDownload", "Update to Database"),
          downloadButton("gridDownload", "Download Field Grid")
      	),
      	tabPanel("Planting Plan",
      	   withLoader(DT::dataTableOutput("plantPlan")),
      	   downloadButton("plantDownload", "Download")
      	 )
      )
    )
    )
 )

