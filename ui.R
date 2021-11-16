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
        selectInput(label="Year", inputId="trialYear",
                    choices=NULL),
        # selectInput(label="Trial type", inputId="trialType",
                    # choices=c("None")),
        actionButton("Load trials", inputId="trialImport"),
        selectInput(label="Available trials", inputId="trialName",
		            choices=NULL),
		actionButton(label="Load trial layout", inputId="layoutImport"),
	    numericInput(label="Number of tiers", inputId="nRows", value=NULL, min=1),
	    numericInput(label="Number of passes", inputId="nCols", value=NULL, min=1),
	    checkboxGroupInput(label="Borders", inputId="border",
	                       inline=T, choices=c("Left", "Right")),
	    selectInput(label="Layout type", inputId="layout",
		    choices=c("None", "Side-by-Side", "Stacked")),
	   actionButton(label="Update layout", inputId="update")),
    mainPanel(
      tabsetPanel(
      	tabPanel("Layout",
      	  withLoader(plotlyOutput("plot")),
      	  downloadButton("spatialDownload", "Download spatial layout"),
          downloadButton("gridDownload", "Download field map")
      	),
      	tabPanel("Planting Plan",
      	   fileInput("metadata", "Add metadata",
                     multiple = FALSE,
                     accept = c(".xlsx")),
      	   withLoader(DT::dataTableOutput("plantPlan")),
          downloadButton("plantDownload", "Download")
      	 )
      )
    )
    )
 )

