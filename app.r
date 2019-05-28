# Main ARMAP application
#
# Author: Nicholas Garcia
# Version 1.0.0
# Date: 2019-05-27
#

#Set path to data folders (folder structure: data/ and src/ folders in root ARMAP folder)
# src folder shall contain all source files
# data folder shall contain folders
#	genome/
#	index/
#	reads/
#	alignments/
#	annotations/
#	counts/
assign("my_path", paste0(getwd(), '/data/'), envir = .GlobalEnv)

#debug messages
print(my_path)
print(paste0(my_path, 'reads'))
print(list.files(paste0(my_path, 'reads')))

#source files containing logic and per page shiny content panels
source('src/outputLogic.R')
source('src/pipelinePage.R')
source('src/countsPage.R')
source('src/mappingPage.R')
source('src/manualPage.R')
source('src/welcomePage.R')
source('src/utilities.R')
source('src/qc_page_logic.R')

#'
checkDependencies <- function() {
	showModal(modalDialog("Dependencies not installed, check manual page", title = "Dependencies Check Failed"))
}

#' Loads required libraries
loadLibraries <- function() {
	library(shiny)
	library(doParallel)
	library(Rqc)
	library(Rsubread)
}

loadLibraries()

#' Debug page with several text outputs which allow easy debug prints
debugPage <- function() {
	fluidRow( 
		column(
			3,
			helpText("I am a debug message"),

			hr(),

			textOutput("text3")
		),

		column(
			4,
					
			textOutput("text1"),

			hr(),

			textOutput("text4")
		),

		column(
			3,
					
			textOutput("text2"),

			hr(),

			textOutput("text5")
		)
	)
}

#' Defines a Shiny NavListPanel which allows navigation of various pages
navList <- function() {
	navlistPanel(
		id='nav',
		widths=c(3, 9),

    	"Documentation",
	    tabPanel("Welcome Page", welcomePage()),
	    tabPanel("Manual", manualPage()),
	    tabPanel("Debug", debugPage()),


	   	"Automated Workflow",
	    tabPanel("Automated Pipeline", pipelinePage()),

	    "Tools",
	   	tabPanel("Map Reads", mappingPage()),
	   	tabPanel("Count Features", countsPage())
	)
}

#' Define UI for application
ui <- fluidPage(
	navList()
)

#'Define server logic for output
server <- function(input, output, session) 
	{
		#degbug page outputs
		output$text1 <- renderText({
    		paste0('Text1: ', input$file_location_counting)#getFileInfo('fasta/psuedo_assembly.fasta'))
  		})

	  	output$text2 <- renderText({
	    	paste0('Text2: ', input$annotation_counts)
	  	})

	  	output$text3 <- renderText({
	    	paste0('Text3: ', input$nav)
	  	})

	  	output$text4 <- renderText({
	    	paste0('Alignment File: ', input$alignment_file_counts)
	  	})

	  	output$text5 <- renderText({
	    	paste0('Text5: ', input$annotation_local_pipeline$name)
	  	})

	  	pipeline_page_qc(input, output, session)

  		pipeline_page_full_qc(input, output, session)

  		run_pipeline(input, output, session)

  		pipeline_page_download(input, output, session)

  		counts_page_count(input, output, session)

  		counts_page_download(input, output, session)

  		mapping_page_qc(input, output, session)

  		mapping_page_full_qc(input, output, session)

  		mapping_page_run(input, output, session)

  		mapping_page_download(input, output, session)
	}

#'Run the application 
shinyApp(ui = ui, server = server)
