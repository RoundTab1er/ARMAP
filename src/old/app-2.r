source('test.R')
source('outputLogic.R')
source('pipelinePage.R')
source('countsPage.R')
source('mappingPage.R')
source('manualPage.R')
source('welcomePage.R')
source('utilities.R')

checkDependencies <- function() {
	showModal(modalDialog("Dependencies not installed, check manual page", title = "Dependencies Check Failed"))
}

loadLibraries <- function() {
	library(shiny)
	library(doParallel)
	library(Rqc)
	library(Rsubread)
}

loadLibraries()

debugPage <- function() {
	fluidRow( 
		column(
			3,
			helpText("I am a debug message")
		),

		column(
			4,
					
			textOutput("text1")
		),

		column(
			3,
					
			textOutput("text2")
		)
	)
}

navList <- function() {
	navlistPanel(
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

ui <- fluidPage(
	navList()
)

server <- function(input, output, session) 
	{
		output$text1 <- renderText({
    		paste0('File creation time: ', getFileInfo('fasta/psuedo_assembly.fasta'))
  		})

	  	output$text2 <- renderText({
	    	paste0('Available memory: ', functionTest())
	  	})
	}

shinyApp(ui = ui, server = server)