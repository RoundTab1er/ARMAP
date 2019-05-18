### ARMAP ###
### Author: Nicholas Garcia
### Date: 4/4/19
### License: ?
### 
### Purpose:
	# Automated
	# RNA-Seq
	# Mapping (and)
	# Analysis
	# Pipeline

#load required libraries
library(shiny)

## Define Shiny App ##

#define ui object
ui <- fluidPage(
	# *Input() functions
	selectInput(inputId='file-location', label='File location', 
		choices= c("local" = "Files on my computer", "server" = "Files on server")),

	### select local or server files ###

	#if local -->
	conditionalPanel(
		condition= "input.file-location == 'local'"

		#select genomic reference *FASTA*
		fileInput(inputId = "fasta", label = "FASTA File", multiple = FALSE, width = NULL, 
		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf')
        )
	),

	#else server file selection
	conditionalPanel(
		condition= "input.file-location == 'server'",
	
		### Input RNA-Seq files *FASTQ*
		selectInput(inputId='paired-end', label='Sequence Type', choices=
					c("se" = "Single End",
	                  "pe" = "Paired End")),

		#if paired-end, select TWO files
		conditionalPanel(
	  		condition = "input.paired-end == 'pe'",

			selectInput(inputId="selectfile", label="Select File 1", choices = list.files('./fastq/')),
			selectInput(inputId="selectfile", label="Select File 2", choices = list.files('./fastq/'))
		),

		#else, select ONE file
		conditionalPanel(
	  		condition = "input.paired-end == 'se'",

			selectInput(inputId="selectfile", label="Select File", choices = list.files('./fastq/')),
	),

	#enter annoation file (GFF only)

	# *Output() functions
	textOutput("fileselected")
	)


server <- function(input, output) {
  output$fileselected <- renderText({
    paste0('You have selected: ', input$selectfile)
  })
}

shinyApp(ui = ui, server = server)

