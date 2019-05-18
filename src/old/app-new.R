#load required libraries
library(shiny)

#define ui object
ui <- fluidPage(
	# *Input() functions

	### select local or server files ###
	selectInput(inputId='file_location', label='File location', 
		choices= c("Files on my computer" = "local", "Files on server" = "server")
	),

	#if local -->
	conditionalPanel(
		condition = "input.file_location == 'local'",

		#select genomic reference *FASTA*
		fileInput(inputId = "fasta", label = "FASTA File", multiple = FALSE, width = NULL, 
		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
        ),

		#enter annoation file (GFF only)
        fileInput(inputId = 'annotation', label = "Annotation File", multiple = FALSE, width = NULL, 
        	buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf')
       	),

		### Input RNA-Seq files *FASTQ*
		selectInput(inputId = 'paired_end', label='Sequence Type', choices=
					c("Single End" = "se",
	                  "Paired End" = "pe")
		),

		#if paired-end, select TWO files
		conditionalPanel(
			condition = "input.paired_end == 'pe'",

			fileInput(inputId="pe-1", label="Select File 1", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
			),
			
			fileInput(inputId="pe-2", label="Select File 2", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
			)
		),

		conditionalPanel(
			condition = "input.paired_end == 'se'",

			selectInput(inputId="se-1", label="Select File", choices = list.files('./fastq/')
			)
		)
	),

	#else server file selection
	conditionalPanel(
		condition = "input.file_location == 'server'",

		#select genomic reference *FASTA*
		selectInput(inputId = 'fasta', label = 'FASTA File', choices = list.files('./fasta/')),

		### Input RNA-Seq files *FASTQ*
		selectInput(inputId='paired_end', label='Sequence Type', choices=
					c("Single End" = "se",
	                  "Paired End" = "pe")),

		#if paired-end, select TWO files
		conditionalPanel(
			condition = "input.paired_end == 'pe'",

			selectInput(inputId="pe-1", label="Select File 1", choices = list.files('./fastq/')),
			selectInput(inputId="pe-2", label="Select File 2", choices = list.files('./fastq/'))
		),

		#else, select ONE file
		conditionalPanel(
			condition = "input.paired_end == 'se'",

			selectInput(inputId="se-1", label="Select File", choices = list.files('./fastq/'))
		),

		#enter annoation file (GFF only)
		selectInput(inputId = 'annotation', label = "Annotation File", choices = list.files('./annotations/'))
	),

	# *Output() functions
	textOutput("fileselected")
)

server <- function(input, output) {
	output$fileselected <- renderText({
    	paste0('You have selected: ', input$file_location)
  	})

  	output$fileselected <- renderText({
    	paste0('You have selected: ', input$paired_end)
  	})
}

shinyApp(ui = ui, server = server)
