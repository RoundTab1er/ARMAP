#load required libraries
library(shiny)
library(Rqc)
library(Rsubread)

local_input <- function() {
	#if local -->
	conditionalPanel(
		condition = "input.file_location == 'local'",

		#select genomic reference *FASTA*
		fileInput(inputId = "local_fasta", label = "FASTA File", multiple = FALSE, width = NULL, 
		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
        ),

		#enter annoation file (GFF only)
        fileInput(inputId = 'annotation', label = "Annotation File", multiple = FALSE, width = NULL, 
        	buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf')
       	),

		### Input RNA-Seq files *FASTQ*
		selectInput(inputId = 'paired_end_local', label='Sequence Type', choices=
					c("Single End" = "se",
	                  "Paired End" = "pe")
		),

		#if paired-end, select TWO files
		conditionalPanel(
			condition = "input.paired_end_local == 'pe'",

			fileInput(inputId="lp_1", label="Select File 1", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
			),
			
			fileInput(inputId="lp_2", label="Select File 2", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
			)
		),

		conditionalPanel(
			condition = "input.paired_end_local == 'se'",

			fileInput(inputId="ls_1", label="Select File", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
			)
		)
	)
}

server_input <- function() {
	#else server file selection
	conditionalPanel(
		condition = "input.file_location == 'server'",

		#select genomic reference *FASTA*
		selectInput(inputId = 'server_fasta', label = 'FASTA File', choices = list.files('fasta/')),

		### Input RNA-Seq files *FASTQ*
		selectInput(inputId='paired_end_server', label='Sequence Type', choices=
					c("Single End" = "se",
	                  "Paired End" = "pe")),

		#if paired-end, select TWO files
		conditionalPanel(
			condition = "input.paired_end_server == 'pe'",

			selectInput(inputId="pe_1", label="Select File 1", choices = list.files('fastq/')),
			selectInput(inputId="pe_2", label="Select File 2", choices = list.files('fastq/'))
		),

		#else, select ONE file
		conditionalPanel(
			condition = "input.paired_end_server == 'se'",

			selectInput(inputId="se_1", label="Select File", choices = list.files('fastq/'))
		),

		actionButton('qc', 'View Basic QC Report'),
		actionButton('qc_full', 'View Full QC Report'),

		#enter annoation file (GFF only)
		selectInput(inputId = 'annotation', label = "Annotation File", choices = list.files('./annotation/'))
	)
}

#define ui object
ui <- fluidPage(
	titlePanel("ARMAP", windowTitle="ARMAP"),

	# *Input() functions

	### select local or server files ###
	selectInput(inputId='file_location', label='File location', 
		choices= c("Files on my computer" = "local", "Files on server" = "server")
	),

	local_input(),
	server_input(),


	actionButton('start_mapping', 'Begin alignment'),
	actionButton('start_counting', 'Make featureCounts'),

	selectInput(inputId="split_index", label="Split Index", choice=
		c("Yes" = "True",
	    	"No" = "False")
	),

	conditionalPanel(
		condition="input.split_index == 'True'",
		
		sliderInput("memory", "Amount of RAM to use",
    		min = 0, max = 4000, value = 1000
  		)
	),

	selectInput(inputId="trim", label="Trim Reads", choice=
		c("Yes" = "True",
	    	"No" = "False")),

	conditionalPanel( 
		condition="input.trim == 'True'",

		numericInput(inputId="trim_3", "# 3' Bases to Trim:", 0, min = 0, max = 100),
		numericInput(inputId="trim_5", "# 5' Bases to Trim:", 0, min = 0, max = 100)
	),

	numericInput(inputId="num_mm", "# Mismatches to Allow", 3, min = 0, max = 50),

	# *Output() functions

	### DEBUG OUTPUT ###
	textOutput("fileselected"),
	textOutput("text1"),
	textOutput("text2"),
	textOutput("bi_output"),
	textOutput("sj_output"),

	### TEST OUTPUT ###
	plotOutput("cycle_quality"),
	plotOutput("cycle_gc")
)

server <- function(input, output, session) {
	### PRINT DEBUG TEXT ###
	output$fileselected <- renderText({
    	paste0('You have selected: ', input$file_location)
  	})

  	output$text1 <- renderText({
    	paste0('You have selected: ', input$split_index == "True")
  	})

  	output$text2 <- renderText({
    	paste0('You have selected: ', input$trim_3)
  	})

  	### TESTING REAL PIPELINE IMPLEMENTATION ###
  	observeEvent(input$qc_full, 
	  	{
	  		progress <- Progress$new(session, min=1, max=15)
    		on.exit(progress$close())

   			progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

		    for (i in 1:15) {
		      progress$set(value = i)
		      Sys.sleep(0.5)
		    }

	  		qc_results <- rqc(path = "fastq", pattern = ".fastq")
	 		View(qc_results)
	 	}
  	)

  	observeEvent(input$qc, 
  		{
  			progress <- Progress$new(session, min=1, max=15)
    		on.exit(progress$close())

   			progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

		    for (i in 1:15) {
		      progress$set(value = i)
		      Sys.sleep(0.1)
		    }

  			fastqDir <- "fastq/"
  			files <- list.files(fastqDir, "fastq", full.names=TRUE)
  			qa <- rqcQA(files, workers=1)

  			df <- rqcCycleAverageQualityCalc(qa)
			cycle <- as.numeric(levels(df$cycle))[df$cycle]
			output$cycle_quality <- renderPlot(plot(cycle, df$quality, col = df$filename, xlab='Cycle', ylab='Quality Score'))

			gcp <- rqcCycleGCPlot(qa)
			cycle2 <- as.numeric(levels(gcp[["data"]]$cycle))[gcp[['data']]$cycle]
			output$cycle_gc <- renderPlot(plot(cycle2, gcp[['data']]$gc, col = gcp[['data']]$filename, xlab='Cycle', ylab='% GC', type='b'))
  		}
  	)  	

  	observeEvent(input$start_mapping, 
  		{
			progress <- Progress$new(session, min=1, max=15)
    		on.exit(progress$close())

   			progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

		    for (i in 1:15) {
		      progress$set(value = i)
		      Sys.sleep(0.1)
		    }

  			filename <- strsplit(input$server_fasta, "[.]")[[1]][[1]] #"psuedo_assembly"

  			if(input$trim != "True") {
  				num_bases_5 = 0
  				num_bases_3 = 0
  			} else {
  				num_bases_3 = input$trim_3
  				num_bases_5 = input$trim_5
  			}

  			print(num_bases_3)

  			if(input$split_index == "True") {

  				bi = buildindex(basename=paste0("index/", filename, "_index"), reference=paste0("fasta/", filename, ".fasta"), indexSplit=TRUE, memory=input$memory)
  			} else {
  				bi = buildindex(basename=paste0("index/", filename, "_index"), reference=paste0("fasta/", filename, ".fasta"))
  			}
  			
  			reads = strsplit(input$se_1, "[.]")[[1]][[1]] #"psuedo_assembly"

  			sj = subjunc(paste0("index/", filename, "_index"), paste0("fastq/", reads, ".fastq"), output_file = paste0("alignments/", reads, ".sam"), 
  				output_format = "SAM", maxMismatches = input$num_mm, nTrim5=num_bases_5, nTrim3=num_bases_3)

  			output$fileselected <- renderText({
    			paste0("Build index results: ", bi)
  			})

  			output$fileselected <- renderText({
    			paste0('Subjunc results: ', sj)
  			})

  			#subjunc("index/psuedo_assembly_index", "fastq/reads.fastq", output_file = "alignments/reads_alignment.sam", output_format = "SAM", maxMismatches = 10, nTrim5=15, nTrim3=15)
  		} 
  	)



  	observeEvent(input$start_counting, 
  		{
  			#
  		} 
  	)
}

shinyApp(ui = ui, server = server)
