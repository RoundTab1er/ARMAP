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

getMem <- function() {
	os <- getOS()
	gc()

	if(os == "win") {
		temp <- system('wmic OS get FreePhysicalMemory /Value', intern=TRUE)
		mem = strsplit(temp[3], '=')
		mem = mem[[1]][2]
		mem = as.integer(sub("\r", "", b))
		mem = round(mem/1000, digits=0)
	} else if(os == "mac") {
		mem <- system('free -m', intern=TRUE)
		mem = strsplit(mem[[2]][1], '        ')[[1]][7]
	} else if(os == "unix") {
		mem <- system('free -m', intern=TRUE)
		mem = as.integer(strsplit(mem[[2]][1], '        ')[[1]][7])
	} else {
		mem <- -1
	}

	return(mem)
}

getOS <- function() {
	if (.Platform$OS.type == "windows") { 
	    os = "win"
	} else if (Sys.info()["sysname"] == "Darwin") {
	    os = "mac" 
	} else if (.Platform$OS.type == "unix") { 
	    os = "unix"
	} else {
	    os = "Unknown OS"
	}

	return(os)
}

setIndexMem <- function() {
	mem_free = getMem()

	if(mem_free < 1900) {
		showModal(modalDialog("Not Enough Memory Available", title = "Memory Failure"))
		quit()
	} else {
		return(as.integer(mem_free/2))
	}
}

getCores <- function() {
	return(detectCores())
}

getFileInfo <- function(filename) {
	time = file.info(filename)$ctime
	return(sub(" EDT", "", time))
}

welcomePage <- function() {
	helpText("This is the ARMAP app. It is designed to provide an easy-to-use graphical interface for processing of RNA-Seq data.")
}

manualPage <- function() {
	helpText("The manual will go here.")
}

debugPage <- function() {
	fluidRow( 
		column(
			3,
			textOutput("text1")
		),

		column(
			6,
					
			textOutput("text2")
		),

		column(
			3,
					
			textOutput("text3")
		)
	)
}

indexPage <- function() {
	fluidRow(
		#Reference Genome Files
		column(
			6,
			style="margin-top:0px",

			h4("Reference Genome"),

			### select local or server files ###
			selectInput(inputId='file_location', label='File location', 
				choices= c("Files on my computer" = "local", "Files on server" = "server")
			),

			conditionalPanel(
				condition = "input.file_location == 'local'",

				fileInput(inputId = 'annotation_local', label = "Annotation File", multiple = FALSE, width = NULL, 
        			buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf', ".saf")
      		 	)
			),

			conditionalPanel(
				condition="input.file_location == 'server'",

				selectInput(inputId = 'annotation_server', label = "Annotation File", choices = list.files('./annotation/'))
			)
		),

		column(
			6,
			style="margin-top:0px",

			br(),
			br(),

			#select genomic FASTA
			conditionalPanel(
				condition = "input.file_location == 'local'",

				fileInput(inputId = "local_fasta", label = "FASTA File", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
		        )
			),

			conditionalPanel(
				condition = "input.file_location == 'server'",
				
				selectInput(inputId = 'server_fasta', label = 'FASTA File', choices = list.files('fasta/')),
				br()
			)
		)
	)
}

mappingPage <- function() {
	tabsetPanel(
		tabPanel(
			"Config",

			fluidRow(
				column(
					4,
					style="margin-top:0px",

					h4("RNA-Seq Data"),
					selectInput(inputId = 'paired_end_local', label='Sequence Type', choices=
								c("Single End" = "se",
				                  "Paired End" = "pe")
					),

					conditionalPanel(
						condition="input.file_location == 'local'",

						br()
					)
				),

				column(
					4, 
					style="margin-top:0px",

					br(),
					br(),

					conditionalPanel(
						condition = "input.paired_end_local == 'se'",

						conditionalPanel(
							condition="input.file_location == 'local'",

							fileInput(inputId="ls_1", label="Select File", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz'))
						), 

						conditionalPanel(
							condition="input.file_location == 'server'",

							selectInput(inputId="se_1", label="Select File", choices = list.files('fastq/'))
						)
					),

					conditionalPanel(
						condition = "input.paired_end_local == 'pe'",

						conditionalPanel(
							condition="input.file_location == 'local'",

							fileInput(inputId="lp_1", label="Select File 1", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
							)
						), 

						conditionalPanel(
							condition="input.file_location == 'server'",

							selectInput(inputId="pe_1", label="Select File 1", choices = list.files('fastq/'))
						)
					)
				),

				column(
					4, 
					style="margin-top:0px",

					br(),
					br(),

					conditionalPanel(
						condition = "input.paired_end_local == 'pe'",

						conditionalPanel(
							condition="input.file_location == 'local'",

							fileInput(inputId="lp_2", label="Select File 2", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
							)
						),

						conditionalPanel(
							condition="input.file_location == 'server'",

							selectInput(inputId="pe_2", label="Select File 2", choices = list.files('fastq/'))
						)				
					)
				)
			),

			#RNA-Seq Parameter Configuration
			fluidRow(
				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="trim_3", "# 3' Bases to Trim:", 10, min = 0, max = 100),

					selectInput(inputId="sj_format", label="Output Format", choice=c("SAM (text-based)" = "sam", "BAM (binary)" = "bam"))
				),
			
				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="trim_5", "# 5' Bases to Trim:", 10, min = 0, max = 100)
				),

				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="num_mm", "# Mismatches Allowed", 3, min = 0, max = 50)				
				),

				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="num_indels", label="Max Indels Allowed", 5, min=0, max=100)
				)
			),

			actionButton('qc', 'View Basic QC Report'),
			actionButton('qc_full', 'View Full QC Report'),

			actionButton('start_mapping', 'Begin alignment'),
			actionButton('start_counting', 'Make featureCounts')
		),

		tabPanel(
			"Basic QC Report",

			plotOutput("cycle_quality"),
			plotOutput("cycle_gc")
		)
	)
}

countsPage <- function() {
	fluidRow(
		column(
			3,

			selectInput(inputId='annotation_type', label='Annotation Type', 
				choices= c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext")
			),

			selectInput(inputId='alignment_file', label='Alignment File', 
				choices= list.files("alignments/")
			)
		),

		column(
			3,

			conditionalPanel(
				condition="input.annotation_type == 'inbuilt'",

				selectInput(inputId='annotation_type', label='Annotation Type', 
					choices= c("mm10" = "mm10", "mm9" = "mm9", "hg38" = "hg38", "hg19" = "hg19")
				)
			),

			conditionalPanel(
				condition="input.annotation_type == 'ext'",

				selectInput(inputId='GTF', label='Annotation Type', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			3,

			selectInput(inputId='meta_features', label='Use Meta-Features', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		)

		### HAVE TO WORK OUT ANNOTATION FILE --> IF INBUILT DOESN'T MAKE SENSE TO ASK FOR IT
		### HAVE TO PASS IT TO featureCounts

		# GTF.featureType
		#allowMultiOverlap
		#countMultiMappingReads
		#minMQS

		#others??

	)
}

fileSelection <- function() {
	fluidRow(
		column(
			4,

			selectInput(inputId='file_location', label='File location', 
				choices= c("Files on my computer" = "local", "Files on server" = "server")
			),

			selectInput(inputId = 'paired_end_local', label='Sequence Type', choices=
								c("Single End" = "se",
				                  "Paired End" = "pe")
			)
		),


		column(
			4,

			#conditionalPanel(
			#	condition = "input.file_location == 'local'",

			#	fileInput(inputId = "local_fasta", label = "FASTA File", multiple = FALSE, width = NULL, 
			#		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
		    #    )
			#),

			conditionalPanel(
				condition = "input.file_location == 'server'",
				
				selectInput(inputId = 'server_fasta', label = 'FASTA File', choices = list.files('fasta/')),
				br()
			),

			conditionalPanel(
				condition = "input.paired_end_local == 'pe'",

				#conditionalPanel(
				#	condition="input.file_location == 'local'",

				#	fileInput(inputId="lp_1", label="Select File 1", multiple = FALSE, width = NULL, 
				#		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
				#	)
				#), 

				conditionalPanel(
					condition="input.file_location == 'server'",

					selectInput(inputId="pe_1", label="Select File 1", choices = list.files('fastq/'))
				)
			)
		),

		column(
			4,

			#conditionalPanel(
			#	condition="input.file_location == 'local'",

			#	fileInput(inputId = 'annotation_local', label = "Annotation File", multiple = FALSE, width = NULL, 
        	#		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf', ".saf")
      		# 	)
			#),

			conditionalPanel(
				condition="input.file_location == 'server'",

				selectInput(inputId = 'annotation_server', label = "Annotation File", choices = list.files('./annotation/'))
			),

			conditionalPanel(
				condition = "input.paired_end_local == 'pe'",

				#conditionalPanel(
				#	condition="input.file_location == 'local'",

				#	fileInput(inputId="lp_2", label="Select File 2", multiple = FALSE, width = NULL, 
				#		buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
				#	)
				#), 

				conditionalPanel(
					condition="input.file_location == 'server'",

					selectInput(inputId="pe_2", label="Select File 2", choices = list.files('fastq/'))
				)
			)
		)
	)
}

parameterSelection <- function() {
	fluidRow(
		column(
			3,

			numericInput(inputId="trim_3", "# 3' Bases to Trim:", 10, min = 0, max = 100),

			selectInput(inputId="annotation_selection", label="annotation_type", choices=
				c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext"))
		),


		column(
			3,

			numericInput(inputId="trim_5", "# 5' Bases to Trim:", 10, min = 0, max = 100),

			conditionalPanel(
				condition="input.annotation_selection == 'inbuilt'",

				selectInput(inputId='annotation_type', label='Annotation Type', 
					choices= c("mm10" = "mm10", "mm9" = "mm9", "hg38" = "hg38", "hg19" = "hg19")
				)
			),

			conditionalPanel(
				condition="input.annotation_selection == 'ext'",

				selectInput(inputId='GTF', label='Annotation Type', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			3,

			numericInput(inputId="num_mm", "# Mismatches Allowed", 3, min = 0, max = 50)
		),

		column(
			3,

			numericInput(inputId="num_indels", label="Max Indels Allowed", 5, min=0, max=100)
		)
	)
}

pipelinePage <- function() {
	tabsetPanel(
		tabPanel(
			"Config",

			fileSelection(),

			parameterSelection()
		),

		tabPanel(
			"QC Graphs",

			plotOutput("cycle_quality"),
			plotOutput("cycle_gc")
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

	    "Tools",
	    tabPanel("Build Index", indexPage()),
	    tabPanel("Map Reads", mappingPage()),
	    tabPanel("Count Features", countsPage()),

	    "Automated Workflow",
	    tabPanel("Automated Pipeline", pipelinePage())
	)
}

ui <- fluidPage(
	navList()
)

getAnnotationFile <- function(input) {
	
}

getIndexFile <- function(input) {

}

getSeqFiles <- function(input) {

}

getGenomeFile <- function(input) {

}

server <- function(input, output, session) 
	{
		output$text1 <- renderText({
    		paste0('You have selected: ', input$Documentation)
  		})

	  	output$text2 <- renderText({
	    	paste0('You have selected: ', getFileInfo("fasta/psuedo_assembly.fasta"))
	  	})
	}

shinyApp(ui = ui, server = server)