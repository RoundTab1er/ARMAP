source('/home/garcian/thesis/r_workspace/utilities.r')

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

indexPage <- function() {
	fluidRow(
		#Reference Genome Files
		column(
			4,
			style="margin-top:0px",

			h4("Reference Genome"),

			### select local or server files ###
			selectInput(inputId='file_location_index', label='File location', 
				choices= c("Files on my computer" = "local", "Files on server" = "server")
			)
		),

		column(
			4,
			style="margin-top:0px",

			br(),
			br(),

			#select genomic FASTA
			conditionalPanel(
				condition = "input.file_location_index == 'local'",

				fileInput(inputId = "local_fasta_index", label = "FASTA File", multiple = FALSE, width = NULL, 
				buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
		        )
			),

			conditionalPanel(
				condition = "input.file_location_index == 'server'",
				
				selectInput(inputId = 'server_fasta_index', label = 'FASTA File', choices = list.files('fasta/')),
				br()
			)
		),

		column(
			4,
			style="margin-top:0px",

			br(),
			br(),

			conditionalPanel(
				condition = "input.file_location_index == 'local'",

				fileInput(inputId = 'annotation_local_index', label = "Annotation File", multiple = FALSE, width = NULL, 
        			buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf', ".saf")
      		 	)
			),

			conditionalPanel(
				condition="input.file_location_index == 'server'",

				selectInput(inputId = 'annotation_server_index', label = "Annotation File", choices = list.files('./annotation/'))
			)
		),

		hr(),

		actionButton('build_index', 'Build Index')
	)
}

mappingPage <- function() {
	tabsetPanel(
		tabPanel(
			"Config",

			fluidRow(
				column(
					4,

					selectInput(inputId='file_location_mapping', label='File location', 
						choices= c("Files on my computer" = "local", "Files on server" = "server")
					)
				),

				column(
					6,

					selectInput(inputId = 'index_file_mapping', label = "Index File", choices = list.files('./index/', ".files"))
				)
			),

			fluidRow(
				column(
					4,
					style="margin-top:0px",

					h4("RNA-Seq Data"),
					selectInput(inputId = 'paired_end_mapping', label='Sequence Type', choices=
								c("Single End" = "se",
				                  "Paired End" = "pe")
					),

					conditionalPanel(
						condition="input.file_location_mapping == 'local'",

						br()
					)
				),

				column(
					4, 
					style="margin-top:0px",

					br(),
					br(),

					conditionalPanel(
						condition = "input.paired_end_mapping == 'se'",

						conditionalPanel(
							condition="input.file_location_mapping == 'local'",

							fileInput(inputId="ls_1_mapping", label="Select File", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz'))
						), 

						conditionalPanel(
							condition="input.file_location_mapping == 'server'",

							selectInput(inputId="se_1_mapping", label="Select File", choices = list.files('fastq/'))
						)
					),

					conditionalPanel(
						condition = "input.paired_end_mapping == 'pe'",

						conditionalPanel(
							condition="input.file_location_mapping == 'local'",

							fileInput(inputId="lp_1_mapping", label="Select File 1", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
							)
						), 

						conditionalPanel(
							condition="input.file_location_mapping == 'server'",

							selectInput(inputId="pe_1_mapping", label="Select File 1", choices = list.files('fastq/'))
						)
					)
				),

				column(
					4, 
					style="margin-top:0px",

					br(),
					br(),

					conditionalPanel(
						condition = "input.paired_end_mapping == 'pe'",

						conditionalPanel(
							condition="input.file_location_mapping == 'local'",

							fileInput(inputId="lp_2_mapping", label="Select File 2", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
							)
						),

						conditionalPanel(
							condition="input.file_location_mapping == 'server'",

							selectInput(inputId="pe_2_mapping", label="Select File 2", choices = list.files('fastq/'))
						)				
					)
				)
			),

			#RNA-Seq Parameter Configuration
			fluidRow(
				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="trim_3_mapping", "# 3' Bases to Trim:", 10, min = 0, max = 100),

					selectInput(inputId="sj_format_mapping", label="Output Format", choice=c("SAM (text-based)" = "sam", "BAM (binary)" = "bam"))
				),
			
				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="trim_5_mapping", "# 5' Bases to Trim:", 10, min = 0, max = 100)
				),

				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="num_mm_mapping", "# Mismatches Allowed", 3, min = 0, max = 50)				
				),

				column(
					3,
					style="margin-top:0px",

					numericInput(inputId="num_indels_mapping", label="Max Indels Allowed", 5, min=0, max=100)
				)
			),

			hr(),

			actionButton('qc_mapping', 'View Basic QC Report'),
			actionButton('qc_full_mapping', 'View Full QC Report'),

			actionButton('start_mapping_mapping', 'Begin alignment'),
			actionButton('start_counting_mapping', 'Make featureCounts')
		),

		tabPanel(
			"Basic QC Report",

			plotOutput("cycle_quality"),
			plotOutput("cycle_gc")
		)
	)
}


countsRow1 <- function() {
	fluidRow(
		column(
			4,

			selectInput(inputId='annotation_type_counts', label='Annotation Type', 
				choices= c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext")
			),

			selectInput(inputId='alignment_file_counts', label='Alignment File', 
				choices= list.files("alignments/")
			)
		),

		column(
			4,

			conditionalPanel(
				condition="input.annotation_type_counts == 'inbuilt'",

				selectInput(inputId='annotation_type_counts', label='Annotation Type', 
					choices= c("mm10" = "mm10", "mm9" = "mm9", "hg38" = "hg38", "hg19" = "hg19")
				)
			),

			conditionalPanel(
				condition="input.annotation_type_counts == 'ext'",

				selectInput(inputId='GTF_counts', label='Annotation Type', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			4,

			selectInput(inputId='meta_features_counts', label='Use Meta-Features', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		)
	)
}

countsRow2 <- function() {

}

countsPage <- function() {
	countsRow1()

	#hr()

	#actionButton('count_features', 'Count Features')

	### HAVE TO WORK OUT ANNOTATION FILE --> IF INBUILT DOESN'T MAKE SENSE TO ASK FOR IT
	### HAVE TO PASS IT TO featureCounts

	# GTF.featureType
	#allowMultiOverlap
	#countMultiMappingReads
	#minMQS

	#others??
}

fileSelection <- function() {
	fluidRow(
		column(
			3,

			selectInput(inputId='file_location_pipeline', label='File location', 
				choices= c("Local files" = "local", "Files on server" = "server")
			),

			conditionalPanel(
				condition = "input.file_location_pipeline == 'local'"
			)			
		),

		column(
			4,

			conditionalPanel(
				condition = "input.file_location_pipeline == 'local'",

				fileInput(inputId = "local_fasta_pipeline", label = "FASTA File", multiple = FALSE, width = NULL, 
					buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
		        )
			),

			conditionalPanel(
				condition = "input.file_location_pipeline == 'server'",
				
				selectInput(inputId = 'server_fasta_pipeline', label = 'FASTA File', choices = list.files('fasta/'))
			)
		),

		column(
			5,

			conditionalPanel(
				condition="input.file_location_pipeline == 'local'",

				fileInput(inputId = 'annotation_local_pipeline', label = "Annotation File", multiple = FALSE, width = NULL, 
        			buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf', ".saf")
      		 	)
			)
		)
	)
}

parameterSelection <- function() {
	fluidRow(
		column(
			3,

			numericInput(inputId="trim_3_pipeline", "# 3' Bases to Trim:", 10, min = 0, max = 100)
		),


		column(
			3,

			numericInput(inputId="trim_5_pipeline", "# 5' Bases to Trim:", 10, min = 0, max = 100),

			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'ext'",

				selectInput(inputId='GTF_pipeline', label='Annotation Type', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			3,

			numericInput(inputId="num_mm_pipeline", "# Mismatches Allowed", 3, min = 0, max = 50)
		),

		column(
			3,

			numericInput(inputId="num_indels_pipeline", label="Max Indels Allowed", 5, min=0, max=100)
		)
	)
}


seqSelection <- function() {
	fluidRow(
		column(
			4,

			selectInput(inputId = 'paired_end_pipeline', label='Sequence Type', choices=
				c("Single End" = "se",
                  "Paired End" = "pe")
			)
		),

		column(
			4,
			
			conditionalPanel(
				condition = "input.paired_end_pipeline == 'se'",

				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="ls_1_pipeline", label="Select File", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz'))
				), 

				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="se_1_pipeline", label="Select File", choices = list.files('fastq/'))
				)
			),

			conditionalPanel(
				condition = "input.paired_end_pipeline == 'pe'",

				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="lp_1_pipeline", label="Select File 1", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
					)
				), 

				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="pe_1_pipeline", label="Select File 1", choices = list.files('fastq/'))
				)
			)
		),
 
		column(
			4, 

			conditionalPanel(
				condition="input.file_location_pipeline == 'server'",

				selectInput(inputId = 'annotation_server_pipeline', label = "Annotation File", choices = list.files('./annotation/'))
			),

			conditionalPanel(
				condition = "input.paired_end_pipeline == 'pe'",

				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="lp_2_pipeline", label="Select File 2", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
					)
				), 

				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="pe_2_pipeline", label="Select File 2", choices = list.files('fastq/'))
				)
			)
		)
	)
}

countParams <- function() {
	fluidRow(
		column(
			3,

			selectInput(inputId="annotation_selection_pipeline", label="annotation_type", choices=
				c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext"))
		),


		column(
			3,

			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'inbuilt'",

				selectInput(inputId='annotation_type_pipeline', label='Annotation Type', 
					choices= c("mm10" = "mm10", "mm9" = "mm9", "hg38" = "hg38", "hg19" = "hg19")
				)
			),

			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'ext'",

				selectInput(inputId='GTF_pipeline', label='Annotation Type', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			3,

			numericInput(inputId="num_mm_pipeline", "# Mismatches Allowed", 3, min = 0, max = 50)
		),

		column(
			3,

			numericInput(inputId="num_indels_pipeline", label="Max Indels Allowed", 5, min=0, max=100)
		)
	)
}

pipelinePage <- function() {
	tabsetPanel(
		tabPanel(
			"Config",

			fileSelection(),

			hr(),

			seqSelection(),

			parameterSelection(),

			hr(),

			countParams(),

			hr(),

			actionButton('qc_pipeline', 'View Basic QC Report'),
			actionButton('qc_full_pipline', 'View Full QC Report'),

			actionButton('start_pipeline', 'Start')
		),

		tabPanel(
			"QC Graphs",

			plotOutput("cycle_quality_pipeline"),
			plotOutput("cycle_gc_pipeline")
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
	    tabPanel("Build Index", indexPage()),
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
	    	paste0('Available memory: ', getMem())
	  	})
	}

shinyApp(ui = ui, server = server)