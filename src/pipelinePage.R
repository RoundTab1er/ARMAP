print(paste0(my_path, 'reads'))
print(list.files(paste0(my_path, 'reads')))

# ROW #1 --> FASTA FILE SELECTION
fileSelection <- function() {
	fluidRow(
		column(
			4,

			#Select file location --> local or remote
			selectInput(inputId='file_location_pipeline', label='File location', 
				choices= c("Local files" = "local", "Files on server" = "server")
			)	
		),

		column(
			8,

			# IF LOCAL, upload file
			conditionalPanel(
				condition = "input.file_location_pipeline == 'local'",

				fileInput(inputId = "local_fasta_pipeline", label = "FASTA File", multiple = FALSE, width = NULL, 
					buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fasta')
		        )
			),

			# IF REMOTE, select file from fasta folder
			conditionalPanel(
				condition = "input.file_location_pipeline == 'server'",
				
				selectInput(inputId = 'server_fasta_pipeline', label = 'FASTA File', choices = list.files(paste0(my_path, 'genome/')))
			)
		)
	)
}

# SELECT RNA-SEQ DATA FILES
seqSelection <- function() {
	fluidRow(
		column(
			3,
			#style="margin-top:-50px",

			# SELECT PAIRED-END or SINGLE-END READS 
			selectInput(inputId = 'paired_end_pipeline', label='Sequence Type', choices=
				c("Single End" = "se",
                  "Paired End" = "pe")
			)
		),

		column(
			4,
			#style="margin-top:-50px",
			
			# IF SINGLE-END, select one reads file
			conditionalPanel(
				condition = "input.paired_end_pipeline == 'se'",

				# IF LOCAL, select from computer
				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="ls_1_pipeline", label="Select File", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz'))
				), 

				# IF SERVER, select from fastq folder
				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="se_1_pipeline", label="Select File", choices = list.files(paste0(my_path, 'reads')))
				)
			),

			# IF SINGLE-END, select two reads files
			conditionalPanel(
				condition = "input.paired_end_pipeline == 'pe'",

				# IF LOCAL, select from computer
				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="lp_1_pipeline", label="Select File 1", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz')
					)
				), 

				# IF SERVER, select from fastq folder
				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="pe_1_pipeline", label="Select File 1", choices = list.files(paste0(my_path, 'reads')))
				)
			)
		),
 
 		#select second seq file
		column(
			5, 
			#style="margin-top:-50px",

			conditionalPanel(
				condition = "input.paired_end_pipeline == 'pe'",

				#select local
				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId="lp_2_pipeline", label="Select File 2", multiple = FALSE, width = NULL, 
						buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.fastq', '.fastq.gz')
					)
				), 

				#select remote
				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId="pe_2_pipeline", label="Select File 2", choices = list.files(paste0(my_path, 'reads')))
				)
			)
		)
	)
}

#select mapping parameters
parameterSelection <- function() {
	fluidRow(
		column(
			3,
			style="margin-top:-10px",

			#number of 3' bases to trim
			numericInput(inputId="trim_3_pipeline", "# 3' Bases to Trim:", 10, min = 0, max = 100)
		),


		column(
			3,
			style="margin-top:-10px",

			#number of 5' bases to trim
			numericInput(inputId="trim_5_pipeline", "# 5' Bases to Trim:", 10, min = 0, max = 100)
		),

		column(
			3,
			style="margin-top:-10px",

			#max number of mismatches to allow
			numericInput(inputId="num_mm_pipeline", "# Mismatches Allowed", 3, min = 0, max = 50)
		),

		column(
			3,
			style="margin-top:-10px",

			#max indels to allow
			numericInput(inputId="num_indels_pipeline", label="Max Indels Allowed", 5, min=0, max=100)
		)
	)
}

#Set up counting
countFiles <- function() {
	fluidRow(
		column(
			4,

			#select type of annotation, internal or external
			selectInput(inputId="annotation_selection_pipeline", label="Annotation Type", choices=
				c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext"))
		),

		column(
			3,

			#if internal annotation, select the format
			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'inbuilt'",

				selectInput(inputId='annotation_type_pipeline', label='Annotation Format', 
					choices= c("mm10" = "mm10", "mm9" = "mm9", "hg38" = "hg38", "hg19" = "hg19")
				)
			),

			#if external annotation, select format
			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'ext'",

				selectInput(inputId='GTF_pipeline', label='Annotation Format', 
					choices= c("SAF" = FALSE, "GTF" = TRUE)
				)
			)
		),

		column(
			5,

			#if external, select annotation file
			conditionalPanel(
				condition="input.annotation_selection_pipeline == 'ext'",

				#local
				conditionalPanel(
					condition="input.file_location_pipeline == 'local'",

					fileInput(inputId = 'annotation_local_pipeline', label = "Annotation File", multiple = FALSE, width = NULL, 
	        			buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.gff', '.gff3', '.gtf', ".saf")
	      		 	)
				),

				#remote
				conditionalPanel(
					condition="input.file_location_pipeline == 'server'",

					selectInput(inputId = 'annotation_server_pipeline', label = "Annotation File", choices = list.files(paste0(my_path, 'annotations/')))
				)
			)
		)
	)
}

#set up counting params
countParams <- function() {
	fluidRow(
		column(
			3,

			#type of feature to use
			selectInput(inputId='meta_features', label='Use Meta-Features', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		),


		column(
			3,

			#feature name
			selectInput(inputId='feature_type', label='Feature Type', 
				choices= c("Exon" = 'exon')
			)
		),

		column(
			3,

			#multiple overlap
			selectInput(inputId='allow_overlap', label='Allow Multiple Overlap', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		),

		column(
			3,

			#set 11th field type
			selectInput(inputId='attr_type', label='Attribute Type', 
				choices= c("gene_id" = 'gid', "name" = 'n')
			)
		)
	)
}

#define tabset
pipelinePage <- function() {
	tabsetPanel(
		id="tabs",

		#main panel with configuration
		tabPanel(
			"Pipeline Config",

			fileSelection(),

			hr(),

			seqSelection(),

			parameterSelection(),

			hr(),

			countFiles(),

			countParams(),

			hr(),

			actionButton('qc_pipeline', 'View Basic QC Report'),
			actionButton('qc_full_pipline', 'View Full QC Report'),

			actionButton('start_pipeline', 'Start'),

			hr(),

			uiOutput("download_button")
		),

		#secondary tab showing basic qc output
		tabPanel(
			"QC Graphs",

			plotOutput("cycle_quality_pipeline"),
			plotOutput("cycle_gc_pipeline"),

			plotOutput("cycle_quality_pipeline2"),
			plotOutput("cycle_gc_pipeline2")
		)
	)
}