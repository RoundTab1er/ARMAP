#my_path = paste0(getwd(), '/data/')
#my_path = '/home/garcian/thesis/r_workspace/'

countsRow3 <- function() {
	fluidRow(
		column(
			3,

			selectInput(inputId='meta_features_counts', label='Use Meta-Features', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		),


		column(
			3,

			selectInput(inputId='feature_type_counts', label='Feature Type', 
				choices= c("Exon" = 'exon')
			)
		),

		column(
			3,

			selectInput(inputId='allow_overlap_counts', label='Allow Multi-Overlap', 
				choices= c("No" = FALSE, "Yes" = TRUE)
			)
		),

		column(
			3,

			selectInput(inputId='attr_type_counts', label='Attribute Type', 
				choices= c("gene_id" = 'gid', "name" = 'n')
			)
		)
	)
}

countsRow2 <- function() {
	fluidRow(
		column(
			4,

			selectInput(inputId='annotation_type_counts', label='Annotation Type', 
				choices= c("Inbuilt Annotation" = "inbuilt", "External Annotation" = "ext")
			)
		),

		column(
			3,

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
			5,

			conditionalPanel(
				condition="input.annotation_type_counts == 'ext'",

				conditionalPanel(
					condition="input.file_location_counting == 'server'",

					selectInput(inputId='annotation_counts', label='Annotation File', 
						choices= list.files(paste0(my_path, "annotations/"))
					)
				),

				conditionalPanel(
					condition="input.file_location_counting == 'local'",

					fileInput(inputId="annotation_file_counts_local", label="Select File", multiple = FALSE, width = NULL, 
						buttonLabel="Browse...", placeholder="No file selected", accept=c('.sam', '.bam')
					)
				)
			)
		)
	)
}

countsRow1 <- function() {
	fluidRow(
		column(
			4,

			selectInput(inputId='file_location_counting', label='File location', 
				choices= c("Files on my computer" = "local", "Files on server" = "server")
			)
		),

		column(
			4,

			conditionalPanel(
				condition="input.file_location_counting == 'server'",

				selectInput(inputId='alignment_file_counts', label='Alignment File', 
					choices= list.files(paste0(my_path, "alignments/"))
				)
			),

			conditionalPanel(
				condition="input.file_location_counting == 'local'",

				fileInput(inputId="alignment_file_counts_local", label="Alignment File", multiple = FALSE, width = NULL, 
					buttonLabel = "Browse...", placeholder = "No file selected", accept = c('.sam', '.bam')
				)
			)
		)
	)
}

countsPage <- function() {

	fluidPage(
		countsRow1(),
		countsRow2(),
		countsRow3(),

		hr(),

		actionButton('count_features', 'Count Features'),

		uiOutput("download_button_counts")
	)

	### HAVE TO WORK OUT ANNOTATION FILE --> IF INBUILT DOESN'T MAKE SENSE TO ASK FOR IT
	### HAVE TO PASS IT TO featureCounts

	# GTF.featureType
	#allowMultiOverlap
	#countMultiMappingReads
	#minMQS

	#others??
}