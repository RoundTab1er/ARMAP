#my_path = paste0(getwd(), '/data/')
#my_path = '/home/garcian/thesis/r_workspace/'

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

					selectInput(inputId = 'index_file_mapping', label = "Index File", choices = list.files(paste0(my_path, 'index/'), ".files"))
				)
			),

			hr(),

			fluidRow(
				column(
					3,
					style="margin-top:0px",

					selectInput(inputId = 'paired_end_mapping', label='Sequence Type', choices=
								c("Single End" = "se",
				                  "Paired End" = "pe")
					)
				),

				column(
					4, 
					style="margin-top:0px",

					conditionalPanel(
						condition = "input.paired_end_mapping == 'se'",

						conditionalPanel(
							condition="input.file_location_mapping == 'local'",

							fileInput(inputId="ls_1_mapping", label="Select File", multiple = FALSE, width = NULL, 
								buttonLabel = "Browse...", placeholder = "No file selected", accept = c('fastq', '.fastq.gz'))
						), 

						conditionalPanel(
							condition="input.file_location_mapping == 'server'",

							selectInput(inputId="se_1_mapping", label="Select File", choices = list.files(paste0(my_path, 'reads/')))
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

							selectInput(inputId="pe_1_mapping", label="Select File 1", choices = list.files(paste0(my_path, 'reads/')))
						)
					)
				),

				column(
					5, 
					style="margin-top:0px",

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

							selectInput(inputId="pe_2_mapping", label="Select File 2", choices = list.files(paste0(my_path, 'reads/')))
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

			uiOutput("download_button_mapping")
		),

		tabPanel(
			"Basic QC Report",

			plotOutput("cycle_quality"),
			plotOutput("cycle_gc")
		)
	)
}