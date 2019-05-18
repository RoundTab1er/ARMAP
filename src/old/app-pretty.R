### NOTES:
##	NOT IMPLEMENTED:
#	 - file specific qc, not batch
#
##	ISSUES:
#	 - cannot select some files from server and some from pc
#	 - no exception handling
#
##	DONE:
#	 - buildIndex fully implemented
#	 - subjunc fully implemented
#	 - download fully implemented (may break out into two separate buttons in the future)
#	 - featureCounts working
#	 - allows ouput to BAM
#	 - doesn't work with paired end reads
#
##	NOT TESTED:
#    - currently cannot use the "local" option
#		- have to set all occurences of annotation, read1, read2, fasta, and index to use temp locations

#load required libraries
library(shiny)
library(Rqc)
library(Rsubread)

ui <- fluidPage(
	titlePanel("ARMAP", windowTitle="ARMAP"),

	hr(),

	# *Input() functions

	fluidRow(
		#Reference Genome Files
		column(
			3,
			style="margin-top:-20px",

			h4("Reference Genome"),

			### select local or server files ###
			selectInput(inputId='file_location', label='File location', 
				choices= c("Files on my computer" = "local", "Files on server" = "server")
			)
		),

		column(
			3,
			style="margin-top:-20px",

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
		),

		column(
			4, 
			style="margin-top:-20px",

			br(),
			br(),

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
		)
	),

	#index/mapping config
	fluidRow(
		column(
			3,
			style="margin-top:-20px",

			selectInput(inputId = 'index', label = "Build Index", choices = c("Yes" = "True", "No" = "False"))
		),

		column(
			4,
			style="margin-top:-20px",
			
			conditionalPanel(
				condition="input.index == 'True'",

				selectInput(inputId="split_index", label="Split Index", choices=c("Yes" = "True", "No" = "False"))
			),

			conditionalPanel(
				condition="input.index == 'False'",

				selectInput(inputId = 'index_file', label = "Index File", choices = list.files('./index/', ".files"))
			)
		),

		column(
			3,
			style="margin-top:-20px",

			br(),

			conditionalPanel(
				condition="input.index == 'True'",

				conditionalPanel(
					condition="input.split_index == 'True'",
			
					radioButtons(inputId="memory", label="Size of Index Blocks", choices = c("1GB" = 1000, "2GB" = "2000", "3GB" = 3000), selected = "1GB", inline=TRUE)

					#sliderInput("memory", "Amount of RAM to use",
			    	#	min = 0, max = 4000, value = 1000)
				)
			)
		)
	),

	hr(),

	#RNA-Seq File Selection
	fluidRow(
		column(
			3,
			style="margin-top:-50px",

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
			3, 
			style="margin-top:-50px",

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
			3,
			style="margin-top:-50px",

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
			2,
			style="margin-top:-10px",

			selectInput(inputId="trim", label="Trim Reads", choice=c("Yes" = "True", "No" = "False"))
		),

		column(
			2,
			style="margin-top:-10px",

			conditionalPanel( 
				condition="input.trim == 'True'",

				numericInput(inputId="trim_3", "# 3' Bases to Trim:", 10, min = 0, max = 100)
			)
		),

		column(
			2,
			style="margin-top:-10px",

			conditionalPanel( 
				condition="input.trim == 'True'",

				numericInput(inputId="trim_5", "# 5' Bases to Trim:", 10, min = 0, max = 100)
			)
		),

		column(
			3, 
			style="margin-top:-10px",

			numericInput(inputId="num_mm", "Max # Mismatches to Allow", 3, min = 0, max = 50)
		),

		column(
			2,
			style="margin-top:-10px",

			selectInput(inputId="sj_format", label="Output Format", choice=c("SAM (text-based)" = "sam", "BAM (binary)" = "bam"))
		),

		column(
			1,
			style="margin-top:-10px",

			numericInput(inputId="num_indels", label="Max Indels Allowed", 5, min=0, max=100)
		)
	),

	actionButton('qc', 'View Basic QC Report'),
	actionButton('qc_full', 'View Full QC Report'),

	actionButton('start_mapping', 'Begin alignment'),
	actionButton('start_counting', 'Make featureCounts'),

	hr(),

	downloadButton("download_align", "Download Alignment"),
	downloadButton("download_counts", "Download Feature Counts"),

	br(),

	#counting config
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

	),

	### TEST OUTPUT ###
	plotOutput("cycle_quality"),
	plotOutput("cycle_gc"),

	### DEBUG OUTPUT ###
	textOutput("fileselected"),
	textOutput("text1"),
	textOutput("text2"),

	hr()

	#textOutput("bi_output"),
	#textOutput("sj_output")


	#sidebarLayout(

    # Sidebar with a slider input
    #sidebarPanel(
    #  sliderInput("obs",
    #              "Number of observations:",
    #              min = 0,
    #              max = 1000,
    #              value = 500)
    #),

    # Show a plot of the generated distribution
    #mainPanel(
    #  plotOutput("distPlot")
    #)
  #)
)

server <- function(input, output, session) {
	### PRINT DEBUG TEXT ###
	output$fileselected <- renderText({
    	paste0('You have selected: ', input$annotation_local$datapath)
  	})

  	output$text1 <- renderText({
    	paste0('You have selected: ', input$annotation_server)
  	})

  	output$text2 <- renderText({
    	paste0('You have selected: ', strsplit(input$se_1, "[.]")[[1]][[2]])
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

  			#print(num_bases_3)

  			if(input$index == "True") {
  				if(input$file_location == "server") {
  					filename <- strsplit(input$server_fasta, "[.]")[[1]][[1]] #"psuedo_assembly"
  					filename <- paste0("fasta/", filename, ".fasta")
  				} else {
  					filename = input$local_fasta$datapath
  				}

  				if(input$split_index == "True") {
  					bi = buildindex(basename=paste0("index/", filename, "_index"), reference=filename, indexSplit=TRUE, memory=input$memory)
  				} else {
  					bi = buildindex(basename=paste0("index/", filename, "_index"), reference=filename)
  				}
  			} else {
  				index_file_name = strsplit(input$index_file, "[.]")[[1]][[1]]
  			}
  			
  			#format = strsplit(input$se_1, "[.]")[[1]][[2]]

  			if(input$trim != "True") {
  				num_bases_5 = 0
  				num_bases_3 = 0
  			} else {
  				num_bases_3 = input$trim_3
  				num_bases_5 = input$trim_5
  			}

  			if(input$paired_end_local == "pe") {
  				if(input$file_location == "local") {
  					rf1 = strsplit(input$lp_1$datapath, "[.]")[[1]][[1]]
  					rf2 = strsplit(input$lp_2$datapath, "[.]")[[1]][[1]]
  				} else {
	  				reads_1 = strsplit(input$pe_1, "[.]")[[1]][[1]]
	  				reads_2 = strsplit(input$pe_2, "[.]")[[1]][[1]]

	  				rf1 = paste0("fastq/", reads_1, ".fastq")
  					rf2 = paste0("fastq/", reads_2, ".fastq")
	  			}
  			} else {
  				if(input$file_location == "local") {
  					rf1 = strsplit(input$ls_1, "[.]")[[1]][[1]]
  					rf2 = NULL
  				} else {
	  				reads = strsplit(input$se_1, "[.]")[[1]][[1]] #"psuedo_assembly"
	  				rf1 = paste0("fastq/", reads, ".fastq")
	  				rf2 = NULL
  				}
  			}

  			if(input$index == "True") {
  				sj = subjunc(paste0("index/", filename, "_index"), readfile1=rf1, readfile2=rf2, output_file = paste0("alignments/", rf1, ".", input$sj_format), 
  					output_format =input$sj_format, maxMismatches = input$num_mm, nTrim5=num_bases_5, nTrim3=num_bases_3, indels=input$num_indels)
  			} else {
  				sj = subjunc(paste0("index/", index_file_name), readfile1=rf1, readfile2=rf2, output_file = paste0("alignments/", rf1, ".", input$sj_format), 
  					output_format=input$sj_format, maxMismatches = input$num_mm, nTrim5=num_bases_5, nTrim3=num_bases_3, indels=input$num_indels)
  			}  		

  			pretty_string = ""

  			for(i in 1:nrow(sj)) {
  				pretty_string = paste0(pretty_string, sj[i, 1], ": ", sj[i, 2], "\n <br> \n")
  			}

  			showModal(modalDialog(HTML(pretty_string), title = "Alignment Results"))

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
  			#reads = strsplit(input$se_1, "[.]")[[1]][[1]]
  			#files = paste0("alignments/", reads, ".", input$sj_format)

  			if(input$file_location == "local") {

  			} else {
	  			files = paste0("alignments/", input$alignment_file)
	  			reads = strsplit(input$alignment_file, "[.]")[[1]][[1]]  			
	  		}

  			if(input$paired_end_local == "pe") {
  				is_pe = TRUE
  			} else {
  				is_pe = FALSE
  			}

   			if(input$annotation_type == "inbuilt") {
	  			fc = featureCounts(files, annot.inbuilt=input$inbuilt_type, isPairedEnd=is_pe, useMetaFeatures=input$meta_features)
  			} else {
  				if(input$file_location == "local") {
  					annot_file = input$annotation_local$datapath
  				} else {
  					annot_file = paste0("annotation/", input$annotation_server)
  				}

	  			fc = featureCounts(files, annot.ext=annot_file, isGTFAnnotationFile=TRUE)
	  			#fc = featureCounts("alignments/reads.sam", annot.ext="annotation/Gansp1_all_genes_20110326.gff", isGTFAnnotationFile=TRUE)
	  			#fc = featureCounts(files, annot.ext=paste0("annotation/", input$annotation_server), isGTFAnnotationFile=input$GTF, isPairedEnd=is_pe, useMetaFeatures=input$meta_features)
  			}

  			if(dir.exists(paste0("counts/", reads)) == FALSE) {
  				dir.create(paste0("counts/", reads))
  			} 

			write.table(fc[['counts']], paste0("counts/", reads, "/", "counts.txt"), sep="\t")
			write.table(fc[['annotation']], paste0("counts/", reads, "/", "annotation.txt"), sep="\t")
			write.table(fc[['targets']], paste0("counts/", reads, "/", "targets.txt"), sep="\t")
			write.table(fc[['stat']], paste0("counts/", reads, "/", "stats.txt"), sep="\t")

  			pretty_string = ""

  			fc_stats = fc[['stat']]

  			for(i in 1:nrow(fc_stats)) {
  				pretty_string = paste0(pretty_string, fc_stats[i, 1], ": ", fc_stats[i, 2], "\n <br> \n")
  			}

			showModal(modalDialog(HTML(pretty_string), title = "Feature Counts Results"))
  		} 
  	)

  	output$download_align <- downloadHandler(
  		filename <- function() {
    		paste0(strsplit(input$alignment_file, "[.]")[[1]][[1]], ".zip")
  		},

	  	content <- function(file) {
	    	sam_file = paste0("alignments/", strsplit(input$alignment_file, "[.]")[[1]][[1]], ".", input$sj_format)
  			
	    	files = c(sam_file)
	    	zip(file, files)
	  	},
	  	contentType = "application/zip"
	)

	output$download_counts <- downloadHandler(
  		filename <- function() {
    		paste0(strsplit(input$alignment_file, "[.]")[[1]][[1]], ".zip")
  		},

	  	content <- function(file) {
  			counts_file = paste0("counts/", strsplit(input$alignment_file, "[.]")[[1]][[1]], "/counts.txt")
  			annotation_file = paste0("counts/", strsplit(input$alignment_file, "[.]")[[1]][[1]], "/annotation.txt")
  			targets_file = paste0("counts/", strsplit(input$alignment_file, "[.]")[[1]][[1]], "/targets.txt")
  			stats_file = paste0("counts/", strsplit(input$alignment_file, "[.]")[[1]][[1]], "/stats.txt")

	    	files = c(counts_file, annotation_file, targets_file, stats_file)
	    	zip(file, files)
	  	},
	  	contentType = "application/zip"
	)
}

shinyApp(ui = ui, server = server)