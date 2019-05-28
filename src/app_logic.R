#qc page logic
pipeline_page_qc <- function(input, output, session) {
	observeEvent(input$qc_pipeline, 
		{	
			progress(session)

			pattern1 <- NULL
			pattern2 <- NULL

			#showModal(modalDialog(paste0(input$file_location_pipeline, " ", input$paired_end_pipeline, " ", input$se_1_pipeline), title = "File Error"))

			filename2 = NULL

			if(input$file_location_pipeline == "local") {
				if(input$paired_end_pipeline == "se") {
				filename1 <- input$ls_1_pipeline$name
				
				files <- c(input$ls_1_pipeline$datapath)
			} else {
				filename1 <- input$lp_1_pipeline$name
				filename2 <- input$lp_2_pipeline$name
				
				files <- c(input$lp_1_pipeline$datapath, input$lp_2_pipeline$datapath)
			}
			} else {
				if(input$paired_end_pipeline == "se") {
					filename1 <- strsplit(input$se_1_pipeline, "[.]")[[1]][[1]]
				} else {
					filename1 <- strsplit(input$pe_1_pipeline, "[.]")[[1]][[1]]
					filename2 <- strsplit(input$pe_2_pipeline, "[.]")[[1]][[1]]
				}

				fastqDir <- paste0(getwd(), "/data/reads/")
					files <- list.files(fastqDir, c(filename1, filename2), full.names=TRUE)
			}

			if(is.null(filename1)) {
					showModal(modalDialog("Missing FASTQ file", title = "File Error"))

					return()
					#stop("No file selected for qc")
			}

			print(paste0("yoda ", getwd(), "/reads"))
			print(list.files(paste0(getwd(), "/data/reads/")))

			#get fastq files to qc
			qa <- rqcQA(files, workers=1)

			### FILE 1 ###
			df_1 <- rqcCycleAverageQualityCalc(qa[1])
			cycle1 <- as.numeric(levels(df_1$cycle))[df_1$cycle]

			#calculate per cycle gc % graph
			gcp_1 <- rqcCycleGCPlot(qa[1])
			cycle2 <- as.numeric(levels(gcp_1[["data"]]$cycle))[gcp_1[['data']]$cycle]

			### FILE 2 ###
			df_2 <- rqcCycleAverageQualityCalc(qa[1])
			cycle3 <- as.numeric(levels(df_2$cycle))[df_2$cycle]

			#calculate per cycle gc % graph
			gcp_2 <- rqcCycleGCPlot(qa[1])
			cycle4 <- as.numeric(levels(gcp_2[["data"]]$cycle))[gcp_2[['data']]$cycle]

			#make output graphs
			output$cycle_quality_pipeline <- renderPlot(plot(cycle1, df_1$quality, col = df_1$filename, xlab='Cycle', ylab='Quality Score'))
			output$cycle_gc_pipeline <- renderPlot(plot(cycle2, gcp_1[['data']]$gc, col = gcp_1[['data']]$filename, xlab='Cycle', ylab='% GC', type='b'))

			if(!is.null(filename2)) {
				output$cycle_quality_pipeline2 <- renderPlot(plot(cycle3, df_2$quality, col = df_2$filename, xlab='Cycle', ylab='Quality Score'))
				output$cycle_gc_pipeline2 <- renderPlot(plot(cycle4, gcp_2[['data']]$gc, col = gcp_2[['data']]$filename, xlab='Cycle', ylab='% GC', type='b'))
			}
	 	}
	)	
}

pipeline_page_full_qc <- function(input, output, session) {
	observeEvent(input$qc_full_pipline, 
  		{
  			progress(session)

  			qc_path_pipeline1 <- NULL
  			qc_path_pipeline2 <- NULL

  			if(input$file_location_pipeline == "local") {
  				if(input$paired_end_pipeline == "se") {
					qc_path_pipeline1 <- strsplit(input$ls_1_pipeline$datapath, input$ls_1_pipeline$name)
				} else {
					qc_path_pipeline1 <- strsplit(input$lp_1_pipeline$datapath, input$lp_1_pipeline$name)
					qc_path_pipeline2 <- strsplit(input$lp_2_pipeline$datapath, input$lp_2_pipeline$name)
				}
				} else {
					qc_path_pipeline1 <- paste0(my_path, 'reads')
				}

				print(qc_path_pipeline1)
				print(qc_path_pipeline2)

  			qc_results_pipeline <- rqc(path=c(qc_path_pipeline1, qc_path_pipeline2), pattern = c(".fastq", '.fastq.gz'))
 			View(qc_results_pipeline)
  	 	}
	)	
}

run_pipeline <- function(input, output, session) {
	observeEvent(input$start_pipeline, 
		{
			progress(session)

			fasta_file <- getFastaFile(input)

			index_file_name <- getFastaFileName(input)

			if(!file.exists(paste0(my_path, 'index/', index_file_name, '_index.files'))) {
				buildindex(basename=paste0(my_path, "index/", index_file_name, "_index"), reference=paste0(my_path, 'genome/', fasta_file), indexSplit=TRUE, 
					memory=setIndexMem())
			}

			seqfile1 <- getSeqFile1(input)
			seqfile2 <- getSeqFile2(input)

			name <- getSeqName1(input)

			format <- "SAM" #input$sj_format

			print(paste0(my_path, "alignments/", name, ".", format))

			subjunc(paste0(my_path, "index/", index_file_name, "_index"), readfile1=seqfile1, readfile2=seqfile2, 
				output_file = paste0(my_path, "alignments/", name, ".", format), output_format=format, maxMismatches = input$num_mm_pipeline, 
				nTrim5=input$trim_5_pipeline, nTrim3=input$trim_3_pipeline, indels=input$num_indels_pipeline, nthreads=2)

			### COUNT $$$
			files <- paste0(my_path, "alignments/", name, ".", format)
			reads <- name		

			annot_file <- getAnnotationFile(input)

			if(input$annotation_selection_pipeline == "inbuilt") {
				fc <- featureCounts(files, annot.inbuilt=input$annotation_type_pipeline, isPairedEnd=is_pe(input), useMetaFeatures=input$meta_features)
			} else {
				fc <- featureCounts(files, annot.ext=annot_file, isGTFAnnotationFile=TRUE, nthreads=2)
			}

			if(dir.exists(paste0(my_path, "counts/", reads)) == FALSE) {
				dir.create(paste0(my_path, "counts/", reads))
			} 

			write.table(fc[['counts']], paste0(my_path, "counts/", reads, "/", "counts.txt"), sep="\t")
			write.table(fc[['annotation']], paste0(my_path, "counts/", reads, "/", "annotation.txt"), sep="\t")
			write.table(fc[['targets']], paste0(my_path, "counts/", reads, "/", "targets.txt"), sep="\t")
			write.table(fc[['stat']], paste0(my_path, "counts/", reads, "/", "stats.txt"), sep="\t")

			pretty_string <- ""

			fc_stats <- fc[['stat']]

			for(i in 1:nrow(fc_stats)) {
				pretty_string <- paste0(pretty_string, fc_stats[i, 1], ": ", fc_stats[i, 2], "\n <br> \n")
			}

			output$download_button <- renderUI({downloadButton("download_pipeline", "Download Data")})

			showModal(modalDialog(HTML(pretty_string), title="Feature Counts Results"))
	 	}
	)	
}

pipeline_page_download <- function(input, output, session) {
	##### TODO: INCLUDE BED FILE IN DOWNLOAD PACKAGE
	output$download_pipeline <- downloadHandler(
  		filename <- function() {
    		paste0(getSeqName1(input), ".zip")
  		},

	  	content <- function(file) {
	    	sam_file <- paste0(my_path, "alignments/", getSeqName1(input), ".", "SAM")
	    	counts_file <- paste0(my_path, "counts/", getSeqName1(input), "/counts.txt")
  			annotation_file <-  paste0(my_path, "counts/", getSeqName1(input), "/annotation.txt")
  			targets_file <-  paste0(my_path, "counts/", getSeqName1(input), "/targets.txt")
  			stats_file <-  paste0(my_path, "counts/", getSeqName1(input), "/stats.txt")
  			
	    	files <- c(sam_file, counts_file, annotation_file, targets_file, stats_file)
	    	zip(file, files)
	  	},
	  	contentType <- "application/zip"
	)
}

counts_page_count <- function(input, output, session) {
	observeEvent(input$count_features, 
  		{
  			progress(session)

  			### COUNT $$$
  			alignment_file_1 <- getAlignmentFile1(input)
  			alignment_file_2 <- NULL #getAlignmentFile2()

  			name <- getName(input)

  			files <- c(alignment_file_1, alignment_file_2) #paste0(my_path, "alignments/", name, ".", format)

  			annot_file <- getAnnotationFile(input)

  			print(annot_file)
  			print(files)
  			print(input$nav)

  			if(input$annotation_type_counts == "inbuilt") {
	  			fc <- featureCounts(files, annot.inbuilt=input$annotation_type_counts, isPairedEnd=is_pe(input), useMetaFeatures=input$meta_features_counts)
  			} else {
	  			fc <- featureCounts(files, annot.ext=annot_file, isGTFAnnotationFile=TRUE, nthreads=2)
  			}

  			if(dir.exists(paste0(my_path, "counts/", name)) == FALSE) {
					dir.create(paste0(my_path, "counts/", name))
  			} 

			write.table(fc[['counts']], paste0(my_path, "counts/", name, "/", "counts.txt"), sep="\t")
			write.table(fc[['annotation']], paste0(my_path, "counts/", name, "/", "annotation.txt"), sep="\t")
			write.table(fc[['targets']], paste0(my_path, "counts/", name, "/", "targets.txt"), sep="\t")
			write.table(fc[['stat']], paste0(my_path, "counts/", name, "/", "stats.txt"), sep="\t")

  			pretty_string <- ""

  			fc_stats <- fc[['stat']]

  			for(i in 1:nrow(fc_stats)) {
  				pretty_string <- paste0(pretty_string, fc_stats[i, 1], ": ", fc_stats[i, 2], "\n <br> \n")
  			}

  			output$download_button_counts <- renderUI({downloadButton("download_counts", "Download Data")})

  			showModal(modalDialog(HTML(pretty_string), title="Feature Counts Results"))
  		}
  	)
}

counts_page_download <- function(input, output, session) {
  	output$download_counts <- downloadHandler(
  		filename <- function() {
    		paste0(getAlignmentFileName(input), ".zip")
  		},

	  	content <- function(file) {
	    	sam_file1 <- paste0(my_path, "alignments/", getAlignmentFileName(input), ".", "SAM")
	    	sam_file2 <- getAlignmentFile2()
	    	counts_file <- paste0(my_path, "counts/", getAlignmentFileName(input), "/counts.txt")
  			annotation_file <- paste0(my_path, "counts/", getAlignmentFileName(input), "/annotation.txt")
  			targets_file <- paste0(my_path, "counts/", getAlignmentFileName(input), "/targets.txt")
  			stats_file <- paste0(my_path, "counts/", getAlignmentFileName(input), "/stats.txt")
  			
	    	files <- c(sam_file1, sam_file2, counts_file, annotation_file, targets_file, stats_file)
	    	zip(file, files)
	  	},
	  	contentType <- "application/zip"
	)
}

mapping_page_qc <- function(input, output, session) {
	observeEvent(input$qc_mapping, 
		{
			progress(session)

  			pattern1 <- NULL
  			pattern2 <- NULL

  			#showModal(modalDialog(paste0(input$file_location_mapping, " ", input$paired_end_mapping, " ", input$se_1_mapping), title = "File Error"))

  			filename2 = NULL

  			if(input$file_location_mapping == "local") {
  				if(input$paired_end_mapping == "se") {
					filename1 <- input$ls_1_mapping$name
					
					files <- c(input$ls_1_mapping$datapath)
				} else {
					filename1 <- input$lp_1_mapping$name
					filename2 <- input$lp_2_mapping$name
					
					files <- c(input$lp_1_mapping$datapath, input$lp_2_mapping$datapath)
				}
			} else {
				if(input$paired_end_mapping == "se") {
					filename1 <- strsplit(input$se_1_mapping, "[.]")[[1]][[1]]
				} else {
					filename1 <- strsplit(input$pe_1_mapping, "[.]")[[1]][[1]]
					filename2 <- strsplit(input$pe_2_mapping, "[.]")[[1]][[1]]
				}

				fastqDir <- paste0(getwd(), "/data/reads/")
  				files <- list.files(fastqDir, c(filename1, filename2), full.names=TRUE)
			}

			if(is.null(filename1)) {
  				showModal(modalDialog("Missing FASTQ file", title = "File Error"))

  				return()
  				#stop("No file selected for qc")
			}

			print(paste0("test ", getwd(), "/reads"))
			print(list.files(paste0(getwd(), "/data/reads/")))

  			#get fastq files to qc
  			qa <- rqcQA(files, workers=1)

  			#calculate per cycle quality 


  			### FILE 1 ###
  			df_1 <- rqcCycleAverageQualityCalc(qa[1])
			cycle1 <- as.numeric(levels(df_1$cycle))[df_1$cycle]

			#calculate per cycle gc % graph
			gcp_1 <- rqcCycleGCPlot(qa[1])
			cycle2 <- as.numeric(levels(gcp_1[["data"]]$cycle))[gcp_1[['data']]$cycle]

			### FILE 2 ###
			df_2 <- rqcCycleAverageQualityCalc(qa[1])
			cycle3 <- as.numeric(levels(df_2$cycle))[df_2$cycle]

			#calculate per cycle gc % graph
			gcp_2 <- rqcCycleGCPlot(qa[1])
			cycle4 <- as.numeric(levels(gcp_2[["data"]]$cycle))[gcp_2[['data']]$cycle]

			#make output graphs
			output$cycle_quality_mapping <- renderPlot(plot(cycle1, df_1$quality, col = df_1$filename, xlab='Cycle', ylab='Quality Score'))
			output$cycle_gc_mapping <- renderPlot(plot(cycle2, gcp_1[['data']]$gc, col = gcp_1[['data']]$filename, xlab='Cycle', ylab='% GC', type='b'))

			if(!is.null(filename2)) {
				output$cycle_quality <- renderPlot(plot(cycle3, df_2$quality, col = df_2$filename, xlab='Cycle', ylab='Quality Score'))
				output$cycle_gc <- renderPlot(plot(cycle4, gcp_2[['data']]$gc, col = gcp_2[['data']]$filename, xlab='Cycle', ylab='% GC', type='b'))
			}
		}
	)
}

mapping_page_full_qc <- function(input, output, session) {
	observeEvent(input$qc_full_mapping, 
		{
			progress(session)

  			qc_path_mapping1 <- NULL
  			qc_path_mapping2 <- NULL

  			if(input$file_location_mapping == "local") {
  				if(input$paired_end_mapping == "se") {
					qc_path_mapping1 <- strsplit(input$ls_1_mapping$datapath, input$ls_1_mapping$name)
				} else {
					qc_path_mapping1 <- strsplit(input$lp_1_mapping$datapath, input$lp_1_mapping$name)
					qc_path_mapping2 <- strsplit(input$lp_2_mapping$datapath, input$lp_2_mapping$name)
				}
				} else {
					qc_path_mapping1 <- paste0(my_path, 'reads')
				}

				print(qc_path_mapping1)
				print(qc_path_mapping2)

  			qc_results_mapping <- rqc(path=c(qc_path_mapping1, qc_path_mapping2), pattern = c(".fastq", '.fastq.gz'))
 			View(qc_results_mapping)
		}
	)
}

mapping_page_run <- function(input, output, session) {
	observeEvent(input$start_mapping_mapping, 
		{
			progress(session)

  			fasta_file <- getFastaFile(input)

  			index_file_name <- getFastaFileName(input)

  			if(!file.exists(paste0(my_path, 'index/', index_file_name, '_index.files'))) {
  				buildindex(basename=paste0(my_path, "index/", index_file_name, "_index"), reference=paste0(my_path, 'genome/', fasta_file), indexSplit=TRUE, 
  					memory=setIndexMem())
  			}

  			seqfile1 <- getSeqFile1(input)
  			seqfile2 <- getSeqFile2(input)

  			name <- getSeqName1(input)

  			format <- "SAM" #input$sj_format

  			print(paste0(my_path, "alignments/", name, ".", format))

  			sj <- subjunc(paste0(my_path, "index/", index_file_name, "_index"), readfile1=seqfile1, readfile2=seqfile2, 
  				output_file=paste0(my_path, "alignments/", name, ".", format), output_format=format, maxMismatches=input$num_mm_mapping, 
  				nTrim5=input$trim_5_mapping, nTrim3=input$trim_3_mapping, indels=input$num_indels_mapping)

  			pretty_string <- ""

  			output$download_button_mapping <- renderUI({downloadButton("download_mapping", "Download Data")})

  			#fc_stats = fc[['stat']]

  			#for(i in 1:nrow(fc_stats)) {
  			#	pretty_string <- paste0(pretty_string, fc_stats[i, 1], ": ", fc_stats[i, 2], "\n <br> \n")
  			#}

  			#output$download_button_counts <- renderUI({downloadButton("download_counts", "Download Data")})

  			#showModal(modalDialog(HTML(pretty_string), title="Feature Counts Results"))
		}
	)	
}	

mapping_page_download <- function(input, output, session) {
	##### TODO: INCLUDE BED FILE IN DOWNLOAD PACKAGE
	output$download_mapping <- downloadHandler(
		filename <- function() {
		paste0(getSeqName1(input), ".zip")
		},

	  	content <- function(file) {
	    	sam_file <- paste0(my_path, "alignments/", getSeqName1(input), ".", "SAM")
				
	    	files <- c(sam_file)
	    	zip(file, files)
	  	},
	  	contentType <- "application/zip"
	)
}