### Defines helper logic for app output

#navpanel tab names
PIPELINE_STRING = "Automated Pipeline"
MAPPING_PAGE = "Map Reads"
COUNTS_PAGE = "Count Features"

#' Returns path to fasta file based on active page
#'
#' @param input List of Shiny input objects
#' @return Path of fasta file on active page
getFastaFile <- function(input) {
	fastaFile = NULL

	if(input$file_location_pipeline == "local") {
		fastaFile = input$local_fasta_pipeline$datapath
	} else {
		fastaFile = input$server_fasta_pipeline
	}

	return(fastaFile)
}

#' Returns file name of fasta file on active page
#'
#' @param input List of Shiny input objects
#' @return Name of fasta file on active page
getFastaFileName <- function(input) {
	name = NULL

	if(input$file_location_pipeline == "local") {
		name = strsplit(input$local_fasta_pipeline$datapath, input$local_fasta_pipeline$name)[[1]][[1]]
	} else {
		name = strsplit(input$server_fasta_pipeline, '[.]')[[1]][[1]]
	}

	return(name)
}

#' Returns path of index file on mapping page
#'
#' @param input List of Shiny input objects
#' @return Path of index file on mapping page
getIndexFile <- function(input) {
	return(input$index_file_mapping)
}

#' Returns path of annotation file on active page
#'
#' @param input List of Shiny input objects
#' @return Path of annotation file on active page
getAnnotationFile <- function(input) {
	activePage = input$nav
	annot_file = NULL

	if(activePage == PIPELINE_STRING) {
		if(input$file_location_pipeline == "local") {
			annot_file <- input$annotation_local_pipeline$datapath
		} else {
			annot_file <- paste0(my_path, 'annotations/', input$annotation_server_pipeline)
		}
	} else if(activePage == COUNTS_PAGE) {
		if(input$file_location_counting == "local") {
			annot_file = input$annotation_file_counts_local$datapath #strsplit(input$annotation_file_counts_local$datapath, input$annotation_file_counts_local$name)[[1]][[1]]
		} else {
			annot_file = paste0(my_path, 'annotations/', input$annotation_counts) #strsplit(input$annotation_counts, '[.]')[[1]][[1]]
		}
	}

	return(annot_file)
}

#' Returns path of alignment file on active page
#'
#' @param input List of Shiny input objects
#' @return Path of alignment file
getAlignmentFile1 <- function(input) {
	activePage <- input$nav
	alignmentFile <- NULL

	if(activePage == COUNTS_PAGE) {
		if(input$file_location_counting == "local") {
			alignmentFile <- input$annotation_local_mapping$datapath
		} else {
			alignmentFile <- paste0(my_path, 'alignments/', input$alignment_file_counts)
		}
	}

	return(alignmentFile)
}

#' Returns file name of alignment file on active page
#'
#' @param input List of Shiny input objects
#' @return Name of alignment file on active page
getName <- function(input) {
	name = NULL

	if(input$file_location_counting == "local") {
		name = strsplit(input$alignment_file_counts_local$datapath, input$alignment_file_counts_local$name)[[1]][[1]]
	} else {
		name = strsplit(input$alignment_file_counts, '[.]')[[1]][[1]]
	}

	return(name)
}

#' Returns file path of sequence file 1 file on active page
#'
#' @param input List of Shiny input objects
#' @return Path of sequence file 1 on active page
getSeqFile1 <- function(input) {
	activePage <- input$nav
	seqfile1 <- NULL

	if(activePage == PIPELINE_STRING) {
		if(input$file_location_pipeline == "local") {
			if(input$paired_end_pipeline == "se") {
				seqfile1 <- input$ls_1_pipeline$datapath
			} else {
				seqfile1 <- input$lp_1_pipeline$datapath
			}
		} else {
			if(input$paired_end_pipeline == "se") {
				seqfile1 <- paste0(my_path, 'reads/', input$se_1_pipeline)
			} else {
				seqfile1 <- paste0(my_path, 'reads/', input$pe_1_pipeline)
			}
		}
	} else if(activePage == MAPPING_PAGE) {
		if(input$file_location_mapping == "local") {
			if(input$paired_end_mapping == "se") {
				seqfile1 <- input$ls_1_mapping$datapath
			} else {
				seqfile1 <- input$lp_1_mapping$datapath
			}
		} else {
			if(input$paired_end_mapping == "se") {
				seqfile1 <- paste0(my_path, 'reads/', input$se_1_mapping)
			} else {
				seqfile1 <- paste0(my_path, 'reads/', input$pe_1_mapping)
			}
		}
	}

	print(seqfile1)

	if(is.null(seqfile1)) {
		showModal(modalDialog("No Sequence File Selected", title = "File Error"))
		stop()
	} else {
		return(seqfile1)
	}
}

#' Returns path of sequence file 2 file on active page
#'
#' @param input List of Shiny input objects
#' @return Path of sequence file 2 on active page
getSeqFile2 <- function(input) {
	activePage = input$nav
	seqfile2 = NULL

	if(activePage == PIPELINE_STRING) {
		if(input$file_location_pipeline == "local") {
			seqfile2 = input$lp_2_pipeline$datapath
		} else {
			seqfile2 = paste0(my_path, 'reads/', input$pe_2_pipeline)
		}
	} else if(activePage == MAPPING_PAGE) {
		if(input$file_location_mapping == "local") {
			seqfile2 = input$lp_2_mapping$datapath
		} else {
			seqfile2 = paste0(my_path, 'reads/', input$pe_2_mapping)
		}
	}

	if(is.null(seqfile2)) {
		showModal(modalDialog("No Sequence File Selected", title = "File Error"))
		stop()
	} else {
		if(input$paired_end_pipeline == "se") {
			return(NULL) 
		} else {
			return(seqfile2)
		}	
	}
}

#' Returns file name of sequence file 1 file on active page
#'
#' @param input List of Shiny input objects
#' @return Name of sequence file 1 on active page
getSeqName1 <- function(input) {
	activePage = input$nav
	seqfile1 = NULL

	if(activePage == PIPELINE_STRING) {
		if(input$file_location_pipeline == "local") {
			if(input$paired_end_pipeline == "se") {
				seqfile1 = strsplit(input$ls_1_pipeline$datapath, input$ls_1_pipeline$name)[[1]][[1]]
			} else {
				seqfile1 = strsplit(input$lp_1_pipeline$datapath, input$lp_1_pipeline$name)[[1]][[1]]
			}
		} else {
			if(input$paired_end_pipeline == "se") {
				seqfile1 = strsplit(input$se_1_pipeline, '[.]')[[1]][[1]]
			} else {
				seqfile1 = strsplit(input$pe_1_pipeline, '[.]')[[1]][[1]]
			}
		}
	} else if(activePage == MAPPING_PAGE) {
		if(input$file_location_mapping == "local") {
			if(input$paired_end_mapping == "se") {
				seqfile1 = strsplit(input$ls_1_mapping$datapath, input$ls_1_mapping$name)[[1]][[1]]
			} else {
				seqfile1 = strsplit(input$lp_1_mapping$datapath, input$lp_1_mapping$name)[[1]][[1]]
			}
		} else {
			if(input$paired_end_mapping == "se") {
				seqfile1 = strsplit(input$se_1_mapping, '[.]')[[1]][[1]]
			} else {
				seqfile1 = strsplit(input$pe_1_pipeline, '[.]')[[1]][[1]]
			}
		}
	}

	print(seqfile1)

	if(is.null(seqfile1)) {
		showModal(modalDialog("No Sequence File Selected", title = "File Error"))
		stop()
	} else {
		return(seqfile1)
	}
}

#' Returns file name of sequence file 2 file on active page
#'
#' @param input List of Shiny input objects
#' @return Name of sequence file 2 on active page
getSeqName2 <- function(input) {
	activePage = input$nav
	seqfile2 = NULL

	if(activePage == PIPELINE_STRING) {
		if(input$file_location_pipeline == "local") {
			seqfile2 = strsplit(input$lp_2_pipeline$datapath, input$lp_2_pipeline$name)[[1]][[1]]
		} else {
			seqfile2 = strsplit(input$pe_2_pipeline, '[.]')[[1]][[1]]
		}
	} else if(activePage == MAPPING_PAGE) {
		if(input$file_location_mapping == "local") {
			seqfile2 = strsplit(input$lp_2_mapping$datapath, input$lp_2_pipeline$name)[[1]][[1]]
		} else {
			seqfile2 = strsplit(input$pe_2_mapping, '[.]')[[1]][[1]]
		}
	}

	if(is.null(seqfile2)) {
		showModal(modalDialog("No Sequence File Selected", title = "File Error"))
		stop()
	} else {
		if(input$paired_end_pipeline == "se") {
			return(NULL) 
		} else {
			return(seqfile2)
		}	
	}
}

#' Creates progress bar
#'
#' @param session Session object
progress <- function(session) {
	progress <- Progress$new(session, min=1, max=15)
		on.exit(progress$close())

			progress$set(message = 'Calculation in progress',
	         detail = 'This may take a while...')

	    for (i in 1:15) {
	      progress$set(value = i)
	      Sys.sleep(0.1)
	    }
}