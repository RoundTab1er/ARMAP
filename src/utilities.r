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