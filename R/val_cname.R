
val_cname <- function(tracks, cname, type = "", size = 0, force = 2, def = TRUE) {

  # Get call info
  fname <- deparse(sys.call(-1))
  tname <- as.character(sys.call()[2])
  tcall_info <- paste0("\"", tname,"\" in \"", fname,"\"")
  ccall_info <- paste0("Column \"",cname,"\" of \"",tname,"\" in \"",fname,"\"")

  # Stop if no cname and no cname is allowed
  if (cname == "" && def) {
    return("")
  }

  # Check each cname
  for (i in 1:length(cname)) {

    result <- check_column_name_private(
      tracks, cname[i], type, size, force, tcall_info, ccall_info
    )

    if (result != "") {return(result)}
  }

  # Return that all is fine
  return("")

}

check_column_name_private <- function(
  tracks, cname, type, size, force, tcall_info, ccall_info
) {

  if (mode(force) != "numeric") {
    message <- paste0("\"force\" in val_var has to be\"numeric\".\n")
    return(val_return_private(message, force))
  }

  # Check for existence of tracks
  if (!exists("tracks")) {
    message <- paste0(tcall_info, " does not exist\n")
    return(val_return_private(message, force))
  }

  # Check if tracks is empty
  if (is.null(tracks)) {
    message <- paste0(tcall_info, " is NULL (empty)\n")
    return(val_return_private(message, force))
  }

  # check data frame
  if (!is.data.frame(tracks)) {
    message <- paste0(tcall_info, " is not a data frame\n")
    return(val_return_private(message, force))
  }

  # Check for existence of test_var
  if (!exists("cname")) {
    message <- paste0(ccall_info, " does not exist\n")
    return(val_return_private(message, force))
  }

  # Check if test_var is empty
  if (is.null(cname)) {
    message <- paste0(ccall_info, " is NULL (empty)\n")
    return(val_return_private(message, force))
  }

  # check if cname exists in tracks
  if (length(setdiff(cname, colnames(tracks))) != 0) {
    message <- paste0(ccall_info, " does not exist\n")
    return(val_return_private(message, force))
  }

  if (mode(type) != "character") {
    message <- paste0("\"type\" in val_var has to be\"character\".\n")
    return(val_return_private(message, force))
  }

  if (type != "") {

    types <- c("numeric", "character", "logical", "list")

    # Check if type is listed
    if (!type %in% types) {

      message <- paste0(
        "'",type,"'", " is not a type of mode() please check ",
        "your val_cname() 'type' variable\n"
      )
      return(val_return_private(message, force))

    }

    if (mode(tracks[,cname]) != type) {
      message <- paste0(ccall_info, " has to be \"", type,"\"\n")
      return(val_return_private(message, force))
    }
  }

  if (mode(size) != "numeric") {
    message <- paste0("\"type\" in val_var has to be\"numeric\".\n")
    return(val_return_private(message, force))
  }

  if (size != 0) {
    if (length(tracks[,cname]) != size) {
      message <- paste0(ccall_info, " has to have \"", size,"\" observations\n")
      return(val_return_private(message, force))
    }
  }

  return("")

}
