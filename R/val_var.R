val_var <- function(test_var, type, force = 2, size = 0, def = FALSE) {

  # Get variable info
  parentf <- deparse(sys.call(-1))
  vname <- as.character(sys.call()[2])
  v_info <- paste0("Variable ", vname," in ", parentf)

  if ( (is.character(test_var)) && (test_var == "") && (def) ) {
    return("")
  }

  # Check for existence of variable
  if (!exists("test_var")) {

    message <- paste0(v_info, " does not exist\n")
    return(val_return_private(message, force))

  }

  # Check if variable is empty
  if (is.null("test_var")) {

    message <- paste0(v_info, " is NULL (empty)\n")
    return(val_return_private(message, force))

  }

  # Check for existence of variable
  if (size != 0) {
    if (length(test_var) != size) {

      message <- paste0(v_info, " must have the size ", size,"\n")
      return(val_return_private(message, force))

    }
  }

  types <- c("numeric", "character", "logical", "list", "POSIXct", "ggplot")

  # Check if type is listed
  if (!type %in% types) {

    message <- paste0(type, " is not a type of val_val()\n")
    return(val_return_private(message, force))

  }

  # Check variable type

  if (type == "numeric") {
    if (mode(test_var) != "numeric") {

      message <- paste0(v_info, " to be 'numeric'\n")
      return(val_return_private(message, force))

    }
  }

  if (type == "character") {
    if (mode(test_var) != "character") {

      message <- paste0(v_info, " has to be 'character'\n")
      return(val_return_private(message, force))

    }
  }

  if (type == "logical") {
    if (mode(test_var) != "logical") {

      message <- paste0(v_info, " has to be 'logical'\n")
      return(val_return_private(message, force))

    }
  }

  if (type == "list") {
    if (mode(test_var) != "list") {

      message <- paste0(v_info, " has to be 'list'\n")
      return(val_return_private(message, force))

    }
  }

  if (type == "POSIXct") {
    if (!lubridate::is.POSIXct(test_var)) {

      message <- paste0(v_info, " has to be 'POSIXct'\n")
      return(val_return_private(message, force))

    }
  }

  if (type == "ggplot") {
    if (!ggplot2::is.ggplot(test_var)) {

      message <- paste0(v_info, " has to be 'ggplot'\n")
      return(val_return_private(message, force))

    }
  }

  return("")

}

val_return_private <- function(message, force) {

  if (force == 2) {
    return(message)
  }

  if (force == 1) {
    warning(message)
  }

  return("")

}

