#' Load the content of a text file.
#'
#' Load the content of a text file, trying to guess file encoding.
#' The content is returned as a single character value.
#'
#' @param path Path to the file from which to load content.
#' @param min.confidence The minimum confidence value (between 0 and 1)
#' @return The content of the file as a single character value,
#' \code{NA_character_} if encoding was not recognized.
#'
#' @examples
#' # Create a file in Latin-1 encoding
#' text_file <- tempfile('myfile', fileext = '.txt')
#' dir.create(dirname(text_file), recursive = TRUE)
#' x <- iconv("Qui sème le vent récolte la tempête.",
#'            from = "utf8", to = "latin1")
#' stringi::stri_write_lines(x, text_file, encoding = "latin1")
#'
#' # Load its content
#' content <- fscache::load_text_content(text_file)
#' content
#'
#' # Remove file
#' unlink(text_file)
#'
#' @import stringi
#' @export
load_text_content <- function(path, min.confidence = 0.0) {

  chk::chk_string(path)
  chk::chk_number(min.confidence)
  chk::chk_gte(min.confidence, 0.0)
  chk::chk_lte(min.confidence, 1.0)
  content <- NA_character_

  # File exists
  if (file.exists(path)) {

    # Load content
    content <- stringi::stri_read_raw(path)

    # Detect encoding
    encs <- stringi::stri_enc_detect(content)[[1]]
    if (! is.null(min.confidence))
      encs <- encs[encs$Confidence >= min.confidence, ]
    if (nrow(encs) > 0) {
      enc <- encs[1, "Encoding"]
      lgr::get_logger("fscache")$debug(
        sprintf("Found encoding %s for file %s.", enc, path)) # nolint
      content <- stringi::stri_encode(content, from = enc, to = "UTF-8")
    } else {
      lgr::get_logger("fscache")$warn(
        sprintf("Failed to find suitable encoding for file %s.", path)) # nolint
      content <- NA_character_
    }
  }

  return(content)
}
