fscache_tag_filename <- "__FSCACHE__"
tag_name_pattern <- "[_0-9A-Z]+"
tag_file_pattern <- paste0("__", tag_name_pattern, "__")

#' A cache class for handling caching on the file system.
#'
#' The purpose of this class is to help managing a user cache folder for an
#' application.
#' Files can be copied or moved into the cache folder.
#' Character values can be saved into files and loaded from files.
#' Sub-folders can be defined.
#' Folders can be listed to see the existing files.
#' Files can be deleted individually or by batch,
#' Whole folders can be deleted, including the main cache folder.
#'
#' @examples
#' # Create a new cache instance inside a custom folder
#' cache <- Cache$new(tempdir())
#'
#' # Create some contents for the example
#' contents <- c("a", "b", "c")
#'
#' # Save contents
#' cache$saveContents(contents, c("a.txt", "b.txt", "c.txt"),
#'                    sub_folder = "sub1")
#'
#' # Get list of files inside folder
#' files <- cache$listFolder("sub1")
#'
#' # Delete files
#' cache$delPaths(c("a.txt", "c.txt"), sub_folder = "sub1")
#'
#' # Delete whole sub-folder
#' cache$delFolder("sub1")
#'
#' # Erase cache
#' cache$erase()
#'
#' @import R6
#' @import chk
#' @import lgr
#' @import lifecycle
#' @import R.utils
#' @import tools
#' @include fcts.R
#' @export
Cache <- R6::R6Class( # nolint: object_name_linter
  "Cache",

  public = list(

    #' @description
    #' New instance initializer.
    #'
    #' Initializes a \code{Cache} instance, using a specified folder.
    #' Path to the folder can be absolute or relative.
    #' When relative, the absolute root folder is either the standard user cache
    #' folder or the current working directory, depending on \code{user}
    #' parameter.
    #'
    #' @param folder The path to the wanted cache folder. Either an absolute
    #'               path, or a relative path that will be resolved immediately
    #'               into an absolute path.
    #' @param user If set to TRUE and the folder path is a relative path, then
    #'             the path is resolved relatively to the standard user cache
    #'             folder (i.e.: we call \code{tools::R_user_dir(folder,
    #'             which = 'cache')}). A good and standard practice is to set
    #'             the \code{folder} parameter to your package name,
    #'             using.
    #' @param force If the folder exists, is not empty and is not an fscache
    #'              folder, fails if force is \code{FALSE}, and use folder
    #'              anyway if force is \code{TRUE}.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance.
    #' # Note for the sake of the example we use a temporary directory specified
    #' # as an absolute path, however the usual way to use the cache system is
    #' # to provide a relative path, that will be placed inside the standard
    #' # user cache folder defined by the OS.
    #' cache <- Cache$new(tempdir())
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    initialize = function(folder, user = TRUE, force = FALSE) {

      chk::chk_string(folder)
      chk::chk_flag(user)
      chk::chk_flag(force)

      # Make folder path absolute
      if (! R.utils::isAbsolutePath(folder)) {

        # Use standard user cache folder
        if (user)
          folder <- tools::R_user_dir(folder, which = "cache")

        # Use current working directory
        else
          folder <- R.utils::getAbsolutePath(folder)
      }

      # Set tag file path
      tag_file <- file.path(folder, fscache_tag_filename)

      if (! dir.exists(folder)) {
        # Folder does not exist
        dir.create(folder, recursive = TRUE)
        file.create(tag_file)

      } else if (length(Sys.glob(file.path(folder, "*"))) == 0) {
        # Folder is empty
        file.create(tag_file)

      } else {
        # Folder is not empty
        if (force && ! file.exists(tag_file))
          file.create(tag_file)
        else
          chk::chk_file(tag_file)
      }

      # Make tag file path
      private$tag_file <- tag_file
      private$folder <- folder

      return(invisible(NULL))
    },

    #' @description
    #' Tests if the cache system is readable.
    #'
    #' Cache reading may be disabled and re-enabled with \code{setReadable()},
    #' Mainly used for debug purposes.
    #'
    #' @return \code{TRUE} if the cache system is readable, \code{FALSE}
    #' otherwise.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Tests if readable (TRUE by default)
    #' if (cache$isReadable()) {
    #'   print("Cache is readable")
    #' }
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    isReadable = function() {
      return(private$readable)
    },

    #' @description
    #' Tests if the cache system is writable.
    #'
    #' Cache reading may be disabled and re-enabled with \code{setWritable()}.
    #' Mainly used for debug purposes.
    #'
    #' @return \code{TRUE} if the cache system is writable, \code{FALSE}
    #' otherwise.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Tests if writeable (TRUE by default)
    #' if (cache$isWritable()) {
    #'   print("Cache is writeable")
    #' }
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    isWritable = function() {
      return(private$writable)
    },

    #' @description
    #' Disables or enable cache reading.
    #'
    #' Allows or disallows reading to the cache folder.
    #'
    #' @param readable Set to \code{FALSE} to disallow reading and to
    #' \code{TRUE} to allow it.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Disallow reading
    #' cache$setReadable(FALSE)
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    setReadable = function(readable) {
      chk::chk_flag(readable)
      private$readable <- readable
      return(invisible(NULL))
    },

    #' @description
    #' Disables or enable cache writing.
    #'
    #' Allows or disallows writing to the cache folder.
    #'
    #' @param writable Set to \code{FALSE} to disallow writing and to
    #'                 \code{TRUE} to allow it.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Disallow writing
    #' cache$setWritable(FALSE)
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    setWritable = function(writable) {
      chk::chk_flag(writable)
      private$writable <- writable
      return(invisible(NULL))
    },

    #' @description
    #' Gets the path to the main cache folder or a sub-folder.
    #'
    #' Returns the absolute path to the main cache folder or a cache sub-folder.
    #' By default, the folder is created if it does not exist.
    #'
    #' @param sub_folder A sub-folder.
    #' @param create If set to \code{TRUE} and the folder does not exist,
    #'               creates it.
    #' @param fail If set to \code{TRUE}, throws an error if the folder does not
    #'             exist.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return The path to the cache folder as a character value.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get the absolute path to the cache folder
    #' folder <- cache$getFolder()
    #'
    #' # Get the absolute path to a cache sub-folder
    #' sub_folder <- cache$getFolder('my_sub_folder')
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    getFolder = function(sub_folder = NULL, create = TRUE, fail = FALSE,
                         sub.folder = NULL) { # nolint: object_name_linter

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "getFolder(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)
      chk::chk_flag(create)
      chk::chk_flag(fail)

      # Erased?
      if (private$erased) {
        dir.create(private$folder, recursive = TRUE)
        file.create(private$tag_file)
        private$erased <- FALSE
      }

      if (is.null(sub_folder)) {
        # Get base folder
        path <- private$folder

      } else if (substr(sub_folder, 1, 1) == "/") {
        # sub-folder starts with a slash
        msg <- sprintf("Sub-folder cannot start with a slash character.")
        lgr::get_logger("fscache")$error(msg)
        stop(msg)

      } else {
        # Get relative folder
        path <- file.path(private$folder, sub_folder)
      }

      # Does folder exist?
      if (! dir.exists(path)) {

        # Create sub-folder
        if (create) {
          lgr::get_logger("fscache")$info(sprintf("Create cache folder \"%s\".",
                                                  path))
          dir.create(path, recursive = TRUE)

        } else if (fail) { # Fails
          msg <- sprintf("No cache folder \"%s\" exists.", path)
          lgr::get_logger("fscache")$error(msg)
          stop(msg)
        }
      }

      return(path)
    },

    #' @description
    #' Tests if the cache main folder or a cache sub-folder exists.
    #'
    #' @param sub_folder The sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return TRUE if the folder exists. FALSE otherwise.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Test if a sub-folder exists
    #' if (cache$hasFolder("my_sub_folder")) {
    #'   print("Sub-folder exists.")
    #' }
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    hasFolder = function(sub_folder = NULL,
                         sub.folder = NULL) { # nolint: object_name_linter

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "hasFolder(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      return(dir.exists(self$getFolder(sub_folder = sub_folder,
                                       create = FALSE)))
    },

    #' @description
    #' Computes paths in the cache folder or a cache sub-folder.
    #'
    #' Takes a list of relative paths and resolves them using the cache folder
    #' path to a list of absolute paths.
    #'
    #' @param paths A character vector containing paths.
    #' @param suffix A suffix to add to all paths.
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return A character vector, the same size as \code{paths},
    #' containing the absolute paths.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get the paths a list of filenames should have in the cache folder
    #' paths <- cache$getPaths(c("a.csv", "b.txt"))
    #'
    #' # Get paths using a common extension for filenames
    #' paths <- cache$getPaths(c("a", "b"), suffix = ".csv")
    #'
    #' # Get paths of files inside a sub-folder
    #' paths <- cache$getPaths(c("a.csv", "b.txt"), sub_folder = "foo")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    getPaths = function(paths, suffix = NULL, sub_folder = NULL,
                        sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_character(paths)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "getPaths(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      # Get list of valid paths
      is_valid <- (! is.na(paths)) & (paths != "")

      # Get folder
      folder <- self$getFolder(sub_folder = sub_folder)

      # Append suffix
      if (! is.null(suffix))
        paths[is_valid] <- paste0(paths[is_valid], suffix)

      # Build absolute paths
      paths[is_valid] <- file.path(folder, paths[is_valid])

      return(paths)
    },

    #' @description
    #'
    #' Search for files inside the cache folder or one of its subfolders.
    #'
    #' @param suffix The suffix files must have.
    #' @param sub_folder A sub-folder where to search.
    #' @param tag_files If set to \code{FALSE} (default), exclude the tag files.
    #' Otherwise include them in the output.
    #' @param folders If set to \code{FALSE} (default), exclude the folders.
    #' Otherwise include them in the output.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @param tag.files `r lifecycle::badge("deprecated")` Use tag_files
    #'                  instead.
    #' @return A character vector containing paths to existing file matching the
    #' criteria.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get all existing files inside sub-folder foo
    #' paths <- cache$globPaths(sub_folder = "foo")
    #'
    #' # Get all existing files with extension ".txt" inside main folder
    #' paths <- cache$globPaths(suffix = ".txt")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    globPaths = function(suffix = NULL, sub_folder = NULL, tag_files = FALSE,
                         folders = FALSE,
                         tag.files = NULL, sub.folder = NULL) { # nolint

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "globPaths(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(tag.files)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "globPaths(tag.files)",
          details = "Parameter tag.files is replaced by tag_files."
        )
        chk::chk_flag(tag.files)
        tag_files <- tag.files
      }
      chk::chk_flag(tag_files)
      chk::chk_flag(folders)

      # Get folder
      folder <- self$getFolder(sub_folder = sub_folder)

      # Set pattern
      pattern <- if (is.null(suffix)) "*" else paste0("*", suffix)

      # Look for files
      paths <- Sys.glob(file.path(folder, pattern))

      # Filter out tag files
      if (! tag_files) {
        tag_indices <- grep(paste0("^(.+/|)", tag_file_pattern, "$"), paths)
        if (length(tag_indices) > 0)
          paths <- paths[-tag_indices]
      }

      # Filter out folders
      if (! folders)
        paths <- paths[! dir.exists(paths)]

      return(paths)
    },

    #' @description
    #' Gets the number of items contained inside a cache folder.
    #'
    #' Counts the number of items (files or folders) contained inside a cache
    #' folder.
    #' This method does not explore the file system recursively, but only look
    #' at the files inside the folder.
    #'
    #' @param sub_folder A sub-folder.
    #' @param tag_files If set to \code{FALSE} (default), do not count the tag
    #'                  files. Otherwise count them.
    #' @param folders If set to \code{FALSE} (default), do not count the
    #'                folders. Otherwise count them.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @param tag.files `r lifecycle::badge("deprecated")` Use tag_files
    #'                  instead.
    #' @return The number of items.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get the number of files inside sub-folder "foo"
    #' n <- cache$getNbItems("foo")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    getNbItems = function(sub_folder = NULL, tag_files = FALSE, folders = FALSE,
                          tag.files = NULL, sub.folder = NULL) { # nolint

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "getNbItems(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)
      if (! is.null(tag.files)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "getNbItems(tag.files)",
          details = "Parameter tag.files is replaced by tag_files."
        )
        chk::chk_flag(tag.files)
        tag_files <- tag.files
      }
      chk::chk_flag(tag_files)
      chk::chk_flag(folders)

      # Get folder
      folder <- self$getFolder(sub_folder)

      # Get all items
      items <- Sys.glob(file.path(folder, "*"))

      # Filter out tag files
      if (! tag_files) {
        is_tag <- grepl(paste0("/", tag_file_pattern, "$"), items, perl = TRUE)
        items <- items[! is_tag]
      }

      # Filter out folders
      if (! folders)
        items <- items[! dir.exists(items)]

      # Return number of items
      return(length(items))
    },

    #' @description
    #' Tests if paths exist inside a cache folder.
    #'
    #' Takes a list of relative paths and resolves them using the cache folder
    #' path to a list of absolute paths. Tests then if each path points to real
    #' object on the file system.
    #'
    #' @param paths A character vector containing paths.
    #' @param suffix A suffix to add to all paths.
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return A logical vector, the same size as \code{paths}, with \code{TRUE}
    #' value if the file exists in the cache, or \code{FALSE} otherwise.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Test if some files exist in the cache
    #' exits <- cache$pathsExist(c("a", "b"), suffix = ".txt")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    pathsExist = function(paths, suffix = NULL, sub_folder = NULL,
                          sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_character(paths)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "pathsExist(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      paths <- self$getPaths(paths = paths, suffix = suffix,
                             sub_folder = sub_folder)

      return(file.exists(paths))
    },

    #' @description
    #' Tests if a tag exists in the cache.
    #'
    #' Tags are empty files, without extension, whose name starts and ends with
    #' \code{"__"}.
    #' This method tests if some tag file already exist in a cache folder.
    #'
    #' @param name The name of the tag, without the prefix \code{"__"} and the
    #' suffix \code{"__"}. It will be automatically converted in uppercase. It
    #' can only contains digits, letters and underscore characters.
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return \code{TRUE} if the tag exists in the cache. \code{FALSE}
    #' otherwise.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Test if tag file "downloaded" exists in sub-folder "hmdb"
    #' if (cache$tagExists("downloaded", sub_folder = "hmdb")) {
    #'   print("Tag exists")
    #' }
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    tagExists = function(name, sub_folder = NULL,
                         sub.folder = NULL) { # nolint: object_name_linter

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "tagExists(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      path <- private$getTagPath(name, sub_folder = sub_folder)

      return(file.exists(path))
    },

    #' @description
    #' Sets a tag into the cache.
    #'
    #' @param name The name of the tag, without the prefix \code{"__"} and the
    #' suffix \code{"__"}. It will be automatically converted in uppercase. It
    #' can only contains digits, letters and underscore characters.
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Create tag file "downloaded" in sub-folder "hmdb"
    #' cache$writeTag("downloaded", sub_folder = "hmdb")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    writeTag = function(name, sub_folder = NULL,
                        sub.folder = NULL) { # nolint: object_name_linter

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "writeTag(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      # Check if cache is writable
      private$checkWritable()

      # Get tag path
      path <- private$getTagPath(name, sub_folder = sub_folder)

      # Create tag file
      file.create(path)

      return(invisible(NULL))
    },

    #' @description
    #' Gets path to the cache system temporary folder.
    #'
    #' This temporary folder located inside the cache folder is needed in order
    #' to be able to move/rename files into the right cache location.
    #' When creating files in the system temporary folder, which may reside on a
    #' different partition, moving a file could fail as in the following error:
    #'     cannot rename file "/tmp/Rtmpld18y7/10182e3a086e7b8a7.tsv" to
    #'     "/home/pr228844/dev/biodb/cache/comp.csv.file-58e...c4/2e3...a7.tsv",
    #'     reason "Invalid cross-device link".
    #'
    #' When you download a file directly to the disk using for instance
    #' \code{download.file()}, write the destination into this destination
    #' folder.  When downloaded is complete, move the file using the method
    #' \code{importFiles()}.
    #'
    #' @return A string containing the path to the folder.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get the cache temporary folder
    #' tmp <- cache$getTmp()
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    getTmp = function() {

      tmp_dir <- file.path(self$getFolder(), "tmp")
      if (! dir.exists(tmp_dir))
        dir.create(tmp_dir, recursive = TRUE)

      return(tmp_dir)
    },

    #' @description
    #' Returns all existing sub-folders.
    #'
    #' @return A character vector containing all the sub-folders.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Get the list of sub-folders
    #' sub.folders <- cache$getSubFolders()
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    getSubFolders = function() {

      # Get all items
      items <- Sys.glob(file.path(self$getFolder(), "*"))

      # Remove files
      folders <- items[dir.exists(items)]

      # Get basename
      folders <- basename(folders)

      # Remove tmp folder
      folders <- folders[folders != "tmp"]

      return(folders)
    },

    #' @description
    #' Imports existing files into the cache.
    #'
    #' @param src A character vector containing paths to files to import.
    #' @param dst A character vector containing destination filenames. The
    #'            vector must have the length as the \code{src} vector. If
    #'            \code{NULL}, the filenames in \code{src} will be used.
    #' @param suffix A suffix to add to all destination paths.
    #' @param sub_folder A sub-folder. All files will copied or moved to this
    #'                   sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @param action Specifies if files have to be moved or copied into the
    #'               cache.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Create some files for the example
    #' files <- c("k.txt", "u.csv")
    #' file.create(files)
    #'
    #' # Move those files into the cache
    #' cache$importFiles(files, sub_folder = "foo", action = "copy")
    #'
    #' # Remove original files
    #' unlink(files)
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    importFiles = function(src, dst = NULL, suffix = NULL, sub_folder = NULL,
                           action = c("copy", "move"),
                           sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_character(src)
      chk::chk_null_or(dst, vld = chk::vld_character)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "importFiles(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)
      if (! is.null(dst))
        chk::chk_identical(length(src), length(dst))
      chk::chk_all(src, chk_file) # Check all src files exist
      action <- match.arg(action)

      # Check if cache is writable
      private$checkWritable()

      # Get destination folder
      folder <- self$getFolder(sub_folder = sub_folder)

      lgr::get_logger("fscache")$info(sprintf("%s %d files to %s.", action,
                                              length(src), folder))

      # Get destination file names
      if (is.null(dst))
        dst <- basename(src)
      if (! is.null(suffix))
        dst <- paste0(dst, suffix)
      dst <- file.path(folder, dst)

      # Move files
      if (action == "move")
        file.rename(src, dst)

      # Copy files
      else
        file.copy(src, dst)

      lgr::get_logger("fscache")$info("Done importing files.")

      return(invisible(NULL))
    },

    #' @description
    #' Saves contents to files into the cache.
    #'
    #' Saves character values into files inside a cache folder.
    #'
    #' @param contents A character vector containing the contents to write.
    #' @param dst A character vector containing destination filenames. The
    #'            vector must have the length as the \code{contents} vector.
    #' @param suffix A suffix to add to all destination paths.
    #' @param sub_folder A sub-folder. All files will copied or moved to this
    #'                   sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Create some contents for the example
    #' contents <- c("a", "b", "c")
    #'
    #' # Save contents
    #' cache$saveContents(contents, c("a.txt", "b.txt", "c.txt"))
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    saveContents = function(contents, dst, suffix = NULL, sub_folder = NULL,
                            sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_character(contents)
      chk::chk_character(dst)
      chk::chk_identical(length(contents), length(dst))
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "saveContents(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      # Check if cache is writable
      private$checkWritable()

      # Get destination folder
      folder <- self$getFolder(sub_folder = sub_folder)

      lgr::get_logger("fscache")$info(sprintf("Save %d contents to %s.",
                                              length(contents), folder))

      # Get destination file names
      if (! is.null(suffix))
        dst <- paste0(dst, suffix)
      dst <- file.path(folder, dst)

      # Remove NA elements. They are not saved.
      nas <- vapply(contents, is.na, FUN.VALUE = TRUE)
      contents <- contents[! nas]
      dst <- dst[! nas]

      # Save contents
      mapply(function(cnt, f) cat(cnt, file = f), contents, dst)

      return(invisible(NULL))
    },

    #' @description
    #' Loads contents from files stored into the cache.
    #'
    #' Loads character values from cache files.
    #'
    #' @param paths A character vector containing destination filenames. The
    #'              vector must have the length as the \code{contents} vector.
    #' @param suffix A suffix to add to all destination paths.
    #' @param sub_folder A sub-folder. All files will copied or moved to this
    #'                   sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return A character vector , the same size as \code{paths}, containing
    #' the contents of the files. If some file does not exist, a \code{NA} value
    #' is returned.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Create some contents for the example
    #' contents <- c("1", "2", "3")
    #'
    #' # Save contents
    #' cache$saveContents(contents, c("a", "b", "c"), suffix = ".txt",
    #'                    sub_folder = "ex2")
    #'
    #' # Load contents
    #' contents <- cache$loadContents(c("a", "b", "c"), suffix = ".txt",
    #'                                sub_folder = "ex2")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    loadContents = function(paths, suffix = NULL, sub_folder = NULL,
                            sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_character(paths)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "loadContents(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      # Check if cache is readable
      private$checkReadable()

      # Initialize contents vector
      contents <- rep(NA_character_, length(paths))

      # Get folder
      folder <- self$getFolder(sub_folder = sub_folder)

      # Get full file paths
      if (! is.null(suffix))
        paths <- paste0(paths, suffix)
      paths <- file.path(folder, paths)

      # Load contents
      lgr::get_logger("fscache")$info(sprintf("Load %d contents, from %s.",
                                              length(paths), folder))
      contents <- vapply(paths, load_text_content, FUN.VALUE = "")

      return(contents)
    },

    #' @description
    #' Deletes a list of paths inside the cache system.
    #'
    #' Takes a list of relative paths, resolves them using the cache folder path
    #' to a list of absolute paths, and deletes the corresponding files.
    #'
    #' @param paths A character vector containing paths.
    #' @param suffix A suffix to add to all paths.
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Delete some cache files
    #' cache$delPaths(c("a.txt", "b.txt"))
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    delPaths = function(paths = NULL, suffix = NULL, sub_folder = NULL,
                        sub.folder = NULL) { # nolint: object_name_linter

      chk::chk_null_or(paths, vld = chk::vld_character)
      chk::chk_null_or(suffix, vld = chk::vld_string)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "delPaths(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      # Check if cache is writable
      private$checkWritable()

      # Search for existing files
      if (is.null(paths)) {
        paths <- self$globPaths(suffix = suffix, sub_folder = sub_folder)

      } else { # Get full paths
        paths <- self$getPaths(paths, suffix = suffix, sub_folder = sub_folder)

        # Filter out tag files
        tag_index <- grep(paste0("^(.+/|)", tag_file_pattern, "$"), paths)
        if (length(tag_index) > 0)
          paths <- paths[-tag_index]

        # Filter out folders
        paths <- paths[! dir.exists(paths)]
      }

      # Remove files
      lapply(paths, unlink)

      return(invisible(NULL))
    },

    #' @description
    #' Deletes all files in a sub-folder.
    #'
    #' Deletes a sub-folder and all its content.
    #'
    #' @param sub_folder A sub-folder.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Delete sub-folder
    #' cache$delFolder("my_sub_folder")
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    delFolder = function(sub_folder,
                         sub.folder = NULL) { # nolint: object_name_linter

      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "getNbItems(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_string(sub_folder)

      # Check if cache is writable
      private$checkWritable()

      # Get folder path
      folder <- self$getFolder(sub_folder = sub_folder)

      # Delete folder
      if (dir.exists(folder)) {
        lgr::get_logger("fscache")$info(sprintf("Erasing sub-folder \"%s\".",
                                                sub_folder))
        unlink(folder, recursive = TRUE)
      }

      return(invisible(NULL))
    },

    #' @description
    #' Lists files present inside a cache folder.
    #'
    #' Lists files that exist inside a cache folder.
    #' Returns by default the full paths of the found files.
    #' It is possible to filter on files suffix, and to extract the basename.
    #'
    #' @param sub_folder A sub-folder, or \code{NULL} for the main folder.
    #' @param suffix A file suffix on which to filter.
    #' @param extract_name If set to \code{TRUE}, instead of returning the full
    #'                     paths of the files, returns their basenames.
    #' @param remove_suffix When set to \code{TRUE} and \code{extract.name} is
    #'                      \code{TRUE} and \code{suffix} is not \code{NULL},
    #'                      remove the suffix from the returned basenames.
    #' @param tag_files If set to \code{FALSE} (default), exclude the tag files.
    #'                  Otherwise include them in the output.
    #' @param folders If set to \code{FALSE} (default), exclude the folders.
    #'                Otherwise include them in the output.
    #' @param sub.folder `r lifecycle::badge("deprecated")` Use sub_folder
    #'                   instead.
    #' @param tag.files `r lifecycle::badge("deprecated")` Use tag_files
    #'                  instead.
    #' @param extract.name `r lifecycle::badge("deprecated")` Use extract_name
    #'                     instead.
    #' @param remove.suffix `r lifecycle::badge("deprecated")` Use remove_suffix
    #'                      instead.
    #' @return The paths to the found files, or the names of the files if
    #' \code{extract.name} is set to \code{TRUE}.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new("my_cache_folder")
    #'
    #' # List files in sub-folder
    #' files <- cache$listFolder("my_sub_folder")
    #'
    listFolder = function(sub_folder = NULL, suffix = NULL,
                          extract_name = FALSE, remove_suffix = FALSE,
                          tag_files = FALSE, folders = FALSE,
                          extract.name = NULL, remove.suffix = NULL, # nolint
                          tag.files = NULL, sub.folder = NULL) { # nolint

      chk::chk_null_or(suffix, vld = chk::vld_string)
      chk::chk_flag(folders)
      if (! is.null(sub.folder)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "listFolder(sub.folder)",
          details = "Parameter sub.folder is replaced by sub_folder."
        )
        sub_folder <- sub.folder
      }
      chk::chk_null_or(sub_folder, vld = chk::vld_string)
      if (! is.null(tag.files)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "listFolder(tag.files)",
          details = "Parameter tag.files is replaced by tag_files."
        )
        chk::chk_flag(tag.files)
        tag_files <- tag.files
      }
      chk::chk_flag(tag_files)
      if (! is.null(extract.name)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "listFolder(extract.name)",
          details = "Parameter extract.name is replaced by extract_name."
        )
        chk::chk_flag(extract.name)
        extract_name <- extract.name
      }
      chk::chk_flag(extract_name)
      if (! is.null(remove.suffix)) {
        lifecycle::deprecate_warn(
          when = "1.0.3",
          what = "listFolder(remove.suffix)",
          details = "Parameter remove.suffix is replaced by remove_suffix."
        )
        chk::chk_flag(remove.suffix)
        remove_suffix <- remove.suffix
      }
      chk::chk_flag(remove_suffix)

      # Get destination folder
      folder <- self$getFolder(sub_folder = sub_folder)

      # List existing paths
      pattern <- file.path(folder, "*")
      if (! is.null(suffix))
        pattern <- paste0(pattern, suffix)
      paths <- Sys.glob(pattern)

      # Extract name
      if (extract_name) {

        # Get basename
        x <- basename(paths)

        # Remove suffix
        if (! is.null(suffix) && remove_suffix)
          x <- substr(x, 1, nchar(x) - nchar(suffix))

        # Replace paths with names
        paths <- x
      }

      # Remove tag file
      if (! tag_files) {
        tag_indices <- grep(paste0("^(.+/|)", tag_file_pattern, "$"), paths)
        if (length(tag_indices) > 0)
          paths <- paths[-tag_indices]
      }

      # Filter out folders
      if (! folders)
        paths <- paths[! dir.exists(paths)]

      return(paths)
    },

    #' @description
    #' Displays information about this object.
    #'
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Print information
    #' print(cache)
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    print = function() {
      cat("Cache class\n")
      cat("  The main cache folder is at ", self$getFolder(), ".\n", sep = "")
      cat("  The cache is ", (if (self$isReadable()) "" else "not "),
          "readable.\n", sep = "")
      cat("  The cache is ", (if (self$isWritable()) "" else "not "),
          "writable.\n", sep = "")
    },

    #' @description
    #' Erases the whole cache folder.
    #'
    #' Deletes the main cache folder and all its files and sub-folders.
    #'
    #' @return Nothing.
    #'
    #' @examples
    #' # Create a new cache instance
    #' cache <- Cache$new(tempdir())
    #'
    #' # Deletes the whole cache content
    #' cache$erase()
    #'
    #' # Erase cache
    #' cache$erase()
    #'
    erase = function() {

      # Erase whole cache
      lgr::get_logger("fscache")$info(sprintf("Erasing cache \"%s\".",
                                              self$getFolder()))
      unlink(self$getFolder(), recursive = TRUE)

      private$erased <- TRUE

      return(invisible(NULL))
    }

  ),

  private = list(
    folder = NULL,
    tag_file = NULL,
    readable = TRUE,
    writable = TRUE,
    erased = FALSE,

    getTagPath = function(name, sub_folder = NULL) {

      chk::chk_string(name)
      name <- toupper(name)
      chk::chk_match(name, paste0("^", tag_name_pattern, "$"))
      chk::chk_null_or(sub_folder, vld = chk::vld_string)

      filename <- paste0("__", name, "__")
      path <- self$getPaths(filename, sub_folder = sub_folder)

      return(path)
    },

    checkReadable = function() {

      if (! self$isReadable()) {
        msg <- paste0("Attempt to read into non-readable cache at \"",
                      self$folder, "\".")
        lgr::get_logger("fscache")$error(msg)
        stop(msg)
      }

      return(invisible(NULL))
    },

    checkWritable = function() {

      if (! self$isWritable()) {
        msg <- paste0("Attempt to write into non-writable cache at \"",
                      self$folder, "\".")
        lgr::get_logger("fscache")$error(msg)
        stop(msg)
      }

      return(invisible(NULL))
    }
  )
)
