testthat::context("Cache class")

# Remove default console appender
lgr::lgr$remove_appender(1)

# Temp folder, to work in
temp_dir <- tempdir()

# Define working folder
wrk_dir <- file.path(temp_dir, "wrk")

# Convert from Windows style to UNIX style, in order to avoid failure of
# comparison tests on Windows platforms.
wrk_dir <- gsub("\\\\", "/", wrk_dir)

# Create working folder
dir.create(wrk_dir, recursive = TRUE)

folder_name <- "test-fscache"
folder_path <- file.path(temp_dir, folder_name)

testthat::test_that("Class initialization works fine", {

  # No folder
  testthat::expect_error(fscache::Cache$new())

  # With absolute folder_path
  cache <- fscache::Cache$new(folder_path)
  testthat::expect_is(cache, "Cache")
  testthat::expect_true(dir.exists(folder_path))

  # Works fine with empty folder
  cache <- fscache::Cache$new(folder_path)
  cache$erase()
  dir.create(folder_path)
  cache <- fscache::Cache$new(folder_path)

  # Force with existing non-empty folder
  cache <- fscache::Cache$new(folder_path)
  cache$erase()
  dir.create(folder_path)
  file.create(file.path(folder_path, "some_file.txt"))
  testthat::expect_error(fscache::Cache$new(folder_path))
  cache <- fscache::Cache$new(folder_path, force = TRUE)
})

testthat::test_that("We can use a relative folder", {

  my_folder <- "relative-folder"

  # User cache folder
  cache <- fscache::Cache$new(my_folder, create = FALSE)
  testthat::expect_false(dir.exists(tools::R_user_dir(my_folder,
                                                      which = "cache")))
  testthat::expect_false(dir.exists(tools::R_user_dir(my_folder,
                                                      which = "cache")))
  testthat::expect_false(dir.exists(tools::R_user_dir(my_folder,
                                                      which = "cache")))
  testthat::expect_equal(cache$getFolder(create = FALSE),
                         tools::R_user_dir(my_folder, which = "cache"))

  # Relative to current working dir
  setwd(wrk_dir)
  cache <- fscache::Cache$new(my_folder, user = FALSE)
  testthat::expect_equal(cache$getFolder(), file.path(wrk_dir, my_folder))
})

testthat::test_that("We can save and load contents into files", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)

  # Loop on number of contents
  for (n in 1:3) {

    # Define contents and files
    contents <- as.character(seq(n))
    files <- paste0("data_", contents, ".txt")

    # Save contents
    cache$saveContents(contents, files)
    full_paths <- cache$getPaths(files)
    testthat::expect_true(all(file.exists(full_paths)))

    # Load contents
    contents_bis <- cache$loadContents(files)
    testthat::expect_identical(!! contents, !! unname(contents_bis))
    testthat::expect_identical(!! full_paths, !! names(contents_bis))

    # Load contents using suffix
    contents_bis <- cache$loadContents(paste0("data_", contents),
                                       suffix = ".txt")
    testthat::expect_identical(!! contents, !! unname(contents_bis))
    testthat::expect_identical(!! full_paths, !! names(contents_bis))
  }
})

testthat::test_that("getFolder() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)

  # Main folder
  testthat::expect_identical(!! folder_path, !! cache$getFolder())

  # Refute sub-folder starting with a slash
  testthat::expect_error(cache$getFolder("/"))

  # Sub-folder
  x <- "my_sub_folder"
  testthat::expect_identical(!! file.path(folder_path, x),
                             !! cache$getFolder(x))
  testthat::expect_true(dir.exists(cache$getFolder(x)))

  # Test create = FALSE
  x <- "my_sub_folder_2"
  testthat::expect_identical(!! file.path(folder_path, x),
                             !! cache$getFolder(x, create = FALSE))
  testthat::expect_false(dir.exists(cache$getFolder(x, create = FALSE)))

  # Test fail = TRUE and create = FALSE
  testthat::expect_error(cache$getFolder("my_sub_folder_3", create = FALSE,
                                         fail = TRUE))

  # Test fail = TRUE and create = TRUE
  x <- "my_sub_folder_4"
  testthat::expect_identical(!! file.path(folder_path, x),
                             !! cache$getFolder(x, fail = TRUE))
  testthat::expect_true(dir.exists(cache$getFolder(x, fail = TRUE)))
})

testthat::test_that("hasFolder() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)

  # Test main folder
  testthat::expect_true(cache$hasFolder())

  # Test existing sub-folder
  x <- "my_sub_folder_10"
  cache$getFolder(x)
  testthat::expect_true(cache$hasFolder(x))

  # Test existing sub-folder
  x <- "my_sub_folder_11"
  testthat::expect_false(cache$hasFolder(x))
})

testthat::test_that("getPaths() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)

  # Get one path
  x <- "my_file.txt"
  testthat::expect_identical(!! file.path(folder_path, x),
                             !! cache$getPaths(x))

  # With suffix
  x <- "my_file_2"
  suffix <- ".txt"
  testthat::expect_identical(!! file.path(folder_path, paste0(x, suffix)),
                             !! cache$getPaths(x, suffix = suffix))
})

testthat::test_that("globPaths() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)
  cache$erase()

  # Empty
  paths <- cache$globPaths()
  testthat::expect_is(paths, "character")
  testthat::expect_length(paths, 0)
  paths <- cache$globPaths(tag_files = TRUE)
  testthat::expect_length(paths, 1)

  # Create one file
  f <- "a_file.txt"
  cache$saveContents("abcd", f)
  paths <- cache$globPaths()
  testthat::expect_is(paths, "character")
  testthat::expect_equal(paths, file.path(folder_path, f))

  # No file *.zip
  testthat::expect_length(cache$globPaths(suffix = ".zip"), 0)

  # Tags are not listed
  cache$writeTag("A")
  paths <- cache$globPaths()
  testthat::expect_is(paths, "character")
  testthat::expect_equal(paths, file.path(folder_path, f))

  # Folders are not listed
  cache$getFolder("mysubfolder")
  paths <- cache$globPaths()
  testthat::expect_length(paths, 1)
  paths <- cache$globPaths(folders = TRUE)
  testthat::expect_length(paths, 2)
})

testthat::test_that("Writable flag works", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)
  testthat::expect_true(cache$isWritable())

  # Set unwritable
  cache$setWritable(FALSE)
  testthat::expect_false(cache$isWritable())

  # Test that it is unwritable
  my_file <- "test_writability.txt"
  testthat::expect_error(cache$saveContents("some content", my_file))

  # Set writable
  cache$setWritable(TRUE)
  testthat::expect_true(cache$isWritable())
})

testthat::test_that("Readable flag works", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)
  testthat::expect_true(cache$isReadable())

  # Set unreadable
  cache$setReadable(FALSE)
  testthat::expect_false(cache$isReadable())

  # Test that it is unreadable
  my_file <- "test_readability.txt"
  cache$saveContents("some content", my_file)
  testthat::expect_error(cache$loadContents(my_file))

  # Set readable
  cache$setReadable(TRUE)
  testthat::expect_true(cache$isReadable())
})

testthat::test_that("We can erase the main cache folder", {

  # Folder name with user cache folder
  cache <- fscache::Cache$new(folder_path)
  cache$erase()
  testthat::expect_false(dir.exists(folder_path))

  # With current folder
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()
  testthat::expect_false(dir.exists(folder_path))
})

testthat::test_that("getNbItems() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)
  cache$erase()
  testthat::expect_equal(0, !! cache$getNbItems())

  # Create some items and test count
  n <- 3
  for (i in seq(n)) {
    cache$saveContents("Some content", paste0("file_", i), suffix = ".txt")
    testthat::expect_equal(!!i, !!cache$getNbItems())
  }

  # Create a tag and check count
  cache$writeTag("foo")
  testthat::expect_equal(!!n, !!cache$getNbItems())
  testthat::expect_equal(!!n + 2, !!cache$getNbItems(tag_files = TRUE))

  # Create a folder and check count
  cache$getFolder("mysubfolder")
  testthat::expect_equal(!!n, !!cache$getNbItems())
  testthat::expect_equal(!!n + 1, !!cache$getNbItems(folders = TRUE))
})

testthat::test_that("print() works correctly", {

  # Create cache
  cache <- fscache::Cache$new(folder_path)

  # Check print
  testthat::expect_output(print(cache), "^Cache class.*$")
})

testthat::test_that("listFolder() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()

  # Create files
  files <- c("a.txt", "b.txt", "c.md")
  cache$saveContents(c("", "", ""), dst = files)

  # Get files
  testthat::expect_equal(cache$listFolder(),
                         file.path(folder_path, files))
  testthat::expect_equal(cache$listFolder(extract_name = TRUE), files)
  testthat::expect_equal(cache$listFolder(suffix = ".txt", extract_name = TRUE),
                         c("a.txt", "b.txt"))
  testthat::expect_equal(cache$listFolder(suffix = ".txt", extract_name = TRUE,
                                          remove_suffix = TRUE),
                         c("a", "b"))

  # Test with a tag
  cache$writeTag("foo")
  testthat::expect_length(cache$listFolder(), 3)
  testthat::expect_length(cache$listFolder(tag_files = TRUE), 5)

  # Create a folder and check count
  cache$getFolder("mysubfolder")
  testthat::expect_length(cache$listFolder(), 3)
  testthat::expect_length(cache$listFolder(folders = TRUE), 4)
})

testthat::test_that("delFolder() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)

  # Get/Create sub-folder
  myfolder <- cache$getFolder("abc")
  testthat::expect_true(dir.exists(myfolder))

  # Delete sub-folder
  testthat::expect_null(cache$delFolder("abc"))
  testthat::expect_false(dir.exists(myfolder))
})

testthat::test_that("delPaths() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()

  # Delete unexisting file
  myfile <- "myfile.txt"
  myfile_full <- file.path(folder_path, myfile)
  testthat::expect_false(file.exists(myfile_full))
  cache$delPaths(myfile)
  testthat::expect_false(file.exists(myfile_full))

  # Create file
  cache$saveContents("mytext", myfile)
  testthat::expect_true(file.exists(myfile_full))

  # Delete one file
  cache$delPaths(myfile)
  testthat::expect_false(file.exists(myfile_full))

  # Create 2 files
  cache$saveContents("mytext", myfile)
  testthat::expect_true(file.exists(myfile_full))
  myfile2 <- "my_second_file.txt"
  myfile2_full <- file.path(folder_path, myfile2)
  cache$saveContents("mytext", myfile2)
  testthat::expect_true(file.exists(myfile2_full))

  # Delete all files
  cache$delPaths()
  testthat::expect_false(file.exists(myfile_full))
  testthat::expect_false(file.exists(myfile2_full))

  # Check tag file is not deleted
  cache$writeTag("A")
  cache$delPaths()
  testthat::expect_true(cache$tagExists("A"))
  cache$saveContents("mytext", myfile)
  cache$delPaths(myfile)
  cache$delPaths("__A__")
  testthat::expect_true(cache$tagExists("A"))

  # Check sub-folder is not deleted
  testthat::expect_true(dir.exists(cache$getFolder("mysubfolder")))
  cache$delPaths()
  testthat::expect_true(dir.exists(cache$getFolder("mysubfolder")))
  cache$saveContents("mytext", myfile)
  cache$delPaths(myfile)
  cache$delPaths("mysubfolder")
  testthat::expect_true(dir.exists(cache$getFolder("mysubfolder")))
})

testthat::test_that("importFiles() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()

  # Create some files
  filenames <- c("o.txt", "p.csv")
  files <- file.path(wrk_dir, filenames)
  file.create(files)

  # Import
  cache$importFiles(files, sub_folder = "foo", action = "move")
  testthat::expect_false(any(file.exists(files)))
  testthat::expect_true(all(cache$pathsExist(filenames, sub_folder = "foo")))

  # Create files
  files <- file.path(wrk_dir, c("c.txt", "d.csv"))
  file.create(files)

  # Try to import with one destination only
  testthat::expect_error(cache$importFiles(files, dst = "onefile.txt"))

  # Import with right number of destinations
  dst <- c("e.txt", "f.csv")
  cache$importFiles(files, dst = dst)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_true(all(cache$pathsExist(dst)))

  # Create two new files
  files <- file.path(wrk_dir, c("g.tsv", "h.tsv"))
  file.create(files)

  # Import using suffix
  dst <- c("i", "j")
  suffix <- ".tsv"
  cache$importFiles(files, dst = dst, suffix = suffix)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_true(all(cache$pathsExist(dst, suffix = suffix)))
})

testthat::test_that("getSubFolders() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()
  testthat::expect_length(cache$getSubFolders(), 0)

  # Create one sub-folder
  folder_a <- cache$getFolder(sub_folder = "a")
  testthat::expect_equal(cache$getSubFolders(), "a")

  # Create a second sub-folder
  folder_b <- cache$getFolder(sub_folder = "b")
  testthat::expect_equal(cache$getSubFolders(), c("a", "b"))
})

testthat::test_that("getTmp() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  testthat::expect_equal(cache$getTmp(), file.path(folder_path, "tmp"))
})

testthat::test_that("tagExists() works fine", {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)
  cache$erase()

  # Create tag
  testthat::expect_false(file.exists(file.path(folder_path, "__A__")))
  testthat::expect_false(cache$tagExists("A"))
  cache$writeTag("A")
  testthat::expect_true(cache$tagExists("A"))
})

# nolint start: indentation_linter
testthat::test_that(paste("Deprecated parameters are marked",
                          "as deprecated and still work"), {

  # Create cache
  cache <- fscache::Cache$new(folder_path, user = FALSE)

  # getFolder()
  lifecycle::expect_deprecated(x <- cache$getFolder(sub.folder = 'foo'))
  y <- cache$getFolder(sub_folder = 'foo')
  testthat::expect_equal(x, y)

  # hasFolder()
  lifecycle::expect_deprecated(x <- cache$hasFolder(sub.folder = 'foo'))
  testthat::expect_true(x)

  # importFiles()
  setwd(wrk_dir)
  files <- 'a.txt'
  for (f in files) {
    write('', file=f)
  }
  lifecycle::expect_deprecated(cache$importFiles(files, sub.folder = 'foo',
                                                 action = 'move'))

  # saveContents()
  f <- 'b.txt'
  content <- 'abcde'
  lifecycle::expect_deprecated(cache$saveContents(content, f,
                                                  sub.folder = 'foo'))
  files <- c(files, f)

  # loadContents()
  lifecycle::expect_deprecated(x <- cache$loadContents(f, sub.folder = 'foo'))
  testthat::expect_equal(unname(x), content)

  # getPaths()
  paths <- files
  lifecycle::expect_deprecated(x <- cache$getPaths(paths, sub.folder = 'foo'))
  y <- cache$getPaths(paths, sub_folder = 'foo')
  testthat::expect_equal(x, y)

  # globPaths()
  lifecycle::expect_deprecated(x <- cache$globPaths(sub.folder = 'foo'))
  y <- cache$globPaths(sub_folder = 'foo')
  testthat::expect_equal(x, y)
  lifecycle::expect_deprecated(x <- cache$globPaths(tag.files = TRUE))
  y <- cache$globPaths(tag.files = TRUE)
  testthat::expect_equal(x, y)

  # getNbItems()
  lifecycle::expect_deprecated(x <- cache$getNbItems(sub.folder = 'foo'))
  y <- cache$getNbItems(sub_folder = 'foo')
  testthat::expect_equal(x, y)
  lifecycle::expect_deprecated(x <- cache$getNbItems(tag.files = TRUE))
  y <- cache$getNbItems(tag.files = TRUE)
  testthat::expect_equal(x, y)

  # pathsExist()
  lifecycle::expect_deprecated(x <- cache$pathsExist(paths, sub.folder = 'foo'))
  testthat::expect_true(all(x))

  # writeTag()
  tag <- 'XXX'
  lifecycle::expect_deprecated(cache$writeTag(tag, sub.folder = 'foo'))

  # tagExists()
  lifecycle::expect_deprecated(x <- cache$tagExists(tag, sub.folder = 'foo'))
  testthat::expect_true(x)

  # listFolder()
  lifecycle::expect_deprecated(x <- cache$listFolder(sub.folder = 'foo'))
  y <- cache$listFolder(sub_folder = 'foo')
  testthat::expect_equal(x, y)
  lifecycle::expect_deprecated(x <- cache$listFolder(sub.folder = 'foo',
                                                     remove.suffix = TRUE))
  y <- cache$listFolder(sub_folder = 'foo', remove_suffix = TRUE)
  testthat::expect_equal(x, y)
  lifecycle::expect_deprecated(x <- cache$listFolder(sub.folder = 'foo',
                                                     extract.name = TRUE))
  y <- cache$listFolder(sub_folder = 'foo', extract_name = TRUE)
  testthat::expect_equal(x, y)
  lifecycle::expect_deprecated(x <- cache$listFolder(tag.files = TRUE))
  y <- cache$listFolder(tag_files = TRUE)
  testthat::expect_equal(x, y)

  # delPaths()
  lifecycle::expect_deprecated(cache$delPaths(paths[1], sub.folder = 'foo'))

  # delFolder()
  lifecycle::expect_deprecated(cache$delFolder(sub.folder = 'foo'))
  testthat::expect_false(cache$hasFolder(sub_folder = 'foo'))

  # Erase cache
  cache$erase()
})
# nolint end

# Erase everything
cache <- fscache::Cache$new(folder_path)
cache$erase()
unlink(wrk_dir, recursive = TRUE)
