testthat::context("Cache class")

# Remove default console appender
lgr::lgr$remove_appender(1)

# Temp folder, to work in
TEMP_DIR <- tempdir()

# Working folder
WRK_DIR <- file.path(TEMP_DIR, 'wrk')
dir.create(WRK_DIR, recursive=TRUE)

folder.name <- 'test-fscache'
folder.path <- file.path(TEMP_DIR, folder.name)

testthat::test_that("Class initialization works fine", {

    # No folder
    testthat::expect_error(fscache::Cache$new())

    # With absolute folder path
    cache <- fscache::Cache$new(folder.path)
    testthat::expect_is(cache, 'Cache')
    testthat::expect_true(dir.exists(folder.path))
#
#    # With current folder
#    cache <- fscache::Cache$new(folder.name)
#    testthat::expect_is(cache, 'Cache')
#    testthat::expect_true(dir.exists(cwd.folder.path))
    
    # Works fine with empty folder
    cache <- fscache::Cache$new(folder.path)
    cache$erase()
    dir.create(folder.path)
    cache <- fscache::Cache$new(folder.path)

    # Force with existing non-empty folder
    cache <- fscache::Cache$new(folder.path)
    cache$erase()
    dir.create(folder.path)
    file.create(file.path(folder.path, 'some_file.txt'))
    testthat::expect_error(fscache::Cache$new(folder.path))
    cache <- fscache::Cache$new(folder.path, force=TRUE)
})

testthat::test_that("We can save and load contents into files", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    
    # Loop on number of contents
    for (n in 1:3) {
        
        # Define contents and files
        contents <- as.character(seq(n))
        files <- paste0('data_', contents, '.txt')
        
        # Save contents
        cache$saveContents(contents, files)
        full.paths <- cache$getPaths(files)
        testthat::expect_true(all(file.exists(full.paths)))
        
        # Load contents
        contents_bis <- cache$loadContents(files)
        testthat::expect_identical( !! contents, !! unname(contents_bis))
        testthat::expect_identical( !! full.paths, !! names(contents_bis))
        
        # Load contents using suffix
        contents_bis <- cache$loadContents(paste0('data_', contents),
                                           suffix='.txt')
        testthat::expect_identical( !! contents, !! unname(contents_bis))
        testthat::expect_identical( !! full.paths, !! names(contents_bis))
    }
})

testthat::test_that("getFolder() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    
    # Main folder
    testthat::expect_identical( !! folder.path, !! cache$getFolder())
    
    # Refute sub-folder starting with a slash
    testthat::expect_error(cache$getFolder('/'))

    # Sub-folder
    x <- 'my_sub_folder'
    testthat::expect_identical(!! file.path(folder.path, x),
                               !! cache$getFolder(x))
    testthat::expect_true(dir.exists(cache$getFolder(x)))
    
    # Test create=FALSE
    x <- 'my_sub_folder_2'
    testthat::expect_identical(!! file.path(folder.path, x),
                               !! cache$getFolder(x, create=FALSE))
    testthat::expect_false(dir.exists(cache$getFolder(x, create=FALSE)))

    # Test fail=TRUE and create=FALSE
    testthat::expect_error(cache$getFolder('my_sub_folder_3', create=FALSE,
                                           fail=TRUE))
    
    # Test fail=TRUE and create=TRUE
    x <- 'my_sub_folder_4'
    testthat::expect_identical(!! file.path(folder.path, x),
                               !! cache$getFolder(x, fail=TRUE))
    testthat::expect_true(dir.exists(cache$getFolder(x, fail=TRUE)))
})

testthat::test_that("hasFolder() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)

    # Test main folder
    testthat::expect_true(cache$hasFolder())

    # Test existing sub-folder
    x <- 'my_sub_folder_10'
    cache$getFolder(x)
    testthat::expect_true(cache$hasFolder(x))

    # Test existing sub-folder
    x <- 'my_sub_folder_11'
    testthat::expect_false(cache$hasFolder(x))
})

testthat::test_that("getPaths() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)

    # Get one path
    x <- 'my_file.txt'
    testthat::expect_identical(!! file.path(folder.path, x),
                               !! cache$getPaths(x))

    # With suffix
    x <- 'my_file_2'
    suffix <- '.txt'
    testthat::expect_identical(!! file.path(folder.path, paste0(x, suffix)),
                               !! cache$getPaths(x, suffix=suffix))
})

testthat::test_that("globPaths() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    cache$erase()
    
    # Empty
    paths <- cache$globPaths()
    testthat::expect_is(paths, 'character')
    testthat::expect_length(paths, 0)
    paths <- cache$globPaths(tag.files=TRUE)
    testthat::expect_length(paths, 1)
    
    # Create one file
    f <- 'a_file.txt'
    cache$saveContents('abcd', f)
    paths <- cache$globPaths()
    testthat::expect_is(paths, 'character')
    testthat::expect_equal(paths, file.path(folder.path, f))
    
    # No file *.zip
    testthat::expect_length(cache$globPaths(suffix='.zip'), 0)
    
    # Tags are not listed
    cache$writeTag('A')
    paths <- cache$globPaths()
    testthat::expect_is(paths, 'character')
    testthat::expect_equal(paths, file.path(folder.path, f))
    
    # Folders are not listed
    cache$getFolder('mysubfolder')
    paths <- cache$globPaths()
    testthat::expect_length(paths, 1)
    paths <- cache$globPaths(folders=TRUE)
    testthat::expect_length(paths, 2)
})

testthat::test_that("Writable flag works", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    testthat::expect_true(cache$isWritable())

    # Set unwritable
    cache$setWritable(FALSE)
    testthat::expect_false(cache$isWritable())

    # Test that it is unwritable
    my.file <- 'test_writability.txt'
    testthat::expect_error(cache$saveContents('some content', my.file))

    # Set writable
    cache$setWritable(TRUE)
    testthat::expect_true(cache$isWritable())
})

testthat::test_that("Readable flag works", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    testthat::expect_true(cache$isReadable())

    # Set unreadable
    cache$setReadable(FALSE)
    testthat::expect_false(cache$isReadable())

    # Test that it is unreadable
    my.file <- 'test_readability.txt'
    cache$saveContents('some content', my.file)
    testthat::expect_error(cache$loadContents(my.file))

    # Set readable
    cache$setReadable(TRUE)
    testthat::expect_true(cache$isReadable())
})

testthat::test_that("We can erase the main cache folder", {

    # Folder name with user cache folder
    cache <- fscache::Cache$new(folder.path)
    cache$erase()
    testthat::expect_false(dir.exists(folder.path))

    # With current folder
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()
    testthat::expect_false(dir.exists(folder.path))
})

testthat::test_that("getNbItems() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    cache$erase()
    testthat::expect_equal(0, !! cache$getNbItems())
    
    # Create some items and test count
    n <- 3
    for (i in seq(n)) {
        cache$saveContents('Some content', paste0('file_', i), suffix='.txt')
        testthat::expect_equal(!!i, !!cache$getNbItems())
    }
    
    # Create a tag and check count
    cache$writeTag('foo')
    testthat::expect_equal(!!n, !!cache$getNbItems())
    testthat::expect_equal(!!n+2, !!cache$getNbItems(tag.files=TRUE))
    
    # Create a folder and check count
    cache$getFolder('mysubfolder')
    testthat::expect_equal(!!n, !!cache$getNbItems())
    testthat::expect_equal(!!n+1, !!cache$getNbItems(folders=TRUE))
})

testthat::test_that("print() works correctly", {

    # Create cache
    cache <- fscache::Cache$new(folder.path)
    
    # Check print
    testthat::expect_output(print(cache), "^Cache class.*$")
})

testthat::test_that("listFolder() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()
    
    # Create files
    files <- c("a.txt", "b.txt", "c.md")
    cache$saveContents(c("", "", ""), dst=files)
    
    # Get files
    testthat::expect_equal(cache$listFolder(),
                           file.path(folder.path, files))
    testthat::expect_equal(cache$listFolder(extract.name=TRUE), files)
    testthat::expect_equal(cache$listFolder(suffix='.txt', extract.name=TRUE),
                           c('a.txt', 'b.txt'))
    testthat::expect_equal(cache$listFolder(suffix='.txt', extract.name=TRUE,
                                            remove.suffix=TRUE),
                           c('a', 'b'))
    
    # Test with a tag
    cache$writeTag('foo')
    testthat::expect_length(cache$listFolder(), 3)
    testthat::expect_length(cache$listFolder(tag.files=TRUE), 5)
    
    # Create a folder and check count
    cache$getFolder('mysubfolder')
    testthat::expect_length(cache$listFolder(), 3)
    testthat::expect_length(cache$listFolder(folders=TRUE), 4)
})

testthat::test_that("delFolder() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    
    # Get/Create sub-folder
    myfolder <- cache$getFolder('abc')
    testthat::expect_true(dir.exists(myfolder))
    
    # Delete sub-folder
    testthat::expect_null(cache$delFolder('abc'))
    testthat::expect_false(dir.exists(myfolder))
})

testthat::test_that("delPaths() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()
    
    # Delete unexisting file
    myfile <- 'myfile.txt'
    myfile.full <- file.path(folder.path, myfile)
    testthat::expect_false(file.exists(myfile.full))
    cache$delPaths(myfile)
    testthat::expect_false(file.exists(myfile.full))

    # Create file
    cache$saveContents('mytext', myfile)
    testthat::expect_true(file.exists(myfile.full))

    # Delete one file
    cache$delPaths(myfile)
    testthat::expect_false(file.exists(myfile.full))

    # Create 2 files
    cache$saveContents('mytext', myfile)
    testthat::expect_true(file.exists(myfile.full))
    myfile2 <- 'my_second_file.txt'
    myfile2.full <- file.path(folder.path, myfile2)
    cache$saveContents('mytext', myfile2)
    testthat::expect_true(file.exists(myfile2.full))

    # Delete all files
    cache$delPaths()
    testthat::expect_false(file.exists(myfile.full))
    testthat::expect_false(file.exists(myfile2.full))
    
    # Check tag file is not deleted
    cache$writeTag('A')
    cache$delPaths()
    testthat::expect_true(cache$tagExists('A'))
    cache$saveContents('mytext', myfile)
    cache$delPaths(myfile)
    cache$delPaths('__A__')
    testthat::expect_true(cache$tagExists('A'))
    
    # Check sub-folder is not deleted
    testthat::expect_true(dir.exists(cache$getFolder('mysubfolder')))
    cache$delPaths()
    testthat::expect_true(dir.exists(cache$getFolder('mysubfolder')))
    cache$saveContents('mytext', myfile)
    cache$delPaths(myfile)
    cache$delPaths('mysubfolder')
    testthat::expect_true(dir.exists(cache$getFolder('mysubfolder')))
})

testthat::test_that("importFiles() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()
    
    # Create some files
    filenames <- c('o.txt', 'p.csv')
    files <- file.path(WRK_DIR, filenames)
    file.create(files)

    # Import
    cache$importFiles(files, sub.folder='foo', action='move')
    testthat::expect_false(any(file.exists(files)))
    testthat::expect_true(all(cache$pathsExist(filenames, sub.folder='foo')))
    
    # Create files
    files <- file.path(WRK_DIR, c('c.txt', 'd.csv'))
    file.create(files)
    
    # Try to import with one destination only
    testthat::expect_error(cache$importFiles(files, dst='onefile.txt'))

    # Import with right number of destinations
    dst <- c('e.txt', 'f.csv')
    cache$importFiles(files, dst=dst)
    testthat::expect_true(all(file.exists(files)))
    testthat::expect_true(all(cache$pathsExist(dst)))
    
    # Create two new files
    files <- file.path(WRK_DIR, c('g.tsv', 'h.tsv'))
    file.create(files)

    # Import using suffix
    dst <- c('i', 'j')
    suffix <- '.tsv'
    cache$importFiles(files, dst=dst, suffix=suffix)
    testthat::expect_true(all(file.exists(files)))
    testthat::expect_true(all(cache$pathsExist(dst, suffix=suffix)))
})

testthat::test_that("getSubFolders() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()
    testthat::expect_length(cache$getSubFolders(), 0)

    # Create one sub-folder
    folder_a <- cache$getFolder(sub.folder='a')
    testthat::expect_equal(cache$getSubFolders(), 'a')

    # Create a second sub-folder
    folder_b <- cache$getFolder(sub.folder='b')
    testthat::expect_equal(cache$getSubFolders(), c('a', 'b'))
})

testthat::test_that("getTmp() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    testthat::expect_equal(cache$getTmp(), file.path(folder.path, 'tmp'))
})

testthat::test_that("tagExists() works fine", {

    # Create cache
    cache <- fscache::Cache$new(folder.path, user=FALSE)
    cache$erase()

    # Create tag
    testthat::expect_false(file.exists(file.path(folder.path, '__A__')))
    testthat::expect_false(cache$tagExists('A'))
    cache$writeTag('A')
    testthat::expect_true(cache$tagExists('A'))
})

# Erase everything
cache <- fscache::Cache$new(folder.path)
cache$erase()
unlink(WRK_DIR, recursive=TRUE)
