testthat::context("Functions")

# Working folder
WRK_DIR <- tempdir()

testthat::test_that("load_text_content() works fine", {

    # Check input values
    testthat::expect_error(fscache::load_text_content(1))
    testthat::expect_error(fscache::load_text_content('a', min.confidence=-1))
    testthat::expect_error(fscache::load_text_content('a', min.confidence=1.1))

    # Try empty file
    f <- file.path(WRK_DIR, 'empty.txt')
    txt <- ''
    cat(txt, file=f)
    content <- fscache::load_text_content(f)
    content <- stringr::str_trim(content)
    testthat::expect_length(content, 1)
    testthat::expect_equal(txt, content)
                        
    # Try ASCII file
    f <- file.path(WRK_DIR, 'ascii.txt')
    txt <- 'ABCDEF'
    cat(txt, file=f)
    content <- fscache::load_text_content(f)
    content <- stringr::str_trim(content)
    testthat::expect_length(content, 1)
    testthat::expect_equal(txt, content)
                        
    # Try UTF-8 file
    f <- file.path(WRK_DIR, 'utf8.txt')
    txt <- 'éàîêò'
    cat(txt, file=f)
    content <- fscache::load_text_content(f)
    content <- stringr::str_trim(content)
    testthat::expect_length(content, 1)
    testthat::expect_equal(txt, content)

    # Try Latin-1 encoding
    f <- file.path(WRK_DIR, 'latin1.txt')
    x <- iconv("Qui sème le vent récolte la tempête.", from="utf8", to="latin1") 
    testthat::expect_equal(Encoding(x), "latin1")
    stringi::stri_write_lines(x, f, encoding="latin1")
    content <- fscache::load_text_content(f)
    content <- stringr::str_trim(content)
    testthat::expect_length(content, 1)
    testthat::expect_equal(x, content)

    # Unknown encoding
    f <- file.path(WRK_DIR, 'unknown_enc.txt')
    cat('\x02\x10\x51\x52\xd4', file=f)
    testthat::expect_true(is.na(fscache::load_text_content(f,
                                                           min.confidence=1.0)))
})
