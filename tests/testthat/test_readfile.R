test_that("filename", {

        # class(fars_read(filename))
        expect_that(make_filename(2013), is_identical_to( "accident_2013.csv.bz2"))

})
