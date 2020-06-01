context('Read file and warning/error checks')

testthat::test_that('File creation',{
  testthat::expect_match(fars::make_filename(2013),'accident_2013.csv.bz2')
  testthat::gives_warning(fars::fars_read_years(2019))
  testthat::throws_error(fars::fars_map_state(51,2013))
})
