
test_files <- list.files('tests/testthat/', full.names = TRUE)
for(file in test_files) source(file)
