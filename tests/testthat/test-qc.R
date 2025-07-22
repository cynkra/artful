test_that("rtf_to_pdf throws error for non-existent file", {
  expect_error(
    rtf_to_pdf("non_existent_file.rtf"),
    "RTF file not found: non_existent_file.rtf"
  )
})

test_that("rtf_to_pdf generates correct PDF path when pdf_path is NULL", {
  # Create a temporary RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  writeLines("dummy RTF content", temp_rtf)
  
  # Mock system2 to avoid actual LibreOffice call
  with_mocked_bindings(
    system2 = function(...) invisible(0),
    .package = "base",
    {
      # Test that function runs without error
      expect_no_error(rtf_to_pdf(temp_rtf))
    }
  )
  
  # Clean up
  unlink(temp_rtf)
})

test_that("rtf_to_pdf handles directory creation when pdf_path is a directory", {
  # Create a temporary RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  writeLines("dummy RTF content", temp_rtf)
  
  # Create a temporary directory path that doesn't exist, ending with slash to indicate directory
  temp_dir <- file.path(tempdir(), "test_qc_dir/")
  
  # Mock system2 to avoid actual LibreOffice call
  with_mocked_bindings(
    system2 = function(...) invisible(0),
    .package = "base",
    {
      # Test that function runs without error and creates directory
      expect_no_error(rtf_to_pdf(temp_rtf, pdf_path = temp_dir))
      expect_true(dir.exists(temp_dir))
    }
  )
  
  # Clean up
  unlink(temp_rtf)
  unlink(temp_dir, recursive = TRUE)
})

test_that("rtf_to_pdf handles directory path ending with slash", {
  # Create a temporary RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  writeLines("dummy RTF content", temp_rtf)
  
  # Create a temporary directory path that doesn't exist, ending with slash
  temp_dir <- file.path(tempdir(), "test_qc_dir_slash/")
  
  # Mock system2 to avoid actual LibreOffice call
  with_mocked_bindings(
    system2 = function(...) invisible(0),
    .package = "base",
    {
      # Test that function runs without error and creates directory
      expect_no_error(rtf_to_pdf(temp_rtf, pdf_path = temp_dir))
      expect_true(dir.exists(temp_dir))
    }
  )
  
  # Clean up
  unlink(temp_rtf)
  unlink(temp_dir, recursive = TRUE)
})

test_that("rtf_to_pdf calls system2 with correct arguments", {
  # Create a temporary RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  writeLines("dummy RTF content", temp_rtf)
  
  # Track system2 calls
  system2_calls <- list()
  
  with_mocked_bindings(
    system2 = function(command, args, stdout, stderr, ...) {
      system2_calls[[length(system2_calls) + 1]] <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
      invisible(0)
    },
    .package = "base",
    {
      rtf_to_pdf(temp_rtf)
    }
  )
  
  # Verify system2 was called correctly
  expect_length(system2_calls, 1)
  call <- system2_calls[[1]]
  
  expect_equal(call$command, "/Applications/LibreOffice.app/Contents/MacOS/soffice")
  expect_true("--headless" %in% call$args)
  expect_true("--convert-to" %in% call$args)
  expect_true("pdf" %in% call$args)
  expect_true("--outdir" %in% call$args)
  expect_false(call$stdout)
  expect_false(call$stderr)
  
  # Clean up
  unlink(temp_rtf)
})

test_that("rtf_to_pdf works with custom soffice_path", {
  # Create a temporary RTF file
  temp_rtf <- tempfile(fileext = ".rtf")
  writeLines("dummy RTF content", temp_rtf)
  
  custom_path <- "/custom/path/to/soffice"
  
  # Track system2 calls
  system2_calls <- list()
  
  with_mocked_bindings(
    system2 = function(command, args, stdout, stderr, ...) {
      system2_calls[[length(system2_calls) + 1]] <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
      invisible(0)
    },
    .package = "base",
    {
      rtf_to_pdf(temp_rtf, soffice_path = custom_path)
    }
  )
  
  # Verify custom soffice_path was used
  expect_length(system2_calls, 1)
  expect_equal(system2_calls[[1]]$command, custom_path)
  
  # Clean up
  unlink(temp_rtf)
})