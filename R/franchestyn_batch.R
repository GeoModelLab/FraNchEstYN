#' Run multiple FraNchEstYN jobs in parallel, each with its own sandbox
#'
#' @param jobs A list of jobs (each is a list of franchestyn() args).
#' @param workers Number of parallel workers.
#' @param cleanup If TRUE, remove sandbox dirs after run (default TRUE).
#' @param cleanup_outputs If TRUE (default), remove output CSVs after reading;
#'   if FALSE, CSVs remain in outputs/ (useful for large batch runs).
#'
#' @return A list of franchestyn() results.
#' @export
franchestyn_batch <- function(jobs,
                              workers = future::availableCores(),
                              cleanup = TRUE,
                              cleanup_outputs = TRUE) {
  stopifnot(is.list(jobs), length(jobs) > 0)

  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required for parallel runs. Install with install.packages('future.apply').")
  }

  run_one <- function(job) {
    # unique sandbox per job
    sandbox <- tempfile("franchestyn_run_")
    dir.create(sandbox, recursive = TRUE)

    # copy exe into sandbox
    exe_src <- system.file("bin", "FraNchEstYN.exe", package = "FraNchEstYN")
    if (!file.exists(exe_src)) stop("❌ Package exe not found at ", exe_src)
    file.copy(exe_src, file.path(sandbox, "FraNchEstYN.exe"))

    on.exit(if (cleanup) unlink(sandbox, recursive = TRUE, force = TRUE), add = TRUE)

    # forward sandbox exe_dir
    do.call(franchestyn, c(job, list(.__exe_dir__ = sandbox,
                                     cleanup_outputs = cleanup_outputs)))
  }

  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::multisession, workers = workers)

  future.apply::future_lapply(jobs, run_one, future.seed = TRUE)
}
