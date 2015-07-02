
#' read and process all the structure output files
#'
#' reads in the structure output files and the trace files and
#' does relabeling of clusters to make them as concordant as possible.
#' @param Dir  directory in which the output resides
#' @return This returns a list with two components: \code{q} is a data frame
#' of inferred ancestry proportions. \code{traces} is a data frame of the
#' traces of a few key variables.
#' @export
read_and_process_structure_output <- function(Dir) {

  message("reading files")
  # read everything in
  results <- read_results(Dir)
  traces <- read_traces(Dir)


  # identify the maximum-a-posteriori q's
  res2 <- MAP_cluster(results)

  message("finding label permutations for the reps")
  # then relabel them for concordance
  suppressMessages(mp <- relabel_map_clusters(res2))

  # then get a data frame that actually holds those max-perms
  # then we can pick out just the max perm indexes for each K and r
  max_perms <- mp %>%
    group_by(K, Rep) %>%
    summarise(mp = first(max_perm))


  message("relabeling the traces and results")
  # and once we have that, we can relabel the traces and the results
  suppressMessages(traces_relab <- relabel_traces(traces, max_perms))
  suppressMessages(results_relab <- relabel_results(results, max_perms))


  # then send the results back:
  list(q = ungroup(results_relab), traces = ungroup(traces_relab))

}
