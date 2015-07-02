


#### Reading the stdout files  ####

#' slurp out the traces from a file and return them as a data frame with headers
#' @param file the name of the stdout file to slurp
#' @param K the value of K that this was run at
#' @param rep Which rep number was this structure run.
slurp_traces <- function(file, K, rep) {
  x <- readLines(file)
  x <- x[1:which(str_detect(x, "MCMC completed"))]  # ditch part of file after traces

  # pick out lines that have a sweep number followed by a colon
  # and then remove the colon and the leading and trailing spaces
  # and also toss the LnPD "0", if it on there.
  x2 <- x[str_detect(x, "^ *[0-9]+:")]  %>%
    str_replace_all(":", "") %>%
    str_trim %>%
    str_replace_all(" +0$", "")


  # get the header and fix spaces in names, etc
  header <- x[str_detect(x, "Rep#:")][1] %>%
    str_replace_all("Ln Like", "LnLike") %>%
    str_replace_all("Rep#:", "Sweep") %>%
    str_trim

  # then catenate those and read.table them through a textConnection, add the K to
  # it and tbl_df it.
  tbl_df(cbind(K = K, Rep = rep, read.table(textConnection(c(header, x2)), header = TRUE, na.strings = "--")))
}


#' make the trace data frame tidy and long format
gather_traces <- function(df) {
  gather(df, "variable", "value", -(K:Sweep))
}



#' read all the traces in from all the files in the specified output directory
#'
#' this function also puts everything into long (tidy) format
#'
#' @param dir  the directory holding all the text output from the structure_run() function
#' @export
read_traces <- function(D) {
  files <- dir(D, pattern = "*stdout*", full.names = TRUE)  # files to slurp



  # cycle over files, slurp em in, and rbind it all at the end
  lapply(files, function(x) {
    # some stuff to get the K and rep index
    fn <- str_replace(x, "^.*stdout_K", "")
    K <- str_split(fn, "_")[[1]][2]
    rep <- str_split(fn, "_")[[1]][4]

    slurp_traces(x, K = K, rep = rep) %>%
      gather_traces
  }) %>%
    do.call(rbind, .)

}


#### Functions to read the results files ####

slurp_results <- function(file, K, Rep) {
  x <- suppressWarnings(readLines(file))
  start <-which(str_detect(x, "^Inferred ancestry of individuals"))
  end <- which(str_detect(x, "^Estimated Allele Frequencies"))

  # get the elements of the header
  header <- x[start+1] %>%
    str_replace_all("[^a-zA-Z ]", "") %>%
    paste("Index", .) %>%
    str_replace("Inferred clusters", "") %>%
    str_trim %>%
    str_split(" +") %>%
    "[["(1)


  # then extract the output for each individual
  indivs <- x[(start+2):end]

  # then get that all in a data frame
  tmp <- indivs[str_detect(indivs, "[0-9]")] %>% # toss blank lines
    str_replace_all("[():]", "")  %>%  # toss the ()'s and the :'s from the output
    str_trim

  # read it into a table
  df <- read.table(textConnection(tmp), header = FALSE)

  # then make the header as appropriate
  names(df) <- c(header, 1:(ncol(df)-length(header)) )

  tbl_df(cbind(K = K, Rep = Rep, df))
}


#' makes a long format data frame of the results
gather_results <- function(df) {
 gather_(df, "cluster", "probability", gather_cols = names(df)[str_detect(names(df), "^[1-9]")])
}




#' read all the results in from all the files in the specified output directory
#'
#' this function also puts everything into long (tidy) format
#'
#' @param D  the directory holding all the text output from the structure_run() function
#' @export
read_results <- function(D) {
  files <- dir(D, pattern = "*results*", full.names = TRUE)  # files to slurp



  # cycle over files, slurp em in, and rbind it all at the end
  lapply(files, function(x) {
    # some stuff to get the K and rep index
    fn <- str_replace(x, "^.*results_K", "")
    K <- str_split(fn, "_")[[1]][2]
    Rep <- str_split(fn, "_")[[1]][4]

    slurp_results(x, K = K, Rep = Rep) %>%
      gather_results
  }) %>%
    do.call(rbind, .)

}


