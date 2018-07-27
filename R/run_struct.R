

#### Functions for running Structure from R ####

#' check what type of OS and then figure out the path to the structure executable appropriately
struct_binary_path <- function() {
  if(.Platform$OS.type=="windows") {
    path <- file.path(system.file(package="Rrunstruct"), "bin/structure_pc.exe")
  }
  else {
    if(.Platform$OS.type=="unix") {
      if(Sys.info()["sysname"]=="Darwin") {
        path <- file.path(system.file(package="Rrunstruct"), "bin/structure_mac")
      } else {
        message(paste("This appears to be a non-Mac Unix architecture.  We'll hope the official Linux binary works for you here....Sys.info()[\"sysname\"] =", Sys.info()["sysname"]))
        path <- file.path(system.file(package="Rrunstruct"), "bin/structure_linux")
      }
    }
  }
  if(!file.exists(path)) stop(paste("structure executable should be installed at", path,"but does not seem to be there"))

  return(path)
}

#' internal function to run one rep of structure.  This will get wrapped.
.run_struct <- function(bin = struct_binary_path(), outdir, K, rep, seed) {
  if(!file.exists("mainparams") || !file.exists("extraparams")) {
    stop("Your current directory should be within a parameter-set directory in a Structure project.
         Please use setwd() to get to such a directory.  It will contain the files mainparams
         and extraparams...If it does not, you may need to do at least one Structure run with the
         structure GUI before proceeding...")
  }

  # make a directory for the output
  if(!file.exists(outdir)) {
    dir.create(outdir)
  }

  # form the outfile name:
  outfile <- file.path(outdir, paste("struct_results_K_", K, "_rep_", rep, sep = ""))
  stdoutf <- file.path(outdir, paste("struct_stdout_K_", K, "_rep_", rep, sep = ""))

  message(paste("Launching structure. Writing stdout to file", stdoutf, "at", date()))

  system2(command = bin,
          args = paste("-K", K, "-o", outfile, "-D", seed),
          stdout = stdoutf,
          stderr = stdoutf)

  message(paste("Done with structure. Results written to file", outfile, "at", date(), "\n"))
}


#' run structure at a specified set of K's for a specified number of reps
#'
#' @param K a vector of K values to simulate at.  They can't be duplicated
#' @param reps a vector of the number of reps to simulate for each K value.  This
#' recycles to the length of the K values vector.
#' @param outdir The name of the output directory to use.  The directory will be
#' created if it does not already exist.  Note, if you give a directory that already
#' has output, that old output might get overwritten.
#' @export
structure_runs <- function(outdir, K, reps, seed) {
  if(any(duplicated(K))) stop("duplicated values now allowed in the K argument")

  reps <- rep(reps, length.out = length(K))
  newseed <- seed
  for(i in 1:length(K)) {
    for(j in 1:reps[i]) {
      .run_struct(outdir = outdir, K = K[i], rep = j, seed = newseed)
      newseed <- newseed + 1
    }
  }
}
