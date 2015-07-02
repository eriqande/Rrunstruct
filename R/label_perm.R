library(combinat)


# a few functions to deal with permuting labels so that the are concordant across runs.


#' return the maximum-a-posteriori cluster label for an individual given K and rep
#'
#' @param df long-format data frame like that which comes out of \code{\link{read_results}}.
#' Minimally it should have the columns \code{K}, \code{rep}, \code{Index}, \code{cluster}, and
#' \code{probability}.
#' @return a summarised data frame with the column \code{map_cluster} in place of \code{cluster}.
MAP_cluster  <- function(df) {
  df %>%
    group_by_(.dots = names(df)[!(names(df) %in% c("cluster", "probability")) ])  %>% # group by everything except cluster and probability
    filter(probability == max(probability)) %>% # get the row that has maximum posterior
    summarise_each(funs(first)) %>% # take only one if the max posterior is shared between clusters
    transmute(map_cluster = cluster) %>%
    ungroup
}






# this is a window function
relabel_by_max_perm <- function(c1, c2, k) {
  k <- as.numeric(as.character(k[1]))
  permy <- permn(k)
  scores <- sapply(permy, function(x) sum(c1 == x[as.numeric(c2)]))
  maxp <- which.max(scores)
  permy[[maxp]][as.numeric(c2)]

}

# this is a summary function
find_max_perm <- function(c1, c2, k) {
  k <- as.numeric(as.character(k[1]))
  permy <- permn(k)
  scores <- sapply(permy, function(x) sum(c1 == x[as.numeric(c2)]))
  which.max(scores)
}


#' for each rep > 1 within a given K, return the permutation index (from kperm) that maximizes the
#' concordance in map_cluster between the first rep and the current one.
#' @param df this should be the output from MAP_cluster
relabel_map_clusters <- function(df) {

  # get the map_clusters from rep 1 and attach them to the original data frame
  # and then for each rep > 1 determine which permutation gives the closest correspondence
  # with rep 1, and put that on there
  df2 <- df %>%
    filter(Rep == 1) %>%
    mutate(rep1_map_cluster = map_cluster) %>%
    select(-map_cluster, -Rep) %>%
    ungroup %>%
    left_join(df, .)

  df3 <- df2 %>%  # slap those values back onto the original data frame
    group_by(K, Rep) %>%  # now, group it by K and rep and compute the permutation that makes each rep look most like rep 1
    summarise(max_perm = find_max_perm(rep1_map_cluster, map_cluster, K)) %>%
    left_join(df2, .) %>%
    group_by(K, Rep) %>%
    mutate(map_cluster_relabeled = relabel_by_max_perm(rep1_map_cluster, map_cluster, K))

  df3


}



# fix Fs
fix_fs <- function(v, mp, k) {

  mp <- as.numeric(as.character(mp[1]))
  k <- as.numeric(as.character(k[1]))



  perm <- permn(k)[[mp]]
  new.v <- v
  fs <- str_detect(new.v, "^F")
  nums <- as.numeric(str_replace_all(new.v[fs], "F", "") )
  new.nums <- perm[nums]
  new.v[fs] <- paste("F", new.nums, sep = "")
  new.v
}


relabel_traces <- function(tr, max_perms) {
  tr %>%
    inner_join(ungroup(max_perms)) %>%
    group_by(K, Rep) %>%
    mutate(variables_relabeled = fix_fs(as.character(variable), mp, K))
}



# now we have to relabel the results.
fix_clust <- function(cl, mp, k) {
  mp <- as.numeric(as.character(mp[1]))
  k <- as.numeric(as.character(k[1]))

  perm <- permn(k)[[mp]]

  perm[cl]

}


relabel_results <- function(res, max_perms) {
  res %>%
    inner_join(ungroup(max_perms)) %>%
    group_by(K, Rep) %>%
    mutate(cluster_relabeled = fix_clust(cluster, mp, K))
}
