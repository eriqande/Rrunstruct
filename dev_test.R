


setwd("~/Desktop/scottish_cats/new")
structure_runs("Boing", 1:4, 3, 1234)

results <- read_results("Boing")
traces <- read_traces("Boing")

res2 <- MAP_cluster(results)

mp <- relabel_map_clusters(res2)

# then we can pick out just the max perm indexes for each K and r
max_perms <- mp %>%
  group_by(K, Rep) %>%
  summarise(mp = first(max_perm))

# and once we have that, we can relabel the traces and the results
traces_relab <- relabel_traces(traces, max_perms)


ggplot(traces_relab %>% filter(str_detect(variable, "^F[0-9]")), aes(x = Sweep, y = value, colour = variables_relabeled)) +
  geom_line() +
  facet_grid(K ~ Rep)


results_relab <- relabel_results(results, max_perms)

# now prepare to plot these with Rep 1 values on the x axes...

prep <- results_relab %>%
  filter(Rep == 1) %>%
  group_by(K, Index, cluster_relabeled) %>%
  transmute(rep1_cluster = cluster_relabeled, rep1_prob = probability) %>%
  ungroup %>%
  inner_join(results_relab) %>%
  filter(Rep != 1) %>%
  mutate(which_cluster = paste(rep1_cluster))




ggplot(prep, aes(x = rep1_prob, y = probability, colour = which_cluster)) +
  geom_point() +
  facet_grid(K ~ Rep)
