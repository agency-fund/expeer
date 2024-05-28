example_1 = function() {
  ex = "
@examples
library('data.table')

set.seed(2603)
n = 2000

pop = data.table(
  group_id = 1:n,
  state = rep_len(c('Karnataka', 'Haryana', 'Bihar'), n),
  num_members = rbinom(n, 50, 0.5),
  mean_engagement_1wk = rexp(n, 0.1),
  mean_engagement_2wk = rnorm(n, 50, 15),
  is_happy_2wk = rbinom(n, 1, 0.4))

pop[, district := paste(state, 'District', rep_len(1:2, n))]
pop[seq(5, n, 80), district := NA]
pop[seq(5, n, 41), num_members := NA]

########################################

design = yaml::read_yaml(
  system.file('extdata', 'expeer_design_01.yml', package = 'expeer'))

pop_filt = get_filtered_units(design, pop)

power_calcs = get_power(design, pop_filt)

num_randomized_units =
  power_calcs[metric_name == 'mean_engagement_2wk']$num_units_for_target_power

pop_rand = get_randomized_units(design, pop_filt, num_randomized_units)
"
  return(strsplit(ex, split = '\n')[[1L]])
}
