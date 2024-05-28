#' @import checkmate
#' @importFrom data.table data.table := set rbindlist %between%
NULL

#' Filter units in the population
#'
#' @param design List corresponding to an experimental design.
#' @param pop `data.frame` containing one row per unit in the population.
#'
#' @return `data.table` of units that meet the filtering criteria.
#'
#' @eval example_1()
#'
#' @seealso [get_randomized_units()]
#'
#' @export
get_filtered_units = function(design, pop) {
  pop = assert_args(design, pop)
  pop_filt = data.table::copy(pop)
  filt_col = NULL

  for (filt in design$filters) {
    env_arg = list(filt_col = filt$name)

    if (!is.null(filt$operator)) {
      op_fmt = if (isTRUE(filt$operator %in% c('between', 'in'))) {
        '`%%%s%%`'
      } else {
        '`%s`'
      }
      op_str = sprintf(op_fmt, filt$operator)
      op_func = eval(parse(text = op_str))
      pop_filt = pop_filt[op_func(filt_col, filt$value), env = env_arg]
    }

    if (!filt$allow_missing) {
      pop_filt = pop_filt[!is.na(filt_col), env = env_arg]
    }
  }
  pop_filt
}

#' @rdname get_randomized_units
#' @export
get_power = function(design, pop_filt) {
  pop_filt = assert_args(design, pop_filt)
  num_units_per_arm = floor(nrow(pop_filt) / length(design$arms))

  power_list = lapply(design$metrics, \(metric) {
    metric_values = pop_filt[[metric$name]]
    baseline_value = mean(metric_values, na.rm = TRUE)

    if (metric$type == 'continuous') {
      diff_value = if (metric$target_diff_scale == 'absolute') {
        metric$target_diff_value
      } else { # relative
        baseline_value * metric$target_diff_value
      }
      target_value = baseline_value + diff_value

      power_available = stats::power.t.test(
        n = num_units_per_arm, delta = diff_value,
        sd = stats::sd(metric_values), sig.level = metric$target_alpha)

      power_required = stats::power.t.test(
        delta = diff_value, sd = stats::sd(metric_values, na.rm = TRUE),
        sig.level = metric$target_alpha, power = metric$target_power)

    } else { # binary
      target_value = if (metric$target_diff_scale == 'absolute') {
        baseline_value + metric$target_diff_value
      } else { # relative
        baseline_value * (1 + metric$target_diff_value)
      }
      target_value = max(0, min(target_value, 1))

      power_available = stats::power.prop.test(
        n = num_units_per_arm, p1 = baseline_value, p2 = target_value,
        sig.level = metric$target_alpha)

      power_required = stats::power.prop.test(
        p1 = baseline_value, p2 = target_value, sig.level = metric$target_alpha,
        power = metric$target_power)
    }

    data.table(
      metric_name = metric$name, metric_type = metric$type,
      baseline_value = baseline_value, target_value = target_value,
      target_alpha = metric$target_alpha, target_power = metric$target_power,
      num_arms = length(design$arms), num_available_units = nrow(pop_filt),
      power_with_available_units = power_available$power,
      num_units_for_target_power = power_required$n * length(design$arms),
      sufficient_units_for_target_power =
        power_available$power >= metric$target_power)
  })
  power_calcs = rbindlist(power_list, use.names = TRUE, fill = TRUE)
  power_calcs
}

#' @rdname get_randomized_units
#' @export
get_stratified_units = function(design, pop_filt) {
  pop_filt = assert_args(design, pop_filt)
  .GRP = `_randomization_block` = x = NULL # nolint: object_name_linter

  pop_strat = pop_filt[, design$id_name, with = FALSE]
  data.table::setattr(pop_strat, 'id_name', design$id_name)

  for (stratum in design$strata) {
    col = stratum$name
    if (!is.numeric(pop_filt[[col]]) || isTRUE(stratum$type == 'categorical')) {
      pop_strat[, (col) := factor(pop_filt[[col]], exclude = NULL)]
    } else {
      pop_strat[, (col) := cut(pop_filt[[col]], breaks = stratum$num_levels)]
    }
    pop_strat[, (col) := as.character(x), env = list(x = col)]
    pop_strat[is.na(x), (col) := 'NA_level', env = list(x = col)]
  }

  strata_cols = sapply(design$strata, \(x) x$name)
  pop_strat[, `_randomization_block` := .GRP, by = strata_cols]
  pop_strat[]
}

#' Set up a randomized experiment
#'
#' `get_stratified_units()` is used by `get_randomized_units()` and does not
#' normally need to be called directly.
#'
#' @param design List corresponding to an experimental design.
#' @param pop_filt `data.frame` containing one row per unit available for the
#'   experiment.
#' @param num_randomized_units Number of units to randomize in the experiment.
#'
#' @return `get_power()` returns a `data.table` of power calculations, with one
#'   row per metric. `get_stratified_units()` returns a `data.table` with one
#'   row per unit and with columns for stratification. `get_randomized_units()`
#'   returns a `data.table` with one row per unit and with columns for
#'   stratification and random assignment to experiment arms.
#'
#' @eval example_1()
#'
#' @seealso [get_filtered_units()]
#'
#' @export
get_randomized_units = function(design, pop_filt, num_randomized_units) {
  pop_filt = assert_args(design, pop_filt)
  assert_number(num_randomized_units, lower = 2)
  assert_true(nrow(pop_filt) >= num_randomized_units)

  .N = `_arm_name` = `_randomization_block` = NULL # nolint: object_name_linter
  num_units = ceiling(num_randomized_units)

  pop_subset = pop_filt[sample.int(.N, num_units)]
  pop_rand = get_stratified_units(design, pop_subset)
  pop_rand[, `_arm_name` := randomizr::block_ra(
    `_randomization_block`, conditions = sapply(design$arms, \(x) x$name))]
  pop_rand[]
}
