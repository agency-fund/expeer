assert_filter = function(filt, pop) {
  assert_names(
    names(filt), subset.of = c('name', 'operator', 'value', 'allow_missing'))
  assert_string(filt$name)
  assert_choice(filt$name, colnames(pop))
  assert_flag(filt$allow_missing)
  assert(is.null(filt$operator) == is.null(filt$value))
  if (!is.null(filt$operator)) {
    assert_string(filt$operator)
    assert_choice(
      filt$operator, c('==', '!=', '<', '<=', '>', '>=', 'between', 'in'))
    assert_true(is.numeric(filt$value) == is.numeric(pop[[filt$name]]))
    assert(
      check_true(filt$operator %in% c('==', '!=') && test_scalar(filt$value)),
      check_true(
        filt$operator %in% c('<', '<=', '>', '>=') && test_number(filt$value)),
      check_true(
        filt$operator == 'between' &&
          test_numeric(
            filt$value, len = 2L, any.missing = FALSE, unique = TRUE,
            sorted = TRUE)),
      check_true(
        filt$operator == 'in' && test_atomic_vector(filt$value, unique = TRUE)))
  }
  invisible(filt)
}

assert_stratum = function(stratum, pop) {
  assert_names(
    names(stratum), type = 'unique',
    subset.of = c('name', 'type', 'num_levels'))
  assert_string(stratum$name)
  assert_choice(stratum$name, colnames(pop))
  if (is.numeric(pop[[stratum$name]])) {
    assert_true(
      (test_int(stratum$num_levels, lower = 2L, upper = 10L) &&
         (is.null(stratum$type) || isTRUE(stratum$type == 'numeric'))) ||
        (is.null(stratum$num_levels) && isTRUE(stratum$type == 'categorical')))
  } else {
    assert_true(
      is.null(stratum$num_levels) &&
        (is.null(stratum$type) || isTRUE(stratum$type == 'categorical')))
  }
  invisible(stratum)
}

assert_metric = function(metric, pop) {
  perm_names = c(
    'name', 'type',
    paste0('target_', c('diff_value', 'diff_scale', 'alpha', 'power')))
  assert_names(names(metric), type = 'unique', permutation.of = perm_names)
  assert_string(metric$name)
  assert_choice(metric$name, colnames(pop))
  assert(
    check_true(metric$type == 'continuous' && is.numeric(pop[[metric$name]])),
    check_true(
      metric$type == 'binary' &&
      (is.numeric(pop[[metric$name]]) &&
         all(setdiff(pop[[metric$name]], NA) %in% 0:1) ||
        is.logical(pop[[metric$name]]))))
  assert_string(metric$target_diff_scale)
  assert_choice(metric$target_diff_scale, c('absolute', 'relative'))
  assert_number(metric$target_diff_value)
  assert_number(metric$target_alpha, lower = 0.001, upper = 0.5)
  assert_number(metric$target_power, lower = 0.01, upper = 0.99)
  invisible(metric)
}

assert_args = function(design, pop) {
  # TODO: assert_filter, _stratum, or _metric should say which element failed
  assert_list(design)
  perm_names = c(
    'population', 'id_name', 'filters', 'strata', 'metrics', 'arms')
  assert_names(names(design), type = 'unique', permutation.of = perm_names)
  assert_string(design$population)
  assert_string(design$id_name)
  assert_choice(design$id_name, colnames(pop))

  assert_data_frame(pop, types = 'atomicvector', min.rows = 2L)
  assert_names(colnames(pop), type = 'unique')
  assert_atomic(pop[[design$id_name]], any.missing = FALSE, unique = TRUE)
  if (!data.table::is.data.table(pop)) pop = data.table::as.data.table(pop)

  assert_list(design$filters, null.ok = TRUE)
  lapply(design$filters, assert_filter, pop = pop)

  assert_list(design$strata, null.ok = TRUE)
  lapply(design$strata, assert_stratum, pop = pop)

  assert_list(design$metrics)
  lapply(design$metrics, assert_metric, pop = pop)

  assert_list(design$arms)
  assert_true(all(sapply(
    design$arms, \(x) test_string(x$name, min.chars = 1L))))
  assert_names(sapply(design$arms, \(x) x$name), type = 'unique')
  invisible(pop)
}
