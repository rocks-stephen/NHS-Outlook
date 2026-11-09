core_outlook_axis_label <- function(value, unit) {
  if (unit == "proportion") return(sprintf("%.0f%%", 100 * value))
  if (unit == "minutes") return(sprintf("%.0f min", value))
  format(round(value), big.mark = ",", scientific = FALSE)
}

core_outlook_y_range <- function(values, unit) {
  z <- as.numeric(values[is.finite(values)])
  if (!length(z)) return(c(0, 1))
  span <- diff(range(z))
  padding <- max(span * 0.16, if (unit == "proportion") 0.015 else 1)
  lower <- min(z) - padding
  upper <- max(z) + padding
  if (unit == "proportion") c(max(0, lower), min(1, upper)) else c(max(0, lower), upper)
}

core_outlook_points <- function(dates, values, x_position, y_position) {
  paste(vapply(seq_along(dates), function(i) paste0(
    sprintf("%.1f", x_position(dates[i])), ",",
    sprintf("%.1f", y_position(values[i]))
  ), character(1)), collapse = " ")
}

core_recent_forecast_svg <- function(history, next_release, metric_row,
                                     actual_value = NA_real_) {
  z <- data.table::copy(history[complete_submission == TRUE & is.finite(value)])
  data.table::setorder(z, calendar_month)
  target_month <- data.table::as.IDate(next_release$forecast_month[1L])
  if (is.finite(actual_value)) z <- z[calendar_month < target_month]
  z <- utils::tail(z, 18L)
  dates <- data.table::as.IDate(c(as.Date(z$calendar_month), as.Date(target_month)))
  point <- next_release$predicted_value[1L]
  y_range <- core_outlook_y_range(c(
    z$value, point, actual_value,
    next_release$lower_80[1L], next_release$upper_80[1L]
  ), metric_row$unit[1L])
  left <- 58; right <- 742; top <- 22; bottom <- 190
  x_position <- function(date) {
    index <- match(as.character(date), as.character(dates))
    left + (index - 1L) * (right - left) / max(1L, length(dates) - 1L)
  }
  y_position <- function(value) {
    bottom - (value - y_range[1L]) * (bottom - top) / diff(y_range)
  }
  ticks <- seq(y_range[1L], y_range[2L], length.out = 3L)
  grid <- paste(vapply(ticks, function(value) paste0(
    '<line class="chart-grid" x1="', left, '" x2="', right,
    '" y1="', sprintf("%.1f", y_position(value)), '" y2="',
    sprintf("%.1f", y_position(value)), '"></line>',
    '<text class="axis-label" x="4" y="',
    sprintf("%.1f", y_position(value) + 4), '">',
    core_outlook_axis_label(value, metric_row$unit[1L]), '</text>'
  ), character(1)), collapse = "")
  tick_indices <- unique(round(seq(1, length(dates), length.out = min(4L, length(dates)))))
  x_ticks <- paste(vapply(seq_along(tick_indices), function(j) {
    i <- tick_indices[j]
    anchor <- if (j == 1L) "start" else if (j == length(tick_indices)) {
      "end"
    } else {
      "middle"
    }
    paste0(
      '<text class="axis-label" x="', sprintf("%.1f", x_position(dates[i])),
      '" y="217" text-anchor="', anchor, '">',
      performance_format_short_month(dates[i]), '</text>'
    )
  }, character(1)), collapse = "")
  history_points <- core_outlook_points(
    z$calendar_month, z$value, x_position, y_position
  )
  last_month <- utils::tail(z$calendar_month, 1L)
  last_value <- utils::tail(z$value, 1L)
  target_x <- x_position(target_month)
  paste0(
    '<svg class="outlook-chart" viewBox="0 0 770 225" role="img" ',
    'aria-label="Recent performance and next-release forecast">', grid,
    '<polyline class="actual-line" points="', history_points, '"></polyline>',
    '<line class="forecast-link" x1="', sprintf("%.1f", x_position(last_month)),
    '" y1="', sprintf("%.1f", y_position(last_value)), '" x2="',
    sprintf("%.1f", target_x), '" y2="', sprintf("%.1f", y_position(point)),
    '"></line>',
    '<line class="forecast-range" x1="', sprintf("%.1f", target_x),
    '" x2="', sprintf("%.1f", target_x), '" y1="',
    sprintf("%.1f", y_position(next_release$upper_80[1L])), '" y2="',
    sprintf("%.1f", y_position(next_release$lower_80[1L])), '"></line>',
    '<circle class="forecast-dot" cx="', sprintf("%.1f", target_x), '" cy="',
    sprintf("%.1f", y_position(point)), '" r="5"></circle>',
    '<text class="chart-label" x="', sprintf("%.1f", target_x - 6), '" y="',
    sprintf("%.1f", y_position(point) - 10), '" text-anchor="end">',
    performance_format_value(point, metric_row$unit[1L], metric_row$digits[1L]),
    '</text>', if (is.finite(actual_value)) paste0(
      '<circle class="actual-release-dot" cx="', sprintf("%.1f", target_x),
      '" cy="', sprintf("%.1f", y_position(actual_value)), '" r="6"></circle>',
      '<text class="chart-label" x="', sprintf("%.1f", target_x - 6),
      '" y="', sprintf("%.1f", y_position(actual_value) + 16),
      '" text-anchor="end">Actual ',
      performance_format_value(
        actual_value, metric_row$unit[1L], metric_row$digits[1L]
      ), '</text>'
    ) else "", x_ticks, '</svg>'
  )
}

core_planning_forecast_svg <- function(history, projection, targets, metric_row) {
  z <- data.table::copy(history[complete_submission == TRUE & is.finite(value)])
  data.table::setorder(z, calendar_month)
  z <- utils::tail(z, 48L)
  p <- data.table::copy(projection)
  data.table::setorder(p, forecast_month)
  first_month <- min(z$calendar_month)
  final_month <- max(p$forecast_month)
  first_id <- month_id(first_month); final_id <- month_id(final_month)
  left <- 58; right <- 742; top <- 22; bottom <- 190
  x_position <- function(date) {
    left + (month_id(date) - first_id) * (right - left) / (final_id - first_id)
  }
  y_range <- core_outlook_y_range(c(
    z$value, p$predicted_value, p$lower_80, p$upper_80, targets$target_value
  ), metric_row$unit[1L])
  y_position <- function(value) {
    bottom - (value - y_range[1L]) * (bottom - top) / diff(y_range)
  }
  ticks <- seq(y_range[1L], y_range[2L], length.out = 3L)
  grid <- paste(vapply(ticks, function(value) paste0(
    '<line class="chart-grid" x1="', left, '" x2="', right,
    '" y1="', sprintf("%.1f", y_position(value)), '" y2="',
    sprintf("%.1f", y_position(value)), '"></line>',
    '<text class="axis-label" x="4" y="',
    sprintf("%.1f", y_position(value) + 4), '">',
    core_outlook_axis_label(value, metric_row$unit[1L]), '</text>'
  ), character(1)), collapse = "")
  actual_points <- core_outlook_points(
    z$calendar_month, z$value, x_position, y_position
  )
  forecast_dates <- data.table::as.IDate(c(
    as.Date(max(z$calendar_month)), as.Date(p$forecast_month)
  ))
  forecast_values <- c(utils::tail(z$value, 1L), p$predicted_value)
  forecast_points <- core_outlook_points(
    forecast_dates, forecast_values, x_position, y_position
  )
  ribbon_points <- paste(
    core_outlook_points(p$forecast_month, p$upper_80, x_position, y_position),
    core_outlook_points(rev(p$forecast_month), rev(p$lower_80), x_position, y_position)
  )
  target_marks <- paste(vapply(seq_len(nrow(targets)), function(i) {
    target_x <- x_position(targets$target_month[i])
    target_y <- y_position(targets$target_value[i])
    # Long labels attached to a target at the final plotted month otherwise
    # extend beyond the SVG viewport. Put right-edge labels to the left of the
    # marker while retaining the natural left-to-right placement elsewhere.
    target_anchor <- if (target_x >= right - 120) "end" else "start"
    target_label_x <- target_x + if (target_anchor == "end") -6 else 6
    paste0(
      '<circle class="target-dot" cx="', sprintf("%.1f", target_x), '" cy="',
      sprintf("%.1f", target_y), '" r="4"></circle>',
      '<text class="target-label" x="', sprintf("%.1f", target_label_x), '" y="',
      sprintf("%.1f", target_y - 7), '" text-anchor="', target_anchor, '">',
      performance_html_escape(targets$target_label[i]), '</text>'
    )
  }, character(1)), collapse = "")
  tick_dates <- unique(data.table::as.IDate(c(
    as.Date(first_month), as.Date(max(z$calendar_month)),
    as.Date(targets$target_month), as.Date(final_month)
  )))
  tick_dates <- tick_dates[tick_dates >= first_month & tick_dates <= final_month]
  x_ticks <- paste(vapply(seq_along(tick_dates), function(i) {
    anchor <- if (i == 1L) "start" else if (i == length(tick_dates)) {
      "end"
    } else {
      "middle"
    }
    paste0(
      '<text class="axis-label" x="', sprintf("%.1f", x_position(tick_dates[i])),
      '" y="217" text-anchor="', anchor, '">',
      performance_format_short_month(tick_dates[i]), '</text>'
    )
  }, character(1)), collapse = "")
  paste0(
    '<svg class="outlook-chart" viewBox="0 0 770 225" role="img" ',
    'aria-label="Medium-term projection and planning targets">', grid,
    '<polygon class="planning-ribbon" points="', ribbon_points, '"></polygon>',
    '<polyline class="actual-line" points="', actual_points, '"></polyline>',
    '<polyline class="planning-forecast" points="', forecast_points,
    '"></polyline>', target_marks, x_ticks, '</svg>'
  )
}

core_provider_spark_svg <- function(path, entity_id, metric_row) {
  if (!nrow(path)) return("")
  entity_id_value <- entity_id
  z <- data.table::copy(path[entity_id == entity_id_value])
  if (!nrow(z) || !all(c("target_month", "predicted_value", "actual_value") %in% names(z))) {
    return("")
  }
  data.table::setorder(z, target_month)
  z <- z[is.finite(predicted_value) & is.finite(actual_value)]
  if (nrow(z) < 2L) return("")
  values <- c(z$predicted_value, z$actual_value)
  padding <- max(diff(range(values)) * 0.12,
    if (metric_row$unit[1L] == "proportion") 0.005 else 0.25)
  y_min <- min(values) - padding; y_max <- max(values) + padding
  x <- seq(6, 104, length.out = nrow(z))
  y <- function(value) 40 - (value - y_min) * 34 / max(1e-9, y_max - y_min)
  points <- function(value) paste(
    paste0(sprintf("%.1f", x), ",", sprintf("%.1f", y(value))), collapse = " "
  )
  paste0(
    '<svg class="provider-spark" viewBox="0 0 110 48" role="img" aria-label="Six-month actual and fixed forecast path">',
    '<polyline class="spark-forecast" points="', points(z$predicted_value), '"></polyline>',
    '<polyline class="spark-actual" points="', points(z$actual_value), '"></polyline>',
    '<circle class="spark-forecast-dot" cx="104" cy="', sprintf("%.1f", y(utils::tail(z$predicted_value, 1L))), '" r="2.4"></circle>',
    '<circle class="spark-actual-dot" cx="104" cy="', sprintf("%.1f", y(utils::tail(z$actual_value, 1L))), '" r="2.4"></circle>',
    '</svg><div class="spark-key"><span class="actual-key">Actual</span><span class="forecast-key">Fixed forecast</span></div>'
  )
}

core_assert_latest_month_gaps <- function(watchlist) {
  if (!nrow(watchlist)) return(invisible(TRUE))
  required <- c("trajectory_gap_basis", "latest_error_native", "latest_expected_value")
  if (!all(required %in% names(watchlist)) || any(
    is.na(watchlist$trajectory_gap_basis) |
      watchlist$trajectory_gap_basis != "latest_month_fixed_path"
  )) {
    stop("Provider watchlist predates the latest-month gap method. Rerun provider modelling before rendering.")
  }
  valid <- is.finite(watchlist$latest_error_native)
  if (any(valid & (
    !is.finite(watchlist$latest_expected_value) |
      abs(watchlist$latest_value - watchlist$latest_expected_value -
        watchlist$latest_error_native) > 1e-8
  ))) stop("Provider latest-month gap does not match actual minus fixed forecast.")
  invisible(TRUE)
}

core_signal_rows_html <- function(watchlist, signal_value, metric_row,
                                  provider_path = data.table::data.table(),
                                  top_n = 3L) {
  core_assert_latest_month_gaps(watchlist)
  z <- data.table::copy(watchlist[signal == signal_value])
  if (!nrow(z)) return('<div class="empty-signal">No qualifying signal.</div>')
  z[, signal_strength___ := abs(favourable_gap_native)]
  data.table::setorder(z, -signal_strength___)
  z <- utils::head(z, top_n)
  css <- if (signal_value == "sustained_favourable") "favourable" else "adverse"
  paste(vapply(seq_len(nrow(z)), function(i) paste0(
    '<div class="provider-row"><div><div class="provider-name">',
    performance_html_escape(z$entity_name[i]), '</div><div class="provider-context">Latest ',
    performance_format_value(z$latest_value[i], metric_row$unit[1L], metric_row$digits[1L]),
    ' · ', z$signal_months_n[i],
    ' months compared with a path fixed six months earlier</div><div class="provider-evidence">',
    'Forecast fixed before the six-month window',
    '</div></div><div class="provider-spark-wrap">',
    core_provider_spark_svg(provider_path, z$entity_id[i], metric_row),
    '</div><div class="provider-gap ', css, '"><strong>',
    performance_format_difference(
      abs(z$favourable_gap_native[i]), metric_row$unit[1L],
      metric_row$digits[1L]
    ), '</strong><span>',
    if (z$latest_error_native[i] >= 0) {
      "latest month: above forecast"
    } else {
      "latest month: below forecast"
    },
    '</span></div></div>'
  ), character(1)), collapse = "")
}

core_provider_signal_severity <- function(signal, favourable_gap_native,
                                          materiality_native) {
  threshold <- as.numeric(materiality_native)[1L]
  if (!is.finite(threshold) || threshold <= 0) {
    stop("Provider signal materiality threshold must be positive.")
  }
  ratio <- abs(favourable_gap_native) / threshold
  data.table::fcase(
    signal == "sustained_favourable" & ratio >= 2, "marked-favourable",
    signal == "sustained_favourable", "moderate-favourable",
    signal == "sustained_adverse" & ratio >= 2, "marked-adverse",
    signal == "sustained_adverse", "moderate-adverse",
    default = "neutral"
  )
}

core_provider_severity_label <- function(severity) {
  gsub("-", " ", severity, fixed = TRUE)
}

core_provider_distribution_svg <- function(watchlist, metric_row, target_value,
                                           materiality_native) {
  core_assert_latest_month_gaps(watchlist)
  z <- data.table::copy(watchlist[is.finite(latest_value)])
  if (!nrow(z)) {
    return(paste0(
      '<div class="distribution-empty">A provider distribution is not available ',
      'for this measure in this edition.</div>'
    ))
  }
  data.table::setorder(z, latest_value, entity_id)
  values <- c(z$latest_value, target_value)
  values <- values[is.finite(values)]
  padding <- max(diff(range(values)) * 0.08,
                 if (metric_row$unit[1L] == "proportion") 0.01 else 0.5)
  x_min <- min(values) - padding
  x_max <- max(values) + padding
  if (metric_row$unit[1L] == "proportion") {
    x_min <- max(0, x_min); x_max <- min(1, x_max)
  }
  left <- 38; right <- 732; top <- 28; bottom <- 125
  x_position <- function(value) {
    left + (value - x_min) * (right - left) / max(1e-9, x_max - x_min)
  }
  z[, y___ := 55 + (seq_len(.N) %% 7L) * 8]
  z[, class___ := core_provider_signal_severity(
    signal, favourable_gap_native, materiality_native
  )]
  gap_description <- function(gap) {
    if (!is.finite(gap)) return("latest-month fixed-path comparison unavailable")
    paste0(
      "latest-month gap ",
      performance_format_difference(
        abs(gap), metric_row$unit[1L], metric_row$digits[1L]
      ),
      if (gap >= 0) " better" else " worse",
      " than fixed forecast for the same month"
    )
  }
  dots <- paste(vapply(seq_len(nrow(z)), function(i) paste0(
    '<circle class="distribution-dot ', z$class___[i], '" cx="',
    sprintf("%.1f", x_position(z$latest_value[i])), '" cy="', z$y___[i],
    '" r="4" tabindex="0" role="button" data-provider-id="',
    performance_html_escape(z$entity_id[i]), '" aria-label="',
    performance_html_escape(z$entity_name[i]), ': ',
    performance_format_value(z$latest_value[i], metric_row$unit[1L], metric_row$digits[1L]),
    '. ', core_provider_severity_label(z$class___[i]), '; ',
    gap_description(z$favourable_gap_native[i]),
    '. Show trajectory"><title>',
    performance_html_escape(z$entity_name[i]), ': ',
    performance_format_value(
      z$latest_value[i], metric_row$unit[1L], metric_row$digits[1L]
    ), ' · ', core_provider_severity_label(z$class___[i]), ' · ',
    gap_description(z$favourable_gap_native[i]), '</title></circle>'
  ), character(1)), collapse = "")
  target <- if (is.finite(target_value)) {
    target_x <- x_position(target_value)
    target_anchor <- if (target_x >= right - 110) {
      "end"
    } else if (target_x <= left + 110) {
      "start"
    } else {
      "middle"
    }
    target_label_x <- target_x + if (target_anchor == "end") {
      -5
    } else if (target_anchor == "start") {
      5
    } else {
      0
    }
    paste0(
      '<line class="distribution-target" x1="', sprintf("%.1f", target_x),
      '" x2="', sprintf("%.1f", target_x), '" y1="', top,
      '" y2="', bottom, '"></line><text class="distribution-label" x="',
      sprintf("%.1f", target_label_x), '" y="18" text-anchor="',
      target_anchor, '">benchmark ', performance_format_value(
        target_value, metric_row$unit[1L], metric_row$digits[1L]
      ), '</text>'
    )
  } else ""
  ticks <- seq(x_min, x_max, length.out = 4L)
  tick_html <- paste(vapply(seq_along(ticks), function(i) {
    value <- ticks[i]
    anchor <- if (i == 1L) "start" else if (i == length(ticks)) "end" else "middle"
    paste0(
      '<line class="distribution-tick" x1="', sprintf("%.1f", x_position(value)),
      '" x2="', sprintf("%.1f", x_position(value)), '" y1="', bottom,
      '" y2="', bottom + 5, '"></line><text class="axis-label" x="',
      sprintf("%.1f", x_position(value)), '" y="145" text-anchor="', anchor, '">',
      core_outlook_axis_label(value, metric_row$unit[1L]), '</text>'
    )
  }, character(1)), collapse = "")
  paste0(
    '<svg class="distribution-chart" viewBox="0 0 770 155" role="img" ',
    'aria-label="Distribution of latest provider performance">', target,
    '<line class="distribution-axis" x1="', left, '" x2="', right,
    '" y1="', bottom, '" y2="', bottom, '"></line>', dots, tick_html,
    '</svg><div class="distribution-legend" aria-label="Trajectory-watch severity legend">',
    '<span><i class="marked-adverse"></i>Marked deterioration</span>',
    '<span><i class="moderate-adverse"></i>Moderate deterioration</span>',
    '<span><i class="neutral"></i>No sustained signal</span>',
    '<span><i class="moderate-favourable"></i>Moderate improvement</span>',
    '<span><i class="marked-favourable"></i>Marked improvement</span>',
    '<em>Moderate = 1–&lt;2×; marked = at least 2× the metric-specific materiality threshold.</em></div>'
  )
}

core_provider_explorer_html <- function(provider_path, metric_row) {
  required <- c("entity_id", "entity_name", "target_month", "predicted_value", "actual_value")
  if (!nrow(provider_path) || !all(required %in% names(provider_path))) return("")
  z <- data.table::copy(provider_path[
    is.finite(predicted_value) & is.finite(actual_value)
  ])
  if (!nrow(z)) return("")
  data.table::setorder(z, entity_name, target_month)
  valid_ids <- z[, .N, by = entity_id][N >= 2L, entity_id]
  z <- z[entity_id %in% valid_ids]
  if (!nrow(z)) return("")
  groups <- split(z, z$entity_id)
  options <- vapply(groups, function(g) {
    months <- format(as.Date(g$target_month), "%b %Y")
    paste0(
      '<option value="', performance_html_escape(g$entity_id[1L]),
      '" data-name="', performance_html_escape(g$entity_name[1L]),
      '" data-months="', paste(months, collapse = "|"),
      '" data-forecast="', paste(g$predicted_value, collapse = "|"),
      '" data-actual="', paste(g$actual_value, collapse = "|"), '">',
      performance_html_escape(g$entity_name[1L]), '</option>'
    )
  }, character(1))
  paste0(
    '<section class="provider-explorer" data-provider-explorer data-unit="',
    performance_html_escape(metric_row$unit[1L]), '" data-digits="',
    metric_row$digits[1L], '"><div class="explorer-head"><div><strong>Explore a provider trajectory</strong>',
    '<span>Select a trust, or click a dot above. Hover or focus on a dot to identify it.</span></div>',
    '<label>Trust<select data-provider-select>', paste(options, collapse = ""),
    '</select></label></div><div class="explorer-summary" data-provider-summary></div>',
    '<svg class="explorer-chart" data-provider-chart viewBox="0 0 770 180" role="img" ',
    'aria-label="Selected provider actual and fixed forecast trajectory"></svg></section>'
  )
}

provider_explorer_script <- function() paste0(
  "document.querySelectorAll('[data-provider-explorer]').forEach(function(root){",
  "var select=root.querySelector('[data-provider-select]'),svg=root.querySelector('[data-provider-chart]'),summary=root.querySelector('[data-provider-summary]'),ns='http://www.w3.org/2000/svg';",
  "if(!select||!svg)return;function add(tag,attrs,text){var el=document.createElementNS(ns,tag);Object.keys(attrs||{}).forEach(function(k){el.setAttribute(k,attrs[k]);});if(text!==undefined)el.textContent=text;svg.appendChild(el);return el;}",
  "function fmt(v){var d=Number(root.dataset.digits||1);return root.dataset.unit==='proportion'?(v*100).toFixed(d)+'%':v.toFixed(d);}",
  "function draw(){var o=select.options[select.selectedIndex],f=o.dataset.forecast.split('|').map(Number),a=o.dataset.actual.split('|').map(Number),m=o.dataset.months.split('|'),all=f.concat(a),lo=Math.min.apply(null,all),hi=Math.max.apply(null,all),pad=Math.max((hi-lo)*.15,root.dataset.unit==='proportion'?.005:.25);lo-=pad;hi+=pad;var x=function(i){return 58+i*664/Math.max(1,a.length-1);},y=function(v){return 142-(v-lo)*112/Math.max(1e-9,hi-lo);},pts=function(v){return v.map(function(n,i){return x(i).toFixed(1)+','+y(n).toFixed(1);}).join(' ');};svg.replaceChildren();[0,.5,1].forEach(function(q){var yy=142-q*112,val=lo+q*(hi-lo);add('line',{x1:58,x2:722,y1:yy,y2:yy,class:'explorer-grid'});add('text',{x:50,y:yy+4,'text-anchor':'end',class:'explorer-axis'},fmt(val));});add('polyline',{points:pts(f),class:'explorer-forecast'});add('polyline',{points:pts(a),class:'explorer-actual'});add('circle',{cx:x(f.length-1),cy:y(f[f.length-1]),r:4,class:'explorer-dot-forecast'});add('circle',{cx:x(a.length-1),cy:y(a[a.length-1]),r:4,class:'explorer-dot-actual'});add('text',{x:58,y:164,'text-anchor':'start',class:'explorer-axis'},m[0]);add('text',{x:722,y:164,'text-anchor':'end',class:'explorer-axis'},m[m.length-1]);var gap=a[a.length-1]-f[f.length-1];summary.textContent=o.dataset.name+' — actual '+fmt(a[a.length-1])+', fixed forecast '+fmt(f[f.length-1])+' ('+(gap>=0?'+':'')+(root.dataset.unit==='proportion'?(gap*100).toFixed(Number(root.dataset.digits||1))+'pp':fmt(gap))+', latest month)';document.querySelectorAll('.distribution-dot').forEach(function(d){d.classList.toggle('selected',d.dataset.providerId===o.value);});}",
  "select.addEventListener('change',draw);document.querySelectorAll('.distribution-dot[data-provider-id]').forEach(function(dot){function choose(){var opt=Array.from(select.options).find(function(o){return o.value===dot.dataset.providerId;});if(opt){select.value=opt.value;draw();root.scrollIntoView({behavior:'smooth',block:'nearest'});}}dot.addEventListener('click',choose);dot.addEventListener('keydown',function(e){if(e.key==='Enter'||e.key===' '){e.preventDefault();choose();}});});draw();});"
)

core_normalise_ae_provider_watchlist <- function(
    watchlist, higher_is_better = TRUE, window_months = 6L) {
  assert_columns(watchlist, c(
    "data_through_month", "analysis_trust_id", "analysis_trust_name",
    "latest_actual_performance", "latest_gap_to_trajectory_pp",
    "latest_expected_performance", "trajectory_gap_basis",
    "signal", "signal_evidence"
  ), "A&E provider watchlist")
  if (!nrow(watchlist)) return(data.table::data.table(
    metric_id = character(), entity_id = character(), entity_name = character(),
    data_through_month = data.table::as.IDate(character()),
    signal_months_n = integer(), favourable_gap_native = numeric(),
    latest_error_native = numeric(), latest_expected_value = numeric(),
    trajectory_gap_basis = character(),
    signal = character(), signal_evidence = character(), latest_value = numeric()
  ))
  direction <- if (isTRUE(higher_is_better)) 1 else -1
  signal_months <- if (all(c(
    "genuine_release_vintages_6m", "simulated_vintages_6m"
  ) %in% names(watchlist))) {
    watchlist$genuine_release_vintages_6m + watchlist$simulated_vintages_6m
  } else {
    rep(as.integer(window_months), nrow(watchlist))
  }
  data.table::data.table(
    metric_id = "ae4h_all",
    entity_id = watchlist$analysis_trust_id,
    entity_name = watchlist$analysis_trust_name,
    data_through_month = data.table::as.IDate(watchlist$data_through_month),
    signal_months_n = as.integer(signal_months),
    favourable_gap_native = direction *
      watchlist$latest_gap_to_trajectory_pp / 100,
    latest_error_native = watchlist$latest_gap_to_trajectory_pp / 100,
    latest_expected_value = watchlist$latest_expected_performance,
    trajectory_gap_basis = watchlist$trajectory_gap_basis,
    signal = data.table::fcase(
      watchlist$signal == "sustained_above_trajectory" & direction > 0,
      "sustained_favourable",
      watchlist$signal == "sustained_below_trajectory" & direction > 0,
      "sustained_adverse",
      watchlist$signal == "sustained_above_trajectory" & direction < 0,
      "sustained_adverse",
      watchlist$signal == "sustained_below_trajectory" & direction < 0,
      "sustained_favourable",
      default = "no_sustained_signal"
    ),
    signal_evidence = watchlist$signal_evidence,
    latest_value = watchlist$latest_actual_performance
  )[]
}

build_provider_watch_outturn <- function(
    metric_row, overview_row, watchlist, benchmark_value, benchmark_note,
    template_path, output_path, provider_status_note = "",
    provider_path = data.table::data.table(), provider_materiality_native) {
  assert_columns(metric_row, c(
    "metric_id", "display_name", "unit", "digits", "data_source_label",
    "data_source_url"
  ), "provider-watch metric config")
  assert_columns(overview_row, c("metric_id", "latest_month"),
                 "provider-watch overview row")
  assert_columns(watchlist, c(
    "entity_id", "entity_name", "data_through_month", "signal_months_n",
    "favourable_gap_native", "signal", "signal_evidence", "latest_value"
  ), "provider watchlist")
  if (nrow(metric_row) != 1L || nrow(overview_row) != 1L) {
    stop("Provider-watch outturn requires one metric row and one overview row.")
  }
  assessed <- sum(is.finite(watchlist$favourable_gap_native))
  favourable <- sum(
    watchlist$signal == "sustained_favourable", na.rm = TRUE
  )
  adverse <- sum(watchlist$signal == "sustained_adverse", na.rm = TRUE)
  without <- assessed - favourable - adverse
  data_month <- if (nrow(watchlist)) {
    max(data.table::as.IDate(watchlist$data_through_month), na.rm = TRUE)
  } else {
    data.table::as.IDate(overview_row$latest_month[1L])
  }
  if (!is.finite(as.numeric(data_month))) {
    data_month <- data.table::as.IDate(overview_row$latest_month[1L])
  }
  status_note <- paste(
    "Each provider's six actual months are compared with one forecast path fixed",
    "before that window. Both lines begin at the same observed value.",
    "The displayed gap is the latest actual minus the fixed forecast for that same month.",
    "A low performer can therefore be improving, while a high performer can be deteriorating.",
    if (!is.na(provider_status_note) && nzchar(trimws(provider_status_note))) {
      paste("Status:", provider_status_note)
    } else {
      ""
    }
  )
  balance <- if (favourable > adverse) {
    paste0("Favourable signals outnumber adverse signals, ", favourable,
           " to ", adverse, ".")
  } else if (adverse > favourable) {
    paste0("Adverse signals outnumber favourable signals, ", adverse,
           " to ", favourable, ".")
  } else {
    paste0("Favourable and adverse signals are balanced at ", favourable,
           " each.")
  }
  commentary <- paste0(
    "The latest release has been incorporated into the provider screen. ",
    assessed, " providers can be assessed across six consecutive releases. ",
    balance
  )
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  replacements <- list(
    PAGE_TITLE = paste0(metric_row$display_name[1L], " · outturn provider watch"),
    DISPLAY_NAME = performance_html_escape(metric_row$display_name[1L]),
    DATA_MONTH = performance_format_month(data_month),
    COMMENTARY = performance_html_escape(commentary),
    PROVIDER_DISTRIBUTION = core_provider_distribution_svg(
      watchlist, metric_row, benchmark_value, provider_materiality_native
    ),
    PROVIDER_EXPLORER = core_provider_explorer_html(provider_path, metric_row),
    PROVIDER_EXPLORER_SCRIPT = provider_explorer_script(),
    PROVIDERS_ASSESSED = as.character(assessed),
    PROVIDERS_FAVOURABLE = as.character(favourable),
    PROVIDERS_ADVERSE = as.character(adverse),
    PROVIDERS_WITHOUT = as.character(without),
    PROVIDER_STATUS_NOTE = performance_html_escape(status_note),
    FAVOURABLE_ROWS = core_signal_rows_html(
      watchlist, "sustained_favourable", metric_row, provider_path
    ),
    ADVERSE_ROWS = core_signal_rows_html(
      watchlist, "sustained_adverse", metric_row, provider_path
    ),
    BENCHMARK_NOTE = performance_html_escape(benchmark_note),
    DATA_SOURCE_LABEL = performance_html_escape(
      metric_row$data_source_label[1L]
    ),
    DATA_SOURCE_URL = performance_html_escape(metric_row$data_source_url[1L]),
    GENERATED_AT = format(Sys.time(), "%d %B %Y, %H:%M %Z")
  )
  output <- performance_fill_template(template, replacements)
  unresolved <- regmatches(
    output, gregexpr("\\{\\{[A-Z0-9_]+\\}\\}", output)
  )[[1L]]
  if (length(unresolved) && unresolved[1L] != "") {
    stop(
      "Unresolved provider-watch outturn fields: ",
      paste(unique(unresolved), collapse = ", ")
    )
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, output_path, useBytes = TRUE)
  invisible(output_path)
}

core_outlook_forecast_driver_sentence <- function(
    overview_row, component_forecasts, materiality_native) {
  if (!nrow(component_forecasts)) return("")
  required <- c("model", "forecast_month", "predicted_value")
  if (!all(required %in% names(component_forecasts))) return("")
  target_month <- data.table::as.IDate(overview_row$forecast_month[1L])
  z <- data.table::copy(component_forecasts)
  z[, forecast_month := data.table::as.IDate(forecast_month)]
  z <- z[forecast_month == target_month & is.finite(predicted_value)]
  seasonal <- z[model == "recent_level_seasonal", predicted_value]
  trend_bearing <- z[
    model %in% c("seasonal_drift_damped", "recent_trend_seasonal"),
    predicted_value
  ]
  if (length(seasonal) != 1L || length(trend_bearing) != 2L) return("")
  latest <- overview_row$latest_value[1L]
  point <- overview_row$forecast_value[1L]
  if (!is.finite(latest) || !is.finite(point)) return("")
  orient <- if (isTRUE(overview_row$higher_is_better[1L])) 1 else -1
  seasonal_effect <- orient * (seasonal[1L] - latest)
  trend_adjustment <- orient * (point - seasonal[1L])
  forecast_change <- orient * (point - latest)
  threshold <- max(as.numeric(materiality_native), .Machine$double.eps)
  classify <- function(x) {
    if (x >= threshold) "favourable" else if (x <= -threshold) "adverse" else "neutral"
  }
  direction <- classify(forecast_change)
  seasonal_direction <- classify(seasonal_effect)
  trend_direction <- classify(trend_adjustment)
  if (direction == "neutral") {
    return(paste0(
      " Current-level seasonal and underlying-trend effects are small or ",
      "offsetting, leaving ",
      "the outlook broadly stable."
    ))
  }

  improving <- direction == "favourable"
  direction_word <- if (improving) "improvement" else "deterioration"
  aligned <- if (improving) "favourable" else "adverse"
  opposed <- if (improving) "adverse" else "favourable"
  if (seasonal_direction == aligned && trend_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word,
      " reflects both a ",
      if (improving) "seasonally favourable" else "seasonally weaker",
      " month and an ", if (improving) "improving" else "deteriorating",
      " underlying trend."
    ))
  }
  if (seasonal_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word,
      " is mainly due to the usual month-to-month seasonal movement",
      if (trend_direction == opposed) {
        paste0(", partly offset by ",
          if (improving) "a weakening" else "an improving",
          " underlying trend."
        )
      } else {
        "; the underlying trend is broadly flat."
      }
    ))
  }
  if (trend_direction == aligned) {
    return(paste0(
      " The model's expected ", direction_word,
      " is mainly driven by the underlying trend",
      if (seasonal_direction == opposed) {
        paste0(", despite a ",
          if (improving) "seasonally weaker" else "seasonally favourable",
          " month."
        )
      } else {
        "; the seasonal effect is small."
      }
    ))
  }
  paste0(
    " The model components are mixed, but together imply a small ",
    direction_word, "."
  )
}

core_outlook_commentary <- function(
    overview_row, include_provider = TRUE,
    component_forecasts = data.table::data.table()) {
  movement <- if (overview_row$expected_direction == "improving") {
    "improve"
  } else if (overview_row$expected_direction == "deteriorating") {
    "worsen"
  } else {
    "remain broadly stable"
  }
  movement_preposition <- if (
    overview_row$expected_direction == "broadly_stable"
  ) " at " else " to "
  status <- if (overview_row$trajectory_status == "on_trajectory") {
    "on trajectory"
  } else {
    "off course"
  }
  provider_sentence <- if (include_provider) {
    favourable <- overview_row$providers_favourable[1L]
    adverse <- overview_row$providers_adverse[1L]
    if (adverse > favourable) paste0(
      " Across providers, sustained adverse signals outnumber favourable ones (",
      adverse, " to ", favourable, ")."
    ) else if (favourable > adverse) paste0(
      " Across providers, sustained favourable signals outnumber adverse ones (",
      favourable, " to ", adverse, ")."
    ) else paste0(
      " Across providers, sustained favourable and adverse signals are balanced at ",
      favourable, " each."
    )
  } else ""
  driver_sentence <- core_outlook_forecast_driver_sentence(
    overview_row, component_forecasts, overview_row$flat_threshold_native[1L]
  )
  reversal_sentence <- if (
    "forecast_reversal_flag" %in% names(overview_row) &&
      isTRUE(overview_row$forecast_reversal_flag[1L])
  ) {
    paste0(
      " This reverses a sustained recent movement and has been flagged for ",
      "editorial review."
    )
  } else {
    ""
  }
  paste0(
    "The latest ", overview_row$display_name, " result was ",
    performance_format_value(
      overview_row$latest_value, overview_row$unit, overview_row$digits
    ), ". For the next release, it is expected to ", movement,
    movement_preposition,
    performance_format_value(
      overview_row$forecast_value, overview_row$unit, overview_row$digits
    ), ".", driver_sentence, reversal_sentence,
    " The current projection is ", status, " for ",
    overview_row$target_label, ".", provider_sentence
  )
}

core_community_service_rows_html <- function(service_summary, maximum_rows = 15L) {
  if (!nrow(service_summary)) {
    return('<div class="service-empty">No service-level rows were available.</div>')
  }
  z <- data.table::copy(service_summary[
    geography_type == "England" & complete_submission == TRUE &
      is.finite(total_waiting_list) & total_waiting_list > 0 &
      is.finite(over_18_weeks_count)
  ])
  if (!nrow(z)) {
    return('<div class="service-empty">No complete England service rows were available.</div>')
  }
  data.table::setorder(z, -over_18_weeks_count, -total_waiting_list, service_name)
  z <- utils::head(z, maximum_rows)
  maximum_over_18 <- max(z$over_18_weeks_count)
  paste(vapply(seq_len(nrow(z)), function(i) {
    bar_width <- if (maximum_over_18 > 0) {
      100 * z$over_18_weeks_count[i] / maximum_over_18
    } else {
      0
    }
    paste0(
      '<div class="service-row"><div><div class="service-name">',
      performance_html_escape(z$service_name[i]),
      '</div><div class="service-bar-track"><span style="width:',
      sprintf("%.1f", bar_width), '%"></span></div></div>',
      '<div class="service-number">',
      format(round(z$total_waiting_list[i]), big.mark = ",", scientific = FALSE),
      '</div><div class="service-number over18">',
      format(round(z$over_18_weeks_count[i]), big.mark = ",", scientific = FALSE),
      '</div><div class="service-number">',
      sprintf("%.1f%%", 100 * z$within_18_weeks_proportion[i]),
      '</div></div>'
    )
  }, character(1)), collapse = "")
}

core_community_service_page <- function(service_summary, latest_month) {
  if (!nrow(service_summary)) return(list(
    available = FALSE, services_total = 0L, services_complete = 0L,
    rows_html = '<div class="service-empty">No service-level rows were available.</div>',
    note = "No service-level rows were imported."
  ))
  latest <- data.table::copy(service_summary[
    calendar_month == data.table::as.IDate(latest_month) &
      geography_type == "England"
  ])
  services_total <- latest[is.finite(total_waiting_list), data.table::uniqueN(service_id)]
  services_complete <- latest[
    complete_submission == TRUE, data.table::uniqueN(service_id)
  ]
  list(
    available = services_total > 0L,
    services_total = services_total,
    services_complete = services_complete,
    rows_html = core_community_service_rows_html(latest),
    note = paste0(
      "Ranked by the published number waiting over 18 weeks. Complete over-18 ",
      "bands were available for ", services_complete, " of ", services_total,
      " services with a reported total. Rows with suppressed or incomplete bands ",
      "are excluded from the ranking. The national headline on page 1 remains the ",
      "authoritative aggregate."
    )
  )
}

build_core_metric_outlook <- function(metric_row, overview_row, national_panel,
                                      national_next, national_projection,
                                      watchlist, targets, template_path,
                                      output_path,
                                      provider_status_note = "",
                                      include_provider_page = TRUE,
                                      forecast_method = data.table::data.table(),
                                      forecast_components = data.table::data.table(),
                                      community_service_summary = data.table::data.table(),
                                      edition = "forecast",
                                      publication_status = "pilot",
                                      diagnostic_html = "",
                                      diagnostic_title = "What changed?",
                                      diagnostic_subtitle = "The components behind the latest movement",
                                      diagnostic_commentary = "",
                                      diagnostic_compact_html = "",
                                      provider_path = data.table::data.table(),
                                      provider_materiality_native) {
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  targets <- data.table::copy(targets)
  targets[, target_month := data.table::as.IDate(target_month)]
  provider_assessed <- overview_row$providers_assessed[1L]
  provider_rule <- paste0(
    "Six actual months are compared with one forecast path fixed before that window. ",
    "Both lines begin at the same observed value. A low performer can be improving, ",
    "while a high performer can be deteriorating."
  )
  if (is.na(provider_status_note)) provider_status_note <- ""
  if (!is.na(provider_status_note) && nzchar(trimws(provider_status_note))) {
    provider_rule <- paste(provider_rule, "Status:", provider_status_note)
  }
  target_sources <- unique(targets[, .(source_note, target_source_url)])
  target_sources_html <- paste(vapply(seq_len(nrow(target_sources)), function(i) {
    paste0(
      '<a href="', performance_html_escape(target_sources$target_source_url[i]),
      '">', performance_html_escape(target_sources$source_note[i]), '</a>'
    )
  }, character(1)), collapse = " · ")
  method_text <- if (nrow(forecast_method) == 1L) {
    interval_text <- if (grepl("empirical", forecast_method$interval_method[1L])) {
      "empirical historical error quantiles"
    } else if (grepl(
      "student_t_predictive_small_sample", forecast_method$interval_method[1L]
    )) {
      "a labelled small-sample Student-t predictive calculation"
    } else {
      forecast_method$interval_method[1L]
    }
    paste0(
      "Backtest-weighted ensemble of current-level seasonal movement, damped ",
      "annual drift and a damped recent seasonal trend; at least ",
      forecast_method$minimum_ensemble_components[1L], " components are required. ",
      "The final fit uses ", forecast_method$final_fit_consecutive_months[1L],
      " consecutive months. Accuracy is tested on ",
      forecast_method$backtest_predictions_scored[1L],
      " rolling one-month-ahead forecasts. The 80% range uses ", interval_text,
      " from ", forecast_method$interval_calibration_n[1L], " forecast errors. ",
      "Weights use inverse rolling RMSE and are shrunk toward equal weights. ",
      "The pre-release commentary compares the current-level seasonal component ",
      "with the combined forecast."
    )
  } else {
    paste0(
      "Backtest-weighted ensemble of current-level seasonal movement, damped ",
      "annual drift and a damped recent seasonal trend; ",
      national_next$training_months_n[1L],
      " consecutive months in the final fit. The pre-release commentary compares ",
      "the current-level seasonal component with the combined forecast."
    )
  }
  is_outturn <- identical(edition, "outturn")
  service_month <- if (is_outturn && "actual_month" %in% names(overview_row)) {
    overview_row$actual_month[1L]
  } else {
    overview_row$latest_month[1L]
  }
  service_page <- core_community_service_page(
    community_service_summary, service_month
  )
  actual_value <- if (is_outturn && "actual_value" %in% names(overview_row)) {
    overview_row$actual_value[1L]
  } else {
    NA_real_
  }
  actual_movement <- if (is.finite(actual_value)) {
    performance_actual_movement(overview_row[1L])
  } else {
    list(direction = "not_available", label = "Not available", difference = NA_real_)
  }
  actual_note <- if (is.finite(actual_value)) {
    paste0(
      actual_movement$label, " from the previous result · ",
      performance_outturn_note(overview_row[1L])
    )
  } else {
    "Not yet released"
  }
  commentary <- if (is.finite(actual_value)) {
    movement_clause <- if (actual_movement$direction == "stable") {
      "was broadly unchanged"
    } else {
      tolower(actual_movement$label)
    }
    paste0(
      "The published result was ",
      performance_format_value(
        actual_value, metric_row$unit[1L], metric_row$digits[1L]
      ), ". It ", movement_clause, " from the previous result and was ",
      performance_outturn_note(overview_row[1L]), "."
    )
  } else {
    core_outlook_commentary(
      overview_row[1L], include_provider_page, forecast_components
    )
  }
  if (!is.na(diagnostic_commentary) && nzchar(trimws(diagnostic_commentary))) {
    commentary <- paste(commentary, diagnostic_commentary)
  }
  replacements <- list(
    PAGE_TITLE = paste0(
      metric_row$display_name[1L], if (is_outturn) " outturn" else " outlook"
    ),
    EDITION_LABEL = nhs_outlook_edition_label(edition, publication_status),
    EDITION_SUBTITLE = if (is_outturn) {
      paste0(
        "Outturn for ", performance_format_month(overview_row$forecast_month[1L]),
        " · forecast issued with data through ",
        performance_format_month(overview_row$latest_month[1L])
      )
    } else {
      paste0(
        "Forecast for ", performance_format_month(overview_row$forecast_month[1L]),
        " · data through ", performance_format_month(overview_row$latest_month[1L])
      )
    },
    RECENT_CHART_TITLE = if (is_outturn) {
      "Recent performance and current release"
    } else {
      "Recent performance and next release"
    },
    DISPLAY_NAME = performance_html_escape(metric_row$display_name[1L]),
    TARGET_MONTH = performance_format_month(overview_row$forecast_month[1L]),
    DATA_THROUGH_MONTH = performance_format_month(overview_row$latest_month[1L]),
    FORECAST_VALUE = performance_format_value(
      overview_row$forecast_value[1L], metric_row$unit[1L], metric_row$digits[1L]
    ),
    FORECAST_RANGE = paste0(
      performance_format_value(national_next$lower_80[1L], metric_row$unit[1L], metric_row$digits[1L]),
      "–",
      performance_format_value(national_next$upper_80[1L], metric_row$unit[1L], metric_row$digits[1L])
    ),
    LATEST_VALUE = performance_format_value(
      overview_row$latest_value[1L], metric_row$unit[1L], metric_row$digits[1L]
    ),
    LATEST_MONTH = performance_format_month(overview_row$latest_month[1L]),
    BASELINE_LABEL = if (is_outturn) "At forecast" else "Latest",
    LATEST_COMPARISON = performance_html_escape(if (is_outturn) {
      "Known when the forecast was made"
    } else {
      performance_latest_comparison_text(overview_row[1L])
    }),
    ACTUAL_VALUE = if (is.finite(actual_value)) performance_format_value(
      actual_value, metric_row$unit[1L], metric_row$digits[1L]
    ) else "—",
    ACTUAL_NOTE = performance_html_escape(actual_note),
    TARGET_STATUS = if (overview_row$trajectory_status[1L] == "on_trajectory") {
      "On trajectory"
    } else {
      "Off course"
    },
    TARGET_CLASS = overview_row$trajectory_status[1L],
    TARGET_LABEL = performance_html_escape(overview_row$target_label[1L]),
    TARGET_GAP = performance_html_escape(performance_target_gap_text(overview_row[1L])),
    COMMENTARY = performance_html_escape(commentary),
    WHAT_CHANGED_CONTENT = diagnostic_compact_html,
    RECENT_CHART = core_recent_forecast_svg(
      national_panel, national_next, metric_row, actual_value
    ),
    PLANNING_CHART = core_planning_forecast_svg(
      national_panel, national_projection, targets, metric_row
    ),
    PROVIDER_DISTRIBUTION = core_provider_distribution_svg(
      watchlist, metric_row, overview_row$target_value[1L],
      provider_materiality_native
    ),
    PROVIDER_EXPLORER = core_provider_explorer_html(provider_path, metric_row),
    PROVIDER_EXPLORER_SCRIPT = provider_explorer_script(),
    PROVIDERS_ASSESSED = as.character(provider_assessed),
    PROVIDERS_FAVOURABLE = as.character(overview_row$providers_favourable[1L]),
    PROVIDERS_ADVERSE = as.character(overview_row$providers_adverse[1L]),
    PROVIDERS_WITHOUT = as.character(overview_row$providers_no_signal[1L]),
    PROVIDER_STATUS_NOTE = performance_html_escape(provider_rule),
    PROVIDER_DATA_MONTH = performance_format_month(service_month),
    PROVIDER_PAGE_CLASS = if (include_provider_page) "" else "provider-omitted",
    PROVIDER_AVAILABILITY_NOTE = performance_html_escape(
      if (include_provider_page) {
        "Provider distribution and trajectory watch are shown on page 2."
      } else {
        paste("Provider page withheld.", provider_status_note)
      }
    ),
    FAVOURABLE_ROWS = core_signal_rows_html(
      watchlist, "sustained_favourable", metric_row, provider_path
    ),
    ADVERSE_ROWS = core_signal_rows_html(
      watchlist, "sustained_adverse", metric_row, provider_path
    ),
    BENCHMARK_LABEL = performance_html_escape(metric_row$benchmark_label[1L]),
    DATA_SOURCE_LABEL = performance_html_escape(metric_row$data_source_label[1L]),
    DATA_SOURCE_URL = performance_html_escape(metric_row$data_source_url[1L]),
    TARGET_SOURCES = target_sources_html,
    METHOD_TEXT = performance_html_escape(method_text),
    SERVICE_PAGE_CLASS = if (service_page$available) "" else "service-omitted",
    SERVICE_DATA_MONTH = performance_format_month(service_month),
    SERVICES_TOTAL = as.character(service_page$services_total),
    SERVICES_COMPLETE = as.character(service_page$services_complete),
    SERVICE_ROWS = service_page$rows_html,
    SERVICE_NOTE = performance_html_escape(service_page$note),
    DIAGNOSTIC_PAGE_CLASS = if (nzchar(diagnostic_html)) "" else "diagnostic-omitted",
    DIAGNOSTIC_TITLE = performance_html_escape(diagnostic_title),
    DIAGNOSTIC_SUBTITLE = performance_html_escape(diagnostic_subtitle),
    DIAGNOSTIC_CONTENT = diagnostic_html,
    GENERATED_AT = format(Sys.time(), "%d %B %Y, %H:%M %Z")
  )
  output <- performance_fill_template(template, replacements)
  unresolved <- regmatches(output, gregexpr("\\{\\{[A-Z0-9_]+\\}\\}", output))[[1L]]
  if (length(unresolved) && unresolved[1L] != "") {
    stop("Unresolved core outlook fields: ", paste(unique(unresolved), collapse = ", "))
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(output, output_path, useBytes = TRUE)
  invisible(output_path)
}
