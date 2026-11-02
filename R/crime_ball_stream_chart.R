# ==============================================================================
# Crime ball stream: one stream of tiny balls per offense pours down the screen
# and splits into UNSOLVED (filled balls, left bin) and SOLVED (hollow rings,
# right bin). One ball = a fixed number of reported violent crimes. Balls pour
# in year order, so the piles grow from 2020 to 2025, and the percentages above
# each lane update as balls land.
#
# Interactive pieces
#   - Percent unsolved for each offense, and for all offenses, update live
#   - Hover (or tap) a bin to see its exact numbers; the other bins fade
#   - Play / Pause and a year slider to scrub back and forth
#   - Starts pouring when the chart scrolls into view
# Everything is drawn on one <canvas> with a small physics loop (no libraries).
# Each ball steers gently toward its own slot, so every pile ends with the
# right number of balls.
#
# Needs: dplyr, tidyr, htmltools, jsonlite, knitr, plus the site's comma() and
# percent() from utils.R. `data` needs one row per offense and year with
#   group, year, incidents_reported, incidents_solved
# (srs_state on a state page, srs_us_solve on the national page).
#
# USE IN A QMD CHUNK
#   crime_ball_stream_chart(srs_state, place = state_name_text)
# ==============================================================================

stream_offense_order <- c("Aggravated assault", "Robbery", "Rape", "Homicide")
# Same offense colors as the microsite's ball drop and the backlog charts
stream_fills <- c("#005DA6", "#7397B0", "#2DEBC3", "#001E35")
# Rings for solved (hollow) balls; rape uses the darker teal so a thin ring is visible
stream_rings <- c("#005DA6", "#7397B0", "#17B894", "#001E35")

# Round UP to a "nice" value (1, 2 or 5 times a power of ten) so the key reads
# "1 ball = 5,000 crimes" instead of "1 ball = 4,317 crimes"
stream_nice_unit <- function(x) {
  p <- 10^floor(log10(x))
  m <- c(1, 2, 5, 10) * p
  m[which(m >= x)[1]]
}

stream_css <- "
.ballstream { margin: 24px 0; }
.ballstream { background: #FFFFFF; }
.ballstream canvas { background: #FFFFFF; display: block; margin: 0 auto; width: 100%; max-width: 880px; height: auto; aspect-ratio: 880 / 525; }
.ballstream-controls { display: flex; align-items: center; gap: 16px; max-width: 880px; margin: 8px auto; }
.ballstream-controls button {
  min-width: 96px; min-height: 44px; background: #2DEBC3; color: #000; border: 0; border-radius: 999px;
  padding: 0 22px; font: 700 14px 'Libre Franklin', sans-serif; letter-spacing: 0.04em; text-transform: uppercase; cursor: pointer;
}
.ballstream-controls button:focus-visible, .ballstream-controls input:focus-visible { outline: 3px solid #005DA6; outline-offset: 2px; }
.ballstream-controls input[type=range] { flex-grow: 1; height: 44px; accent-color: #005DA6; }
.ballstream-controls label { font-size: 13px; color: #4B5D6E; white-space: nowrap; }
.ballstream-key { display: flex; flex-wrap: wrap; justify-content: center; gap: 18px; margin: 4px 0; font-size: 13px; color: #4B5D6E; }
.ballstream-key .dot { display: inline-block; width: 11px; height: 11px; border-radius: 50%; vertical-align: -1px; margin-right: 6px; box-sizing: border-box; }
.ballstream-key .dot.filled { background: #4B5D6E; }
.ballstream-key .dot.hollow { background: #fff; border: 2px solid #4B5D6E; }
.ballstream-caption { max-width: 880px; margin: 8px auto 0 auto; font-size: 15px; line-height: 1.5; color: #000; }
.ballstream-details { max-width: 880px; margin: 12px auto 0 auto; font-size: 14px; }
.ballstream-details table { border-collapse: collapse; margin-top: 8px; }
.ballstream-details th, .ballstream-details td { border-bottom: 1px solid #D5E0EA; padding: 4px 12px; text-align: right; }
.ballstream-details th:first-child, .ballstream-details td:first-child { text-align: left; }
"

stream_js_template <- r"--((function () {
  var cfg = __CFG__, id = '__ID__';
  var root = document.getElementById(id + '-wrap');
  var cv = document.getElementById(id + '-cv');
  if (!root || !cv) return;
  var ctx = cv.getContext('2d');
  var W = 880, H = 525, sc = 1;
  var N = cfg.off.length, K = cfg.offenses.length;
  var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var FONT = "'Libre Franklin', 'Helvetica Neue', Arial, sans-serif";
  var DUR = 12, GRAV = 0.17, VMAX = 6.4;
  var G = { left: 40, right: 840, spoutY: 66, splitY: 134, binTop: 190, binBot: 412, binW: 92 };
  var laneW = (G.right - G.left) / K;

  // ---- Geometry: one lane per offense, an unsolved bin (left) and a solved bin (right)
  function cx(k) { return G.left + (k + 0.5) * laneW; }
  function binX(b) { var k = b >> 1; return (b & 1) ? cx(k) + 3 : cx(k) - 3 - G.binW; }
  function binOf(i) { return cfg.off[i] * 2 + cfg.sol[i]; }
  var counts = []; for (var b0 = 0; b0 < 2 * K; b0++) counts.push(0);
  for (var i0 = 0; i0 < N; i0++) counts[binOf(i0)]++;
  // Ball radius: the biggest radius at which the fullest bin still fits
  var R = (function () {
    var mx = Math.max.apply(null, counts), hgt = G.binBot - G.binTop - 12;
    for (var r = 5; r >= 1.2; r -= 0.1) {
      var sx = 2.15 * r, cols = Math.max(1, Math.floor((G.binW - 8) / sx));
      if (Math.ceil(mx / cols) * sx <= hgt) return r;
    }
    return 1.2;
  })();
  var SX = 2.15 * R, COLS = Math.max(1, Math.floor((G.binW - 8) / SX));
  // Each row of a pile fills in a random column order, so balls land all along the bin
  var perms = {};
  function colPerm(b, row) {
    var key = b + '-' + row;
    if (!perms[key]) { var a = [], c, j, t; for (c = 0; c < COLS; c++) a.push(c);
      for (c = a.length - 1; c > 0; c--) { j = (Math.random() * (c + 1)) | 0; t = a[c]; a[c] = a[j]; a[j] = t; }
      perms[key] = a; }
    return perms[key];
  }
  function slotXY(b, n) {
    var row = Math.floor(n / COLS), col = colPerm(b, row)[n % COLS];
    return [binX(b) + (G.binW - COLS * SX) / 2 + col * SX + SX / 2, G.binBot - row * SX - SX / 2 - 2];
  }

  // ---- State -------------------------------------------------------------------
  var balls = [], order = [], slot = [], emitted = 0, elapsed = 0, tickYear = 0;
  var landed = [], landedTotal = 0, playing = false, started = false, visible = true, last = 0, acc = 0, hover = -1, mouse = [0, 0];
  for (var i1 = 0; i1 < N; i1++) balls.push({ i: i1, st: 'wait', x: 0, y: 0, vx: 0, vy: 0, tx: 0, ty: 0, t: 0, crossed: false });
  function zeroLanded() { landed = []; for (var b = 0; b < 2 * K; b++) landed.push(0); landedTotal = 0; }
  zeroLanded();

  // Balls pour in year order; within a year the offenses and outcomes are mixed
  (function plan() {
    var idx = []; for (var i = 0; i < N; i++) idx.push(i);
    for (i = idx.length - 1; i > 0; i--) { var j = (Math.random() * (i + 1)) | 0, t = idx[i]; idx[i] = idx[j]; idx[j] = t; }
    idx.sort(function (a, b) { return cfg.yr[a] - cfg.yr[b]; });
    var placed = []; for (var b = 0; b < 2 * K; b++) placed.push(0);
    slot = new Array(N);
    idx.forEach(function (i) { var bb = binOf(i); slot[i] = slotXY(bb, placed[bb]++); });
    order = idx;
  })();

  function setProgress(p, silent) {            // jump to a point in the pour (0 to 1)
    var n = Math.max(0, Math.min(N, Math.round(p * N)));
    zeroLanded();
    for (var k = 0; k < N; k++) {
      var i = order[k], b = balls[i];
      if (k < n) { b.st = 'rest'; b.x = slot[i][0]; b.y = slot[i][1]; landed[binOf(i)]++; landedTotal++; }
      else b.st = 'wait';
    }
    emitted = n; elapsed = p * DUR; tickYear = n > 0 ? cfg.yr[order[n - 1]] : 0;
    if (!silent) draw();
  }
  function emit() {
    var i = order[emitted++], b = balls[i];
    b.st = 'fall'; b.x = cx(cfg.off[i]) + (Math.random() - 0.5) * 12; b.y = G.spoutY + 6;
    b.vx = 0; b.vy = 1.6 + Math.random() * 0.6; b.crossed = false;
    b.tx = slot[i][0]; b.ty = slot[i][1]; tickYear = cfg.yr[i];
  }
  function physics() {
    for (var k = 0; k < N; k++) {
      var b = balls[k];
      if (b.st === 'fall') {
        b.vy = Math.min(VMAX, b.vy + GRAV);
        if (!b.crossed && b.y >= G.splitY) {     // the splitter sends solved right, unsolved left
          b.crossed = true; b.vx += (cfg.sol[b.i] ? 1 : -1) * (0.9 + Math.random() * 0.9);
        }
        if (b.crossed) {
          var prog = Math.min(1, Math.max(0, (b.y - G.splitY) / (G.binTop - G.splitY)));
          var remain = Math.max(10, G.binTop - b.y), tFall = remain / Math.max(1.5, b.vy);
          b.vx += ((b.tx - b.x) / tFall - b.vx) * (0.01 + 0.14 * prog * prog);
        }
        b.x += b.vx; b.y += b.vy;
        if (b.y >= G.binTop) { b.st = 'settle'; b.x0 = b.x; b.y0 = b.y; b.t = 0; landed[binOf(b.i)]++; landedTotal++; }
      } else if (b.st === 'settle') {
        b.t += 0.08;
        if (b.t >= 1) { b.st = 'rest'; b.x = b.tx; b.y = b.ty; }
        else { var e = b.t * b.t * (3 - 2 * b.t); b.x = b.x0 + (b.tx - b.x0) * e; b.y = b.y0 + (b.ty - b.y0) * e; }
      }
    }
  }

  // ---- Numbers shown ------------------------------------------------------------
  function fmt(n) { return Math.round(n).toLocaleString('en-US'); }
  var done = function () { return landedTotal >= N; };
  function pctText(u, s, exact) { return exact || (u + s ? Math.round(100 * u / (u + s)) + '%' : '–'); }
  function laneStats(k) {
    var u = landed[2 * k], s = landed[2 * k + 1];
    return { u: u, s: s, pct: done() ? cfg.pctUnsolved[k] : pctText(u, s) };
  }
  function allStats() {
    var u = 0, s = 0; for (var k = 0; k < K; k++) { u += landed[2 * k]; s += landed[2 * k + 1]; }
    return { u: u, s: s, pct: done() ? cfg.pctUnsolvedAll : pctText(u, s) };
  }

  // ---- Drawing --------------------------------------------------------------------
  function drawBall(b, dim) {
    var o = cfg.off[b.i], sol = cfg.sol[b.i];
    ctx.globalAlpha = dim ? 0.25 : 1; ctx.beginPath(); ctx.arc(b.x, b.y, R, 0, 6.2832);
    ctx.fillStyle = sol ? '#FFFFFF' : cfg.fills[o]; ctx.fill();
    if (sol) { ctx.lineWidth = Math.max(0.9, R * 0.4); ctx.strokeStyle = cfg.rings[o]; ctx.stroke(); }
  }
  function draw() {
    var dpr = window.devicePixelRatio || 1, cssW = cv.clientWidth || W;
    if (cv.width !== Math.round(cssW * dpr)) { cv.width = Math.round(cssW * dpr); cv.height = Math.round(cssW * dpr * H / W); }
    sc = cv.width / W;
    ctx.setTransform(sc, 0, 0, sc, 0, 0); ctx.globalAlpha = 1;
    ctx.fillStyle = '#FFFFFF'; ctx.fillRect(0, 0, W, H);   // solid white background
    var all = allStats(), k, b;
    // header: running overall percent and the year
    ctx.textAlign = 'left'; ctx.fillStyle = '#000'; ctx.font = '800 46px ' + FONT; ctx.fillText(all.pct, 40, 46);
    var pw = ctx.measureText(all.pct).width;
    ctx.fillStyle = '#4B5D6E'; ctx.font = '400 17px ' + FONT;
    ctx.fillText(done() ? 'of violent crimes went unsolved' : 'of violent crimes unsolved so far', 40 + pw + 12, 44);
    ctx.textAlign = 'right'; ctx.fillStyle = '#4B5D6E'; ctx.font = '700 12px ' + FONT; ctx.fillText('YEAR', W - 40, 18);
    ctx.fillStyle = '#000'; ctx.font = '800 30px ' + FONT; ctx.fillText(String(cfg.years[tickYear]), W - 40, 50);
    ctx.textAlign = 'center';
    for (k = 0; k < K; k++) {
      var x = cx(k);
      // bins
      for (var s = 0; s < 2; s++) {
        var bi = 2 * k + s, bx = binX(bi), hov = hover === bi;
        ctx.setLineDash([5, 4]); ctx.lineWidth = hov ? 2 : 1; ctx.strokeStyle = hov ? '#001E35' : '#9FB0C0';
        ctx.beginPath(); ctx.roundRect(bx, G.binTop, G.binW, G.binBot - G.binTop, 8); ctx.stroke(); ctx.setLineDash([]);
        ctx.fillStyle = '#4B5D6E'; ctx.font = '700 11px ' + FONT;
        ctx.fillText(s ? 'SOLVED' : 'UNSOLVED', bx + G.binW / 2, G.binBot + 16);
      }
      var st = laneStats(k);
      ctx.fillStyle = '#000'; ctx.font = '800 14px ' + FONT; ctx.fillText(cfg.offenses[k], x, G.binBot + 40);
      ctx.font = '800 26px ' + FONT; ctx.fillText(st.pct, x, G.binBot + 70);
      ctx.fillStyle = '#4B5D6E'; ctx.font = '400 12px ' + FONT; ctx.fillText('unsolved', x, G.binBot + 86);
    }
    // balls (dim the other bins while one is hovered)
    for (var i = 0; i < N; i++) { b = balls[i]; if (b.st !== 'wait') drawBall(b, hover >= 0 && b.st !== 'fall' && binOf(i) !== hover); }
    ctx.globalAlpha = 1;
    // hover card
    if (hover >= 0) {
      var hk = hover >> 1, hs = hover & 1, n = landed[hover], tot = landed[2 * hk] + landed[2 * hk + 1];
      var crimes = done() ? (hs ? cfg.solved[hk] : cfg.unsolved[hk]) : n * cfg.perBall;
      var line1 = cfg.offenses[hk] + ', ' + (hs ? 'solved' : 'unsolved');
      var line2 = fmt(crimes) + ' crimes' + (done() ? '' : ' so far');
      var line3 = (tot ? Math.round(100 * n / tot) : 0) + '% of ' + cfg.offenses[hk].toLowerCase() + ' crimes';
      if (done()) line3 = (hs ? cfg.pctSolved[hk] : cfg.pctUnsolved[hk]) + ' of ' + cfg.offenses[hk].toLowerCase() + ' crimes';
      ctx.font = '700 13px ' + FONT; var w = Math.max(ctx.measureText(line1).width, ctx.measureText(line3).width, ctx.measureText(line2).width) + 20;
      var tx = Math.min(W - w - 6, Math.max(6, mouse[0] + 14)), ty = Math.min(G.binTop - 8, Math.max(6, mouse[1] - 70));
      ctx.fillStyle = '#FFF'; ctx.strokeStyle = '#D5E0EA'; ctx.lineWidth = 1; ctx.beginPath(); ctx.roundRect(tx, ty, w, 62, 8); ctx.fill(); ctx.stroke();
      ctx.textAlign = 'left'; ctx.fillStyle = '#000'; ctx.fillText(line1, tx + 10, ty + 20);
      ctx.font = '400 13px ' + FONT; ctx.fillText(line2, tx + 10, ty + 38); ctx.fillStyle = '#4B5D6E'; ctx.fillText(line3, tx + 10, ty + 54);
    }
  }

  // ---- Loop and controls ---------------------------------------------------------
  var slider = document.getElementById(id + '-scrub'), playBtn = document.getElementById(id + '-play');
  function syncUI() {
    if (playBtn) playBtn.textContent = playing ? 'Pause' : (emitted >= N ? 'Replay' : 'Play');
    if (slider) { slider.value = Math.round(1000 * Math.min(1, emitted / N)); slider.setAttribute('aria-valuetext', String(cfg.years[tickYear])); }
  }
  function loop(ts) {
    if (visible) {
      if (!last) last = ts;
      acc += Math.min(0.1, (ts - last) / 1000); last = ts;
      while (acc >= 1 / 60) {
        acc -= 1 / 60;
        if (playing && emitted < N) {
          elapsed += 1 / 60;
          var target = Math.min(N, Math.floor(elapsed / DUR * N)), g = 0;
          while (emitted < target && g++ < 40) emit();
          if (emitted >= N) { playing = false; syncUI(); }
        }
        physics();
      }
      syncUI(); draw();
    } else last = 0;
    requestAnimationFrame(loop);
  }
  function play() { if (emitted >= N) setProgress(0, true); playing = true; started = true; syncUI(); }
  if (playBtn) playBtn.addEventListener('click', function () { if (playing) { playing = false; syncUI(); } else play(); });
  if (slider) slider.addEventListener('input', function () { playing = false; started = true; setProgress(Number(slider.value) / 1000); syncUI(); });
  function pointer(e) {
    var r = cv.getBoundingClientRect(), p = e.touches ? e.touches[0] : e;
    mouse = [(p.clientX - r.left) / r.width * W, (p.clientY - r.top) / r.height * H];
    var h = -1;
    for (var b = 0; b < 2 * K; b++) { var bx = binX(b); if (mouse[0] >= bx && mouse[0] <= bx + G.binW && mouse[1] >= G.binTop - 6 && mouse[1] <= G.binBot + 100) h = b; }
    hover = h; if (!visible || reduce) draw();
  }
  cv.addEventListener('mousemove', pointer); cv.addEventListener('pointerdown', pointer);
  cv.addEventListener('mouseleave', function () { hover = -1; });
  if ('IntersectionObserver' in window) {
    new IntersectionObserver(function (es) {
      visible = es[0].isIntersecting;
      if (visible && !started && !reduce) play();       // start pouring when the chart scrolls into view
    }, { threshold: 0.35 }).observe(cv);
  } else if (!reduce) play();
  window.addEventListener('resize', draw);
  if (reduce) setProgress(1, true); else setProgress(0, true);
  syncUI(); requestAnimationFrame(loop);
  root.__stream = { play: play, setProgress: setProgress, state: function () { return { emitted: emitted, landed: landed.slice(), R: R }; } };
})();
)--"

crime_ball_stream_chart <- function(data, place = "the U.S.", balls_target = 1500,
                                    crimes_per_ball = NULL, element_id = "crime-stream") {

  # ---- 1. Counts by offense, year and status --------------------------------
  counts <- data |>
    dplyr::filter(group %in% stream_offense_order) |>
    dplyr::summarize(
      reported = sum(incidents_reported, na.rm = TRUE),
      solved   = sum(incidents_solved, na.rm = TRUE),
      .by = c(group, year)
    ) |>
    dplyr::mutate(
      group    = factor(group, levels = stream_offense_order),
      solved   = pmin(solved, reported),
      Unsolved = reported - solved,
      Solved   = solved
    )
  years <- sort(unique(counts$year))
  year_range <- paste0(min(years), "–", max(years))
  total_reported <- sum(counts$reported)
  if (is.null(crimes_per_ball)) {
    crimes_per_ball <- stream_nice_unit(total_reported / balls_target)
  }

  # ---- 2. One row per ball ----------------------------------------------------
  # Each offense-year-status count becomes round(count / crimes_per_ball) balls,
  # so ball counts can be off by a fraction of a ball per group. Percentages and
  # numbers shown when the pour finishes are the exact figures from the data.
  balls <- counts |>
    tidyr::pivot_longer(c(Unsolved, Solved), names_to = "status", values_to = "n") |>
    dplyr::mutate(n_balls = round(n / crimes_per_ball)) |>
    dplyr::filter(n_balls > 0) |>
    tidyr::uncount(n_balls)
  if (nrow(balls) == 0) stop("No balls to draw for ", place, "; lower crimes_per_ball", call. = FALSE)

  # ---- 3. Exact totals for the labels and text --------------------------------
  by_off <- counts |>
    dplyr::summarize(reported = sum(reported), unsolved = sum(Unsolved),
                     solved = sum(Solved), .by = group) |>
    dplyr::arrange(group) |>
    dplyr::mutate(pct_unsolved = unsolved / reported, pct_solved = solved / reported)
  total_unsolved <- sum(by_off$unsolved)
  and_list <- function(x) knitr::combine_words(x)

  caption <- paste0(
    "Each ball is about ", comma(crimes_per_ball, 1), " violent crimes reported in ", place,
    " from ", year_range, ". Filled balls are crimes that went unsolved; hollow rings were solved. ",
    percent(total_unsolved / total_reported, 1), " of all violent crimes went unsolved: ",
    and_list(paste0(tolower(by_off$group), " ", percent(by_off$pct_unsolved, 1))), "."
  )

  cfg <- list(
    off = I(as.integer(balls$group) - 1L),
    sol = I(as.integer(balls$status == "Solved")),
    yr = I(match(balls$year, years) - 1L),
    years = I(years), offenses = I(stream_offense_order),
    fills = I(stream_fills), rings = I(stream_rings),
    reported = I(by_off$reported), unsolved = I(by_off$unsolved), solved = I(by_off$solved),
    perBall = crimes_per_ball,
    pctUnsolved = I(percent(by_off$pct_unsolved, 1)),
    pctSolved = I(percent(by_off$pct_solved, 1)),
    pctUnsolvedAll = percent(total_unsolved / total_reported, 1)
  )
  cfg_json <- jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = 2, null = "null")
  script_text <- gsub("__ID__", element_id,
                      gsub("__CFG__", cfg_json, stream_js_template, fixed = TRUE), fixed = TRUE)

  # ---- 4. Page pieces -----------------------------------------------------------
  tags <- htmltools::tags
  canvas <- tags$canvas(
    id = paste0(element_id, "-cv"), role = "img", `aria-label` = caption,
    "Animated chart of violent crimes reported, split into solved and unsolved for each offense. ",
    "The same numbers appear in the data table below."
  )
  controls <- tags$div(
    class = "ballstream-controls",
    tags$button(id = paste0(element_id, "-play"), type = "button", "Play"),
    tags$label(`for` = paste0(element_id, "-scrub"), paste0("Year, ", min(years), " to ", max(years))),
    tags$input(id = paste0(element_id, "-scrub"), type = "range", min = 0, max = 1000, value = 0,
               `aria-label` = paste0("Year shown in the animation, ", min(years), " to ", max(years)))
  )
  key <- tags$div(
    class = "ballstream-key", `aria-hidden` = "true",
    tags$span(tags$span(class = "dot filled"), "Unsolved"),
    tags$span(tags$span(class = "dot hollow"), "Solved"),
    tags$span(paste0("1 ball = about ", comma(crimes_per_ball, 1), " crimes"))
  )
  table_df <- by_off |>
    dplyr::transmute(
      Offense = as.character(group), Reported = comma(reported, 1),
      Unsolved = comma(unsolved, 1), Solved = comma(solved, 1),
      `Share unsolved` = percent(pct_unsolved, 1)
    )
  details <- tags$details(
    class = "ballstream-details",
    tags$summary("Data table: violent crimes reported, solved and unsolved, ", year_range),
    htmltools::HTML(knitr::kable(table_df, format = "html", align = "lrrrr", escape = TRUE))
  )

  htmltools::browsable(tags$div(
    id = paste0(element_id, "-wrap"), class = "ballstream",
    tags$style(htmltools::HTML(stream_css)),
    canvas, controls, key, tags$p(class = "ballstream-caption", caption), details,
    tags$script(htmltools::HTML(script_text))
  ))
}
