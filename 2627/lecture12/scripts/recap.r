rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggimage)
library(xml2)
library(magick)
library(base64enc)
library(fs)
library(patchwork)
fragment_kable <- function(obj, steps, digits = 2, ...) {
  stopifnot(length(steps) == ncol(obj))
  
  html <- knitr::kable(
    obj, format = "html", digits = digits,
    row.names = FALSE, ...
  )
  doc <- xml2::read_html(as.character(html))
  table <- xml2::xml_find_first(doc, "//table")
  
  for (j in which(!is.na(steps))) {
    cells <- xml2::xml_find_all(
      table,
      sprintf("./thead/tr/th[%d] | ./tbody/tr/td[%d]", j, j)
    )
    xml2::xml_set_attr(cells, "class", "fragment")
    xml2::xml_set_attr(
      cells, "data-fragment-index", as.character(steps[j])
    )
  }
  
  knitr::asis_output(as.character(table))
}

out_dir <- "lecture12/figures"
img_dir <- "~/github/education/omi/images"
if (!dir.exists(out_dir)) dir.create(out_dir)

save_figs <- function(..., width = 25, height = 25, units = "cm", limitsize = TRUE, .base) {
  dont_save <- getOption("dont_save", FALSE)
  if (dont_save) return()
  
  if (...length() == 1L) {
    ggsave(...elt(1), file = .base,
           width = width, height = height, units = units, limitsize = limitsize)
  } else
    for (i in seq_len(...length()))
      ggsave(...elt(i), file = sprintf(.base, i),
             width = width, height = height, units = units, limitsize = limitsize)
}

optimize_svg <- function(src, images = NULL,
                         dest = fs::path(paste0(tools::file_path_sans_ext(src), "optimized"), ext = "svg"),
                         format = "jpeg",
                         width = 256,
                         quality = 85) {
  
  dont_save <- getOption("dont_save", FALSE)
  if (dont_save) return()
  
  doc <- read_xml(src)
  nodes <- xml_find_all(doc, ".//*[local-name()='image']")
  if (!is.null(images)) stopifnot(length(nodes) == length(images))
  
  mime <- switch(format,
                 jpeg = "image/jpeg",
                 webp = "image/webp",
                 avif = "image/avif"
  )
  
  xlink <- c(xlink = "http://www.w3.org/1999/xlink")
  
  for (i in seq_along(nodes)) {
    image_src <- if (is.null(images)) {
      href <- xml_attr(nodes[[i]], "href")
      if (is.na(href)) href <- xml_attr(nodes[[i]], "xlink:href", ns = xlink)
      if (startsWith(href, "data:")) {
        base64decode(sub("^[^,]*,", "", href))
      } else {
        href
      }
    } else {
      images[i]
    }
    img <- image_read(image_src) |>
      image_resize(paste0(width, "x", width, ">"))
    
    raw <- image_write(
      img, format = format, quality = quality
    )
    
    # Remove original embedded image
    xml_set_attr(nodes[[i]], "href", NULL)
    xml_set_attr(nodes[[i]], "xlink:href", NULL, ns = xlink)
    
    xml_set_attr(
      nodes[[i]], "href",
      paste0("data:", mime, ";base64,",
             base64encode(raw))
    )
  }
  
  write_xml(doc, dest)
  cli::cli_inform("saved svg to {.file {dest}}")
}

wminmax <- function(x) c(which.min(x), which.max(x))
make_img_layer <- function(dat, variable_x, variable_y, size = 0.15,
                           angle = -75 * pi / 180, arrow_length = 30,
                           outward = c(-1, 1),
                           ix12 = wminmax(dat[[variable_x]])) {
  # ix12 <- wminmax(dat[[variable_x]])
  
  extrema_x <- dat[[variable_x]][ix12]
  extrema_y <- dat[[variable_y]][ix12]
  
  rho <- cor(dat[[variable_x]], dat[[variable_y]], use = "pairwise")
  
  # Angles are in radians, ordered by minimum and maximum variable_x.
  if (!is.numeric(angle) || !length(angle) %in% c(1L, length(ix12)) ||
      any(!is.finite(angle))) {
    stop("angle must contain one finite angle or one per image (in radians).")
  }
  angle <- rep_len(angle, length(ix12))
  
  # outward <- c(-1, 1)
  x_img <- extrema_x + outward * arrow_length * cos(angle)
  y_img <- extrema_y + if (rho > 0) {
    -outward * arrow_length * sin(angle)
  } else {
    outward * arrow_length * sin(angle)
  }
  
  dat_img <- data.frame(
    x = x_img,
    y = y_img,
    image = file.path(img_dir, paste0(ix12, ".jpg"))
  )
  
  segment_dat <- data.frame(
    x = x_img,
    y = y_img,
    xend = dat[[variable_x]][ix12],
    yend = dat[[variable_y]][ix12]
  )
  
  list(
    geom_segment(data = segment_dat, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.4,"cm")), color = "black", inherit.aes = FALSE),
    geom_image(data = dat_img, aes(x = x, y = y, image = image), size = size, inherit.aes = FALSE)
  )
}

to_js <- function(x, y) {
  cat("points: [\n  ")
  n <- length(x)
  for (i in seq_len(n)) {
    cat(sprintf("[%.2f, %.2f]", x[i], y[i]))
    if (i != n)
      cat(", ")
  }
  cat("\n]\n")
}

path <- "~/github/education/omi/attribute_means.csv"
dat <- readr::read_csv(path)

scales <- list(
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep)
)
regressioncol = "firebrick2"
blue <- "dodgerblue"

set_theme(
  theme_bw(base_size = 24) +
    theme(geom = element_geom(pointshape = 21, fill = "grey", accent = regressioncol))
)
theme_get()$geom
GeomPoint$default_aes
GeomSmooth$default_aes

fig_recap0 <- ggplot(dat, aes(gender, trustworthy)) +
  make_img_layer(dat, "gender", "trustworthy", angle = c(95, 95) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  scales +
  labs(x = "Gender", y = "Trustworthy") +
  theme_bw(base_size = 24)

fig_recap1 <- fig_recap0 + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol)

fig_recap0
fig_recap1

fit <- lm(trustworthy ~ gender, dat)
ab <- coef(fit)
x0 <- 20;y0 <- 15; x1 <- 40; y1 <- ab[1] + ab[2] * x1
intercept_color <- blue
fig_recap2 <- fig_recap1 + 
  geom_segment(x = x0, y = y0, xend = x1, yend = y1, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = x0, y = y0*0.97, label = expression(widehat(Trustworthy) == a + b %*% `Gender`), hjust = 0.25, vjust = 1, color = intercept_color)

fig_recap3 <- fig_recap1 + 
  geom_segment(x = x0, y = y0, xend = x1, yend = y1, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = x0, y = y0*0.97, label = expression(widehat(Trustworthy) == 33.67 + 0.45 %*% `Gender`), hjust = 0.25, vjust = 1, color = intercept_color)

dat$resid_gender <- residuals(fit)
resid_gender <- ggplot(dat, aes(gender, resid_gender)) +
  make_img_layer(dat, "gender", "resid_gender", angle = c(82, 82) * pi / 180, arrow_length = 20) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  labs(x = "Gender", y = "Residuals") +
  theme_bw(base_size = 24)

dparams <- list(mean = mean(dat$resid_gender), sd = sd(dat$resid_gender))
gender_resid_qq <- ggplot(dat, aes(sample = resid_gender)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Q-Q plot of the residuals") +
  theme_bw(base_size = 24)

gender_qq <- ggplot(dat, aes(sample = gender)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Q-Q plot of the raw data") +
  coord_cartesian(ylim = c(-10, 110)) +
  theme_bw(base_size = 24)

save_figs(fig_recap0, fig_recap1, fig_recap2, fig_recap3, .base = file.path(out_dir, "reg_recap_%d.svg"), width = 50)
to_optimize <- list.files(out_dir, pattern = "reg_recap_.*.svg")
sapply(to_optimize, \(src) optimize_svg(fs::path(out_dir, src), width = 256, quality = 85))

gender_resid_qq + plot_spacer()
gender_resid_qq + gender_qq + plot_layout(axes = "collect_y")

save_figs(resid_gender, 
          gender_resid_qq + plot_spacer(),
          gender_resid_qq + gender_qq + plot_layout(axes = "collect_y"),
          .base = file.path(out_dir, "reg_recap2_%d.svg"), width = 50)


set.seed(42)
n <- 300

# -------------------------
# 1. Simulate three datasets
# -------------------------

# A: No violation
x1 <- runif(n, 0, 10)
y1 <- 5 + 2 * x1 + rnorm(n, mean = 0, sd = 2)

# B: Heteroscedasticity
x2 <- runif(n, 0, 10)
y2 <- 5 + 2 * x2 + rnorm(n, mean = 0, sd = 0.5 + 0.6 * x2)

# C: Nonlinearity
x3 <- runif(n, 0, 10)
y3 <- 5 + 2 * x3 - 0.35 * (x3 - 5)^2 + rnorm(n, mean = 0, sd = 2)

# -------------------------
# 2. Fit linear models
# -------------------------
mod1 <- lm(y1 ~ x1)
mod2 <- lm(y2 ~ x2)
mod3 <- lm(y3 ~ x3)

# -------------------------
# 3. Build plotting data
# -------------------------
dat1 <- data.frame(x = x1, residual = resid(mod1), panel = "A. No obvious violation")
dat2 <- data.frame(x = x2, residual = resid(mod2), panel = "B. Heteroscedasticity")
dat3 <- data.frame(x = x3, residual = resid(mod3), panel = "C. Nonlinearity")

dat <- bind_rows(dat1, dat2, dat3)

# Common y-limits for easier comparison
ylims <- range(dat$residual)

# -------------------------
# 4. Make separate plots
# -------------------------
sc_x <- scale_x_continuous(breaks = c(0, 5, 10), limits = c(0, 10))
p1 <- ggplot(dat1, aes(x = x, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = regressioncol, linewidth = 1) +
  coord_cartesian(ylim = ylims) +
  labs(x = "Independent variable", y = "Residual", title = "Ideal")

p2 <- ggplot(dat2, aes(x = x, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = regressioncol, linewidth = 1) +
  geom_abline(intercept = 1, slope = 1.4, color = blue,   linetype = "dashed") +
  geom_abline(intercept = -1, slope = -1.4, color = blue, linetype = "dashed") +
  coord_cartesian(ylim = ylims) +
  labs(x = "Independent variable", y = "Residual", title = "Heteroscedastic")
p2

dat_linesp3 <- tibble(x = seq(0, 10, .01), y = 5 + 2 * x - 0.35 * (x - 5)^2 - (coef(mod3)[1] + coef(mod3)[2] * x))
# dat_linesp3$y <- dat_linesp3$y - mean(dat_linesp3$y)
p3 <- ggplot(dat3, aes(x = x, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = regressioncol, linewidth = 1) +
  geom_line(data = dat_linesp3,
            mapping = aes(x = x, y = y), col = blue) +
  coord_cartesian(ylim = ylims) +
  labs(x = "Independent variable", y = "Residual", title = "Nonlinear")
p3
# -------------------------
# 5. Combine into one row
# -------------------------
wc <- (p1 + p2 + p3 + plot_layout(axes = "collect") & sc_x) 
save_figs(wc, width = 50, .base = fs::path(out_dir, "reg_recap3", ext = "svg"))
