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

out_dir <- "lecture11/figures"
img_dir <- "~/github/education/omi/images"
if (!dir.exists(out_dir)) dir.create(out_dir)

save_figs <- function(..., width = 25, height = 25, units = "cm", .base) {
  if (...length() == 1L) {
    ggsave(...elt(1), file = .base,
           width = width, height = height, units = units)
  } else
    for (i in seq_len(...length()))
      ggsave(...elt(i), file = sprintf(.base, i),
             width = width, height = height, units = units)
}

optimize_svg <- function(src, images = NULL,
                         dest = fs::path(paste0(tools::file_path_sans_ext(src), "optimized"), ext = "svg"),
                         format = "jpeg",
                         width = 256,
                         quality = 85) {
  
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

make_img_layer <- function(dat, variable_x, variable_y, size = 0.15,
                           angle = -75 * pi / 180) {
  ix12 <- wminmax(dat[[variable_x]])
  
  extrema_x <- dat[[variable_x]][ix12]
  extrema_y <- dat[[variable_y]][ix12]
  
  rho <- cor(dat[[variable_x]], dat[[variable_y]], use = "pairwise")
  arrow_length <- 30
  # Angles are in radians, ordered by minimum and maximum variable_x.
  if (!is.numeric(angle) || !length(angle) %in% c(1L, length(ix12)) ||
      any(!is.finite(angle))) {
    stop("angle must contain one finite angle or one per image (in radians).")
  }
  angle <- rep_len(angle, length(ix12))
  
  outward <- c(-1, 1)
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

wminmax <- function(x) c(which.min(x), which.max(x))


path <- "~/github/education/omi/attribute_means.csv"
dat <- readr::read_csv(path)

regressioncol <- "firebrick2"

scales <- list(
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep)
)

fig_recap0 <- ggplot(dat, aes(`happy`, trustworthy)) +
  make_img_layer(dat, "happy", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  scales +
  labs(x = "Happy", y = "Trustworthy") +
  theme_bw(base_size = 24)

fig_recap1 <- fig_recap0 + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol)

fit <- lm(trustworthy ~ `happy`, dat)
ab <- coef(fit)
x0 <- 25;y0 <- 15; x1 <- 10; y1 <- ab[1] + ab[2] * x1
intercept_color <- "dodgerblue4"
fig_recap2 <- fig_recap1 + 
  geom_segment(x = x0, y = y0, xend = x1, yend = y1, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = x0, y = y0*0.97, label = expression(widehat(Trustworthy) == a + b %*% `Happy`), hjust = 0, vjust = 1, color = intercept_color)

fig_recap3 <- fig_recap1 + 
  geom_segment(x = x0, y = y0, xend = x1, yend = y1, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = x0, y = y0*0.97, label = expression(widehat(Trustworthy) == 33.67 + 0.45 %*% `Happy`), hjust = 0, vjust = 1, color = intercept_color)

save_figs(fig_recap0, fig_recap1, fig_recap2, fig_recap3,  .base = file.path(out_dir, "reg_recap_%d.svg"), width = 50)
to_optimize <- list.files(out_dir, pattern = "reg_recap_.*.svg")
sapply(to_optimize, \(src) optimize_svg(fs::path(out_dir, src), width = 256, quality = 85))


stat_col <- "darkgreen"
area_col <- "darkorchid4"

f_crit <- .5
f_max <- 4.
tib_line  <- tibble(x = seq(0.01, 4, .01), y = df(x, 1, 1002))
tib_shade <- tib_line |> filter(x >= f_crit)
# tib_shade <- bind_rows(tibble(x=f_crit, y = 0), tib_shade, tibble(x=f_max, y = 0))
f_dist0 <- ggplot(tib_line, aes(x, y)) +
  geom_line() +
  labs(x = "F-statistic", y = "Density") +
  scale_y_continuous(limits = c(-.05, 4.05)) +
  theme_bw(base_size = 30)

stat_col <- "darkgreen"
f_dist1 <- f_dist0 +
  geom_vline(xintercept = f_crit, linetype = "dashed", alpha = .7, color = stat_col) +
  annotate("text", x = f_crit + .1, y = 3.05, label = "Test-statistic", hjust = 0, vjust = 0, color = stat_col)

area_col <- "darkorchid4"
xb <- 2.5
yb <- 2
xe <- 1.5
ye <- df(xe, 1, 50) * 2
f_dist2 <- f_dist1 +
  geom_ribbon(data = tib_shade, aes(x = x, ymax = y, ymin = 0), fill = area_col, color = area_col, alpha = .8) +
  geom_segment(x = xb, y = yb, xend = xe, yend = ye, arrow = arrow(length = unit(0.4,"cm")), color = area_col) +
  annotate("text", x = xb*1.05, y = yb*1.05, label = "Area = p-value", hjust = 0, vjust = 0, color = area_col)

save_figs(f_dist0, f_dist1, f_dist2, .base = file.path(out_dir, "f_dist_%d.svg"), width = 40)








fig_recap0 <- ggplot(dat, aes(`well-groomed`, trustworthy)) +
  make_img_layer(dat, "well-groomed", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  scales +
  labs(x = "Well-groomed", y = "Trustworthy") +
  theme_bw(base_size = 24)

fig_recap1 <- fig_recap0 + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol)

fit <- lm(trustworthy ~ `well-groomed`, dat)
ab <- coef(fit)
x0 <- 20;y0 <- 20; x1 <- 22.5; y1 <- ab[1] + ab[2] * x1
intercept_color <- "dodgerblue4"
fig_recap2 <- fig_recap1 + 
  geom_segment(x = x0, y = y0, xend = x1, yend = y1, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = x0, y = y0*0.97, label = expression(widehat(Trustworthy) == a + b %*% `Well-groomed`), hjust = 0.6, vjust = 1, color = intercept_color)
  


y_mean <- geom_hline(yintercept = mean(Grade), linewidth = 2, colour = regressioncol)

base <- ggplot(dat, aes(x = Hours, y = Trus)) + scales

points <- geom_point()

regressioncol <- "firebrick2"
y_mean <- geom_hline(yintercept = mean(Grade), linewidth = 2, colour = regressioncol)

errorcol <- scales::alpha("dodgerblue2", .85)
y_mean_errors <- geom_segment(x=Hours, xend = Hours, y = mean(Grade), yend = Grade, col=errorcol)

cijfer.uren <- lm(Grade ~ Hours, studentdata)
y_linreg <- geom_abline(intercept = cijfer.uren$coefficients[1], slope = cijfer.uren$coefficients[2], colour = regressioncol, size = 2)

yoffset <- 0.15

reg_recap <- base + y_linreg + points +
  
  geom_segment(x = 1, y = 6.5, xend = 2, yend = 4.5, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = 1, y = 6.75, label = expression(widehat(Grade) == a + b %*% Hours), hjust = 0.6, vjust = 0, color = intercept_color_txt) +
  
  geom_segment(x = 1, y = 1, xend = -.45, yend = 2.5, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = 1, y = 1, label = "Intercept = a = 2.9", hjust = 0, vjust = 1, color = intercept_color_txt) +
  
  geom_segment(x = 8.76, y = 7.6-yoffset, xend = 8.76, yend = 8.5-yoffset, arrow = arrow(length = unit(0.4,"cm")), color = slope_color) +
  # Horizontal bracket segment (Delta X)
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_start), color = slope_color, linetype = "dashed", linewidth = 0.8, color = slope_color) +
  
  # Vertical bracket segment (Delta Y)
  geom_segment(aes(x = x_end, y = y_start, xend = x_end, yend = y_end), color = slope_color, linetype = "dashed", linewidth = 0.8, color = slope_color) +
  
  # Labels using plotmath for mathematical Greek letters (Delta Δ)
  annotate("text", x = (x_start + x_end) / 2, y = y_start - 0.5, label = "Delta*Hours == 1", parse = TRUE, color = slope_color_txt) +
  annotate("text", x = x_end + 0.15, y = (y_start + y_end) / 2 - yoffset, label = expression(Delta * widehat(Grade) == b* " = " * 0.8),
           parse = TRUE, color = slope_color_txt, hjust = 0) +
  
  # geom_text(x = 1, y = 1,   label = "Intercept", hjust = 0, vjust = 1) +
  annotate("text", x = 8.7, y = 7.5 - yoffset, label = "Slope",     hjust = 0.5, vjust = 1, color = slope_color_txt)


reg_recap

reg_recap1 <- base + y_linreg + points +
  geom_segment(x = .5, y = 7.5, xend = 2, yend = 4.5, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = 1.25, y = 7.6, label = expression(widehat(Grade) == a + b ~ '\u00D7' ~ Hours), hjust = 0.6, vjust = 0, color = intercept_color_txt)# +

reg_recap2 <- reg_recap1 +
  geom_segment(x = 1, y = 1, xend = 0.05, yend = 2.8, arrow = arrow(length = unit(0.4,"cm")), color = intercept_color) +
  annotate("text", x = 1, y = 1, label = "Intercept = a = 2.9", hjust = 0, vjust = 1, color = intercept_color_txt)

slope_x <- 9.45
yoffset <- 0.35

reg_recap3 <- reg_recap2 +
  # Horizontal bracket segment (Delta X)
  annotate("segment", x = x_start, y = y_start, xend = x_end, yend = y_start, linetype = "dashed", linewidth = 0.8, color = slope_color) +
  
  # Vertical bracket segment (Delta Y)
  annotate("segment", x = x_end, y = y_start, xend = x_end, yend = y_end, linetype = "dashed", linewidth = 0.8, color = slope_color) +
  
  annotate("text", x = (x_start + x_end) / 2, y = y_start - 0.5, label = "Delta~~Hours == 1", parse = TRUE, color = slope_color_txt) +
  annotate("text", x = x_end + 0.15, y = (y_start + y_end) / 2 - yoffset, label = expression(Delta ~~ widehat(Grade) == b * " = " * 0.8),
           parse = TRUE, color = slope_color_txt, hjust = 0) +
  
  geom_segment(x = slope_x, y = 7.3-yoffset, xend = slope_x, yend = 8.25 - yoffset, arrow = arrow(length = unit(0.4,"cm")), color = slope_color) +
  annotate("text", x = slope_x, y = 7.2 - yoffset, label = "Slope",     hjust = 0.5, vjust = 1, color = slope_color_txt)

reg_recap0 <- base + points
reg_recap0a <- base + y_linreg + points

save_figs(reg_recap0, reg_recap0a, reg_recap1, reg_recap2, reg_recap3,  .base = file.path(out_dir, "reg_recap_%d.svg"),
          width = 50)
