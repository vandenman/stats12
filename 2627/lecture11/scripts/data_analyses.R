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
# # 1. Create plot
# p <- ggplot(dat, aes(x, y)) +
#   geom_image(aes(image = image), size = 0.15)
# 
# # 2. Save original SVG
# original <- file.path(out_dir, "faces.svg")
# smaller  <- file.path(out_dir, "faces_optimized.svg")
# 
# ggsave(original, p)
# 
# # 3. Optimize
# optimize_svg(
#   original,
#   smaller,
#   images = dat$image,
#   format = "jpeg",
#   width = 256,
#   quality = 85
# )

regressioncol <- "firebrick2"


# path <- "~/Downloads/igd_data.csv"
# dat <- readr::read_csv(path)
# 
# dat$GPA <- dat$`Your  current CGPA?`
# dat$time_played <- dat$`The average time you spend playing games?`
# fit <- lm(GPA~time_played, dat)
# plot(fit)

path <- "~/github/education/omi/attribute_means.csv"
dat <- readr::read_csv(path)
head(dat)
colnames(dat)

scales <- list(
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep)
)

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
to_js(dat$`well-groomed`, dat$trustworthy)



pp1 <- ggplot(dat, aes(`well-groomed`, trustworthy)) +
  make_img_layer(dat, "well-groomed", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Well-groomed", y = "Trustworthy") +
  theme_bw(base_size = 24)

pp2 <- ggplot(dat, aes(happy, trustworthy)) +
  make_img_layer(dat, "happy", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Happy", y = "Trustworthy") +
  theme_bw(base_size = 24)

pp3 <- ggplot(dat, aes(memorable, trustworthy)) +
  make_img_layer(dat, "memorable", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Memorable", y = "Trustworthy") +
  theme_bw(base_size = 24)

pp123 <- (pp1 + pp2 + pp3 + patchwork::plot_layout(axes = "collect")) & coord_cartesian(clip = "off")

src <- file.path(out_dir, "scatter_side_by_side2.svg")
width <- 25
# 1 panel should be 50 x 25, so three 
save_figs(pp123, .base = src, width = (50*3) / 3, height = (25*3) / 3, units = "cm")
imgs <- file.path(img_dir, paste0(
  c(wminmax(dat$`well-groomed`), wminmax(dat$dominant), wminmax(dat$memorable)),
  ".jpg"))
optimize_svg(src, width = 256, quality = 85)

# save_figs(pp1, pp2, pp3, width = 25/2, height = 25/2, units = "cm", .base = fs::path(out_dir, "scatter_indiv_%d.svg"))
# to_optimize <- list.files(out_dir, pattern = "scatter_indiv_.*.svg")
# sapply(to_optimize, \(src) optimize_svg(fs::path(out_dir, src), width = 256, quality = 85))

ivs <- colnames(dat)[3:ncol(dat)]
dv  <- colnames(dat)[2]

all_fits <- lapply(ivs, \(iv) {
  f <- as.formula(paste0(dv, "~ `", iv, "`"))
  lm(f, data = dat)
})
names(all_fits) <- ivs
shaps <- sapply(all_fits, \(f) shapiro.test(residuals(f)))
shaps["p.value", ]
which(unlist(shaps["p.value", ]) > 0.05)
which(unlist(shaps["p.value", ]) > 0.01)
which(unlist(shaps["p.value", ]) > 0.001)

kss <- sapply(all_fits, \(f) nortest::lillie.test(residuals(f)))
which(unlist(kss["p.value", ]) > 0.05)
which(unlist(kss["p.value", ]) > 0.01)
which(unlist(kss["p.value", ]) > 0.001)

plot(shaps["statistic", ], kss["statistic", ])

plot_resid <- function(fit) {
  ggplot(data.frame(predictor = model.frame(fit)[[2]],
                    residual = residuals(fit)), aes(predictor, residual)) +
    geom_point(size = 0.7, alpha = 0.6) +
    geom_hline(yintercept = 0, colour = "red") +
    labs(x = all.vars(formula(fit))[2], y = "Residual", title = "Residuals vs. predictor")
}
plot_hist <- function(fit, color = "red") {
  r <- residuals(fit)
  ggplot(data.frame(residual = r), aes(residual)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "grey80", colour = "grey40") +
    stat_function(fun = dnorm, args = list(mean = mean(r), sd = sd(r)),
                  colour = color) +
    labs(x = "Residual", y = "Density", title = "Histogram")
}
plot_qq <- function(fit, color = "red") {
  r <- residuals(fit)
  dparams <- list(mean = mean(r), sd = sd(r))
  ggplot(data.frame(residual = r), aes(sample = residual)) +
    stat_qq_line(colour = color, dparams = dparams) +
    stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
    labs(x = "Theoretical quantile", y = "Residual quantile", title = "Q-Q plot")
}
plot_pp <- function(fit) {
  r <- sort(residuals(fit))
  ggplot(data.frame(theoretical = pnorm(r, mean(r), sd(r)),
                    empirical = ppoints(length(r))),
         aes(theoretical, empirical)) +
    geom_point(size = 0.7, alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, colour = "red") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Normal probability", y = "Empirical probability", title = "P-P plot")
}

# One row per predictor, with the four diagnostics in the requested order.
residual_panels <- unlist(lapply(names(all_fits), \(iv) {
  fit <- all_fits[[iv]]
  list(plot_resid(fit) + labs(title = paste(iv, "- residuals vs. predictor")),
       plot_hist(fit), plot_qq(fit), plot_pp(fit))
}), recursive = FALSE)
residual_grid <- patchwork::wrap_plots(residual_panels, ncol = 4) &
  theme_bw(base_size = 9)
save_figs(residual_grid, .base = file.path(out_dir, "residual_diagnostics_33x4.pdf"),
       width = 12.8, height = 33 * 2.5, units = "in", limitsize = FALSE)

oo <- order(unlist(shaps["statistic", ]))
names(oo) <- colnames(shaps)[oo]
idx <- wminmax(unlist(shaps["statistic", ]))

## Recap

## Assumption 1: linear relation:
fig_linear_1 <- (
  ggplot(dat, aes(x = cute, y = trustworthy)) +
  ggplot(dat, aes(x = electable, y = trustworthy))
) & 
  geom_point(shape = 21, fill = "grey", alpha = .5) & 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) &
  theme_bw(base_size = 24)

dput(dat[, c("electable", "trustworthy")])

fig_linear_2 <- (ggplot(dat, aes(x = electable, y = trustworthy)) +
    geom_point(shape = 21, fill = scales::alpha("black", .5))) +
(dat |> mutate(
  group = factor(ifelse(trustworthy > 65 & electable <= 22, 1, 2))
) |>
  ggplot(mapping = aes(x = electable, y = trustworthy, group = group, color = group, fill = group)) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C", alpha = .5) +
  geom_point(shape = 21) &
  labs(fill = "Imagined\ngroups", color = "Imagined\ngroups", group = "Imagined\ngroups")
) + patchwork::plot_layout(axes = "collect") &
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol, show.legend = FALSE) &
  theme_bw(base_size = 24)

save_figs(fig_linear_1, fig_linear_2, 
          width = 40, .base = fs::path(out_dir, "faces_linear_%d", ext = "svg"))

scales <- list(
  scale_x_continuous(breaks = seq(0, 80, 20),  limits = c(0, 90), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(20, 80, 20), limits = c(20, 90), oob = scales::oob_keep)
)

fig_linear_11 <- ggplot(dat, aes(x = cute, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)
fig_linear_12 <- ggplot(dat, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)

dat_cluster_curve <- tibble(
  x = seq(0, 28, length.out = 200),
  y = 65 + 20 * (x / 28)^10
)
fig_linear_13 <- ggplot(dat, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey") + 
  # geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  geom_line(data = dat_cluster_curve, aes(x, y),
            inherit.aes = FALSE, linewidth = 1, colour = "dodgerblue") +
  scales +
  theme_bw(base_size = 24)

fig_linear_14 <- dat |> mutate(
  group = factor(ifelse(
    electable <= 28 &
      trustworthy >= approx(dat_cluster_curve$x, dat_cluster_curve$y,
                            xout = electable, rule = 2)$y,
    1, 2
  ))
) |>
  ggplot(mapping = aes(x = electable, y = trustworthy, group = group, color = group, fill = group)) +
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C", alpha = .5) +
  theme_bw(base_size = 24) +
  geom_point(shape = 21) +
  labs(fill = "Imagined\ngroups", color = "Imagined\ngroups", group = "Imagined\ngroups") +
  scales +
  geom_line(data = dat_cluster_curve, aes(x, y),
            inherit.aes = FALSE, linewidth = 1, colour = "dodgerblue")

fig_linear_15 <- fig_linear_14 +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol, show.legend = FALSE)

save_figs(fig_linear_11, fig_linear_12, fig_linear_13, fig_linear_14, fig_linear_15,
          width = 40, .base = fs::path(out_dir, "faces_linear2_%d", ext = "svg"))


ggplot(dat, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey70", alpha = 0.7) +
  geom_smooth(
    method = "lm", se = FALSE,
    color = "firebrick2"
  ) +
  geom_smooth(
    method = "loess", se = FALSE,
    color = "dodgerblue3", span = 0.35
  ) +
  theme_bw(base_size = 20)

## Interrim on residuals
dat_temp <- dat |> 
  mutate(resid_happy     = residuals(all_fits$happy),
         resid_cute      = residuals(all_fits$cute),
         resid_electable = residuals(all_fits$electable),
         resid_gender    = residuals(all_fits$gender),
         resid_smart     = residuals(all_fits$smart),
         resid_typical   = residuals(all_fits$typical)
         ) 

ii <- c(
  which.max(dat_temp$resid_happy),
  which.max(-dat_temp$resid_happy),
  which.min(abs(dat_temp$resid_happy)),
  which.max(abs(dat_temp$resid_happy) * dat_temp$resid_happy > 0),
  which.min(abs(dat_temp$resid_happy))
)
which(dat$happy < 15 & dat$happy > 10 & dat$trustworthy > 30 & dat$trustworthy < 40)


i1 <- c(620, 175, 763)
dat_temp$color <- "base"
dat_temp$color[i1] <- as.character(seq_along(i1))
# fills <- c("base" = scales::alpha("black", .5))
thm <- theme_bw(base_size = 24)
fills <- c("base" = "grey")
fills[1 + (seq_along(i1))] <- rainbow(length(i1), s = 0.4)
colls <- fills; colls[1] <- "black"

names(fills)[1 + (seq_along(i1))] <- as.character(seq_along(i1))
sizes <- setNames(c(thm$point$size*.75, rep(1.35 * thm$point$size, 3)), names(fills))

the_scales <- list(scale_color_manual(values = colls), scale_fill_manual(values = fills), scale_size_manual(values = sizes))

p0 <- ggplot(dat_temp, aes(x = happy, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey", size = sizes[1]) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales + 
  theme_bw(base_size = 24) +
  labs(x = "Happy", y = "Trustworthy")

p1 <- ggplot(dat_temp, aes(x = happy, y = trustworthy, color = color, fill = color, size = color)) +
  make_img_layer(dat, "happy", "trustworthy", angle = c(20, 150, 20) * pi / 180, ix12 = i1, outward = c(-1, 1, 1), arrow_length = 15) +
  geom_point(shape = 21) + 
  the_scales +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  labs(x = "Happy", y = "Trustworthy")
p0
p1

datseg <- tibble(x = dat$happy[i1], y = dat_temp$trustworthy[i1], yend = predict(all_fits$happy, data.frame(happy = dat$happy[i1])),
                 color = as.character(1:3))
p2 <- p1 + geom_segment(data = datseg, mapping = aes(x = x, y = y, yend = yend, color = color), linewidth = 1.5) +
  labs(x = "Happy", y = "Trustworthy")

p3 <- ggplot(dat_temp, aes(x = happy, y = resid_happy, color = color, fill = color, size = color)) +
  make_img_layer(dat_temp, "happy", "resid_happy", angle = c(20, 150, 20) * pi / 180, ix12 = i1, outward = c(-1, 1, 1), arrow_length = 15) +
  geom_point(shape = 21) + 
  the_scales +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  labs(x = "Happy", y = "Residual")

save_figs(p0, p1, p2, p3, width = 40, .base = fs::path(out_dir, "resid_%d.svg"))
save_figs(
  p2 + ggtitle("Distance from the fitted line") + p3 + ggtitle("Residual plot"), 
  width = 40, .base = fs::path(out_dir, "resid_34.svg")
)
srcs <- c(sprintf(fs::path(out_dir, "resid_%d.svg"), 1:4), fs::path(out_dir, "resid_34.svg"))
sapply(srcs, optimize_svg)

pr_cute <- ggplot(dat_temp, aes(x = cute, y = resid_cute)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scale_y_continuous(limits = c(-42, 22), breaks = seq(-40, 20, 20)) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, 20)) +
  labs(x = "Cute", y = "Residual")
pr_electable <- ggplot(dat_temp, aes(x = electable, y = resid_electable)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scale_y_continuous(limits = c(-42, 22), breaks = seq(-40, 20, 20)) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, 20)) +
  labs(x = "Electable", y = "Residual")

pr_cute_electable <- (pr_cute + pr_electable) + plot_layout(axes = "collect")
save_figs(pr_cute_electable, .base = fs::path(out_dir, "resid_cute_electable.svg"))

pr_cute <- ggplot(dat_temp, aes(x = cute, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scales +
  labs(x = "Cute", y = "Trustworthy", title = "Raw data")
pr_cute_resid <- ggplot(dat_temp, aes(x = cute, y = resid_cute)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scales[[1]] +
  labs(x = "Cute", y = "Residual", title = "Residual plot")
pr_cute_cute_resid <- (pr_cute + pr_cute_resid) + plot_layout(axes = "collect")

pr_electable <- ggplot(dat_temp, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scales +
  labs(x = "Electable", y = "Trustworthy", title = "Raw data")
pr_electable_resid <- ggplot(dat_temp, aes(x = electable, y = resid_electable)) +
  geom_point(shape = 21, fill = "grey") + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scales[[1]] +
  labs(x = "Electable", y = "Residual", title = "Residual plot")
pr_electable_electable_resid <- (pr_electable + pr_electable_resid) + plot_layout(axes = "collect")

save_figs(pr_cute_cute_resid, width = 40, .base = fs::path(out_dir, "cute_and_resid.svg"))
save_figs(pr_electable_electable_resid, width = 40, .base = fs::path(out_dir, "electable_and_resid.svg"))

pr_electable <- ggplot(dat_temp, aes(x = electable, y = resid_electable)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  theme_bw(base_size = 24) + theme(legend.position = "none") +
  scale_y_continuous(limits = c(-42, 22), breaks = seq(-40, 20, 20)) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 80, 20)) +
  labs(x = "Electable", y = "Residual")

## Assumption 2: Normal errors
model_x <- c(15, 45, 75)
model_bells <- do.call(rbind, lapply(model_x, function(x0) {
  mu <- predict(all_fits$happy, newdata = data.frame(happy = x0))
  y <- seq(mu - 2.8 * sigma(all_fits$happy), mu + 2.8 * sigma(all_fits$happy), length.out = 160)
  data.frame(x0, y, x = x0 + 7 * dnorm(y, mu, sigma(all_fits$happy)) /
               dnorm(mu, mu, sigma(all_fits$happy)))
}))
blue <- "dodgerblue"
model_plot <- ggplot(dat, aes(x = happy, y = trustworthy)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5), alpha = .2) +
  geom_abline(intercept = coef(all_fits$happy)[1], slope = coef(all_fits$happy)[2], colour = regressioncol, linewidth = 1) +
  geom_segment(data = data.frame(x0 = model_x, mu = coef(all_fits$happy)[1] + coef(all_fits$happy)[2] * model_x),
               aes(x = x0, xend = x0, y = mu - 2.8 * sigma(all_fits$happy),
                   yend = mu + 2.8 * sigma(all_fits$happy)), inherit.aes = FALSE,
               colour = blue, linetype = "dotted") +
  geom_path(data = model_bells, aes(x, y, group = x0), inherit.aes = FALSE,
            colour = blue, linewidth = 1.3) +
  labs(x = "Happy", y = "Trustworthy") +
  theme_bw(base_size = 24)
model_plot

model_bells2 <- do.call(rbind, lapply(model_x, function(x0) {
  mu <- 0#predict(all_fits$happy, newdata = data.frame(happy = x0))
  y <- seq(mu - 2.8 * sigma(all_fits$happy), mu + 2.8 * sigma(all_fits$happy), length.out = 160)
  data.frame(x0, y, x = x0 + 7 * dnorm(y, mu, sigma(all_fits$happy)) /
               dnorm(mu, mu, sigma(all_fits$happy)))
}))
model_plot_resid <- ggplot(dat_temp, aes(x = happy, y = resid_happy)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5), alpha = .2) +
  geom_abline(intercept = 0, slope = 0, colour = regressioncol, linewidth = 1) +
  geom_segment(data = data.frame(x0 = model_x, mu = 0),
               aes(x = x0, xend = x0, y = 0 - 2.8 * sigma(all_fits$happy),
                   yend = 0 + 2.8 * sigma(all_fits$happy)), inherit.aes = FALSE,
               colour = blue, linetype = "dotted") +
  geom_path(data = model_bells2, aes(x, y, group = x0), inherit.aes = FALSE,
            colour = blue, linewidth = 1.3) +
  labs(x = "Happy", y = "Residual") +
  theme_bw(base_size = 24)
model_plot_resid

model_plot_resid_hist <- plot_hist(all_fits$happy, color = blue) + theme_bw(base_size = 24) + coord_flip() +
  labs(x = NULL, y = NULL, title = NULL)
model_plot_resid_hist_combined <- (model_plot_resid + model_plot_resid_hist) + plot_layout(widths = c(4, 1), axes = "collect")

save_figs(model_plot, model_plot_resid, model_plot_resid_hist_combined,
          width = 40, .base = fs::path(out_dir, "model_plot_normality_%d", ext = "svg"))

model_plot_resid_hist2 <- plot_hist(all_fits$happy, color = blue) + theme_bw(base_size = 24) +
  labs(x = "Residuals", y = "Density", title = "Residuals from Trustworthy = a + b x Happy")
save_figs(model_plot_resid_hist2,
          width = 40, .base = fs::path(out_dir, "model_plot_normality_rotated", ext = "svg"))

model_plot_resid_hist2 <- plot_hist(all_fits$happy, color = blue) + theme_bw(base_size = 24) +
  labs(x = "Residuals", y = "Density", title = "Residuals from\nTrustworthy = a + b x Happy")
model_plot_resid_hist3 <- plot_hist(all_fits$godly, color = blue) + theme_bw(base_size = 24) +
  labs(x = "Residuals", y = "Density", title = "Residuals from\nTrustworthy = a + b x Godly")
save_figs(model_plot_resid_hist2 + model_plot_resid_hist3,
          width = 40, .base = fs::path(out_dir, "model_plot_normality_happy_vs_godly", ext = "svg"))

scales_qq <- list(
  scale_x_continuous(limits = c(-38, 38), breaks = seq(-30, 30, 15)),
  scale_y_continuous(limits = c(-38, 38), breaks = seq(-30, 30, 15))
)
model_plot_resid_qq2 <- plot_qq(all_fits$happy, color = blue) + theme_bw(base_size = 24) + scales_qq +
  labs(title = "Q-Q plot for\nTrustworthy = a + b x Happy")
model_plot_resid_qq3 <- plot_qq(all_fits$godly, color = blue) + theme_bw(base_size = 24) + scales_qq +
  labs(title = "Q-Q plot for\nTrustworthy = a + b x Godly")
save_figs((model_plot_resid_qq2 + model_plot_resid_qq3) + plot_layout(axes = "collect"),
          width = 40, .base = fs::path(out_dir, "model_plot_normality_qq_happy_vs_godly", ext = "svg"))

qnorm(97.5/100, mean(residuals(all_fits$happy)) ,sd(residuals(all_fits$happy)))
qnorm(97.5/100) * sd(residuals(all_fits$happy))
quantile(residuals(all_fits$happy), probs = 97.5/100)

save_figs(
  (
    model_plot_resid_hist2 + ggtitle("Trustworthy = a + b x Happy") +
    model_plot_resid_hist3 + ggtitle("Trustworthy = a + b x Godly")
   ) / (
     model_plot_resid_qq2 + ggtitle(NULL) +
     model_plot_resid_qq3 + ggtitle(NULL)
   ) + plot_layout(axes = "collect"),
          width = 40, .base = fs::path(out_dir, "model_plot_normality_hist_qq_happy_vs_godly", ext = "svg"))

gender_hist <- ggplot(dat, aes(gender)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey45") +
  labs(x = "Perceived gender rating (0-100)", y = "Number of faces", title = "Histogram of the raw data") + theme_bw(base_size = 24)

dparams <- list(mean = mean(dat$gender), sd = sd(dat$gender))
gender_qq <- ggplot(dat, aes(sample = gender)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Q-Q plot of the raw data") +
  theme_bw(base_size = 24)
gender_hist / gender_qq

save_figs(gender_hist / gender_qq,
            width = 20, height = 21, .base = fs::path(out_dir, "gender_raw_normality", ext = "svg"))

trust_gender_title <- sprintf(
  "Trustworthy = %.2f + %.2f x Gender",
  coef(all_fits$gender)[1], coef(all_fits$gender)[2]
)
scatter_gender <- ggplot(dat_temp, aes(x = gender, y = trustworthy)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) +
  geom_smooth(method = "lm", colour = regressioncol, se = FALSE) +
  labs(x = "Gender", y = "Trustworthy", title = trust_gender_title) +
  theme_bw(base_size = 24)

scales <- list(
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), oob = scales::oob_keep)
)
scatter_gender <- ggplot(dat_temp, aes(gender, trustworthy)) +
  make_img_layer(dat, "gender", "trustworthy", angle = c(95, 95) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Gender", y = "Trustworthy", title = trust_gender_title) +
  theme_bw(base_size = 24)
resid_gender <- ggplot(dat_temp, aes(gender, resid_gender)) +
  make_img_layer(dat_temp, "gender", "resid_gender", angle = c(82, 82) * pi / 180, arrow_length = 20) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  labs(x = "Gender", y = "Residuals", title = trust_gender_title) +
  theme_bw(base_size = 24)


gender_resid_hist <- plot_hist(all_fits$gender, color = blue) + theme_bw(base_size = 24) +
  ggtitle("Histogram of the residuals")

dparams <- list(mean = mean(dat_temp$resid_gender), sd = sd(dat_temp$resid_gender))
gender_resid_qq <- ggplot(dat_temp, aes(sample = resid_gender)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Q-Q plot of the residuals") +
  theme_bw(base_size = 24)

scatter_gender
resid_gender
gender_resid_hist + plot_spacer()
gender_resid_hist + gender_resid_qq

src1 <- fs::path(out_dir, "trustworthy_vs_gender", ext = "svg")
src2 <- fs::path(out_dir, "trustworthy_vs_gender_resid", ext = "svg")
save_figs(scatter_gender, .base = src1, width = 40)
optimize_svg(src1, width = 256, quality = 85)
save_figs(resid_gender, .base = src2, width = 40)
optimize_svg(src2, width = 256, quality = 85)
save_figs(
  gender_resid_hist + plot_spacer(),
  gender_resid_hist + gender_resid_qq, .base = fs::path(out_dir, "trustworthy_vs_gender_resid_qq_%d", ext = "svg"), width = 40)

# TODO: panels for histogram and qq plot of residuals!

## Heteroscedastic

i_typical <- findInterval(dat$typical, seq(0, 100, 10))
range(i_typical)
range(dat$typical)
seq(0, 100, 10)[c(4, 10)]
sapply(split(dat$typical, i_typical),range)
sapply(split(residuals(all_fits$typical), i_typical), sd)

i_smart <- findInterval(dat$smart, seq(0, 100, 10))
range(i_smart)
range(dat$typical)
seq(0, 100, 10)[c(3, 10)]

resid_tib <- bind_rows(
  lapply(names(all_fits), \(nm) {
    tibble(x = model.matrix(all_fits[[nm]])[, 2],
           r = residuals(all_fits[[nm]]), nm = nm)
  })
)
resid_tib_sd <- resid_tib |> 
  mutate(i = findInterval(x, seq(0, 100, 10))) |> 
  summarise(sd_r = sd(r), n = length(r), .by = c(nm, i))  

resid_tib_sd <- resid_tib_sd |> 
  mutate(
    # label = sprintf("sd=%.2f\nn=%d", sd_r, n),
    label = sprintf("sd=%.1f", sd_r),
    x_coord = (seq(0, 100, 10)[i] + seq(0, 100, 10)[i+1]) / 2
  )

scales <- list(
  scale_x_continuous(breaks = seq(20, 80, 20), limits = c(20, 90), oob = scales::oob_keep),
  scale_y_continuous(breaks = seq(20, 80, 20), limits = c(20, 90), oob = scales::oob_keep)
)
y_scale <- scale_y_continuous(breaks = seq(-30, 30, 30), limits = c(-35, 30), oob = scales::oob_keep)
trust_smart_title <- sprintf("Trustworthy = %.2f + %.2f x Smart", coef(all_fits$smart)[1], coef(all_fits$smart)[2])
trust_typical_title <- sprintf("Trustworthy = %.2f + %.2f x Typical", coef(all_fits$typical)[1], coef(all_fits$typical)[2])
scatter_smart <- ggplot(dat_temp, aes(smart, trustworthy)) +
  make_img_layer(dat, "smart", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Smart", y = "Trustworthy", title = trust_smart_title) +
  theme_bw(base_size = 24)
scatter_typical <- ggplot(dat_temp, aes(typical, trustworthy)) +
  make_img_layer(dat, "typical", "trustworthy", angle = c(80, 80) * pi / 180) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  labs(x = "Typical", y = "Trustworthy", title = trust_typical_title) +
  theme_bw(base_size = 24)
resid_smart <- ggplot(dat_temp, aes(smart, resid_smart)) +
  make_img_layer(dat_temp, "smart", "resid_smart", angle = c(82, 82) * pi / 180, arrow_length = 20) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  labs(x = "Smart", y = "Residuals", title = trust_gender_title) +
  y_scale +
  theme_bw(base_size = 24)
resid_typical <- ggplot(dat_temp, aes(typical, resid_typical)) +
  make_img_layer(dat_temp, "typical", "resid_typical", angle = c(82, 82) * pi / 180, arrow_length = 20) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  labs(x = "Typical", y = "Residuals", title = trust_gender_title) +
  y_scale +
  theme_bw(base_size = 24)

scatter_smart_typical <- scatter_typical + scatter_smart + plot_layout(axes = "collect")
resid_smart_typical   <- resid_typical + resid_smart + plot_layout(axes = "collect")
save_figs(scatter_smart_typical, resid_smart_typical, width = 40, .base = fs::path(out_dir, "smart_typical_%d", ext = "svg"))
sapply(sprintf(fs::path(out_dir, "smart_typical_%d", ext = "svg"), 1:2), optimize_svg)


typical_resid_hist <- plot_hist(all_fits$typical, color = blue) + theme_bw(base_size = 24) + ggtitle("Typical - Histogram of residuals")
smart_resid_hist <- plot_hist(all_fits$smart, color = blue) + theme_bw(base_size = 24)   + ggtitle("Smart - Histogram of residuals")

dparams <- list(mean = mean(dat_temp$resid_typical), sd = sd(dat_temp$resid_typical))
typical_resid_qq <- ggplot(dat_temp, aes(sample = resid_typical)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Typical - Q-Q plot") +
  theme_bw(base_size = 24)

dparams <- list(mean = mean(dat_temp$resid_smart), sd = sd(dat_temp$resid_smart))
smart_resid_qq <- ggplot(dat_temp, aes(sample = resid_smart)) +
  stat_qq_line(colour = blue, dparams = dparams) +
  stat_qq(size = 0.7, alpha = 0.6, dparams = dparams) +
  labs(x = "Theoretical quantile", y = "Residual quantile", title = "Smart - Q-Q plot") +
  theme_bw(base_size = 24)

combined <- (typical_resid_hist + smart_resid_hist) / (typical_resid_qq + smart_resid_qq)
save_figs(combined, width = 55, .base = fs::path(out_dir, "smart_typical_hist_qq", ext = "svg"))

scales <- list(
  scale_x_continuous(breaks = seq(20, 90, 10), limits = c(20, 90)),
  scale_y_continuous(breaks = seq(-30, 30, 30), limits = c(-36, 45))
)
p11 <- plot_resid(all_fits$typical) + scales + labs(x = "Typical", title = "Residual plot") + theme_bw(base_size = 24) +
  scale_x_continuous(breaks = seq(30, 90, 10), limits = c(30, 90))
p12 <- plot_resid(all_fits$smart)   + scales + labs(x = "Smart",   title = "Residual plot") + theme_bw(base_size = 24)

p13 <- p11 + 
  geom_vline(xintercept = seq(20, 90, 10), colour = blue, linetype = "dashed") +
  geom_text(data = resid_tib_sd |> filter(nm == "typical"), 
            mapping = aes(x = x_coord, y = 40, label = label))
p23 <- p12 + 
  geom_vline(xintercept = seq(20, 90, 10), colour = blue, linetype = "dashed") +
  geom_text(data = resid_tib_sd |> filter(nm == "smart"), 
            mapping = aes(x = x_coord, y = 40, label = label))

p12a <- (p11 + p12) + plot_layout(axes = "collect") 

p12b <- p12a & geom_vline(xintercept = seq(20, 90, 10), colour = blue, linetype = "dashed")
p12c <- (p13 + p23) + plot_layout(axes = "collect") 

# p12a
# p12b
# p12c
# save_figs(p12a, p12b, p12c, width = 75, .base = fs::path(out_dir, "residual_variance_%d", ext = "svg"))

nt <- labs(title = NULL)
p122a <- ((p11 + nt) / (p12 + nt)) + plot_layout(axes = "collect") 

p122b <- p122a & geom_vline(xintercept = seq(20, 90, 10), colour = blue, linetype = "dashed")
p122c <- ((p13 + nt) / (p23 + nt)) + plot_layout(axes = "collect") 

# p122a
# p122b
# p122c
m <- 3/7
save_figs(p122a, p122b, p122c, width = 75 * m, height = 45*m, .base = fs::path(out_dir, "residual_variance2_%d", ext = "svg"))


## Example of non-normal y

hist(dat$gender, breaks = 20)
plot(lm(gender ~ trustworthy, dat))
for (iv in ivs) {
  for (dv in ivs) {
    if (iv != dv) {
      formula <- as.formula(paste0("`", iv, "`", "~", "`", dv, "`"))
      fit <- lm(formula, dat)
      
      test_dv1 <- nortest::lillie.test(dat[[dv]])
      test_dv2 <- shapiro.test(dat[[dv]])
      test_resid1 <- nortest::lillie.test(residuals(fit))
      test_resid2 <- shapiro.test(residuals(fit))
      
      check1 <- test_dv1$p.value < .01 || test_dv2$p.value < .01
      check2 <- test_resid1$p.value >= .1 || test_resid1$p.value >= .1
      if (check1 && check2) {
        cat(paste0("`", iv, "`", "~", "`", dv, "`", "\n"))
      }
      
    }
  }
}

hist(dat$outgoing)
qqnorm(dat$outgoing); qqline(dat$outgoing)
fit_godly_cute <- lm(outgoing ~ happy, dat)
with(dat, plot(outgoing, happy))
plot_resid(fit_godly_cute)
plot_qq(fit_godly_cute)

# TODO:
# could be a good wooclap question!
# show the panels side by side
# is the assumption of normality violated?

hist(dat$godly)
qqnorm(dat$godly); qqline(dat$godly)
fit_godly_cute <- lm(godly ~ cute, dat)
with(dat, plot(godly, cute))
plot_resid(fit_godly_cute)
plot_qq(fit_godly_cute)

(
  plot_hist(all_fits$godly) + ggtitle("Godly") +
  plot_hist(all_fits$happy) + ggtitle("Happy")
) + patchwork::plot_layout(axes = "collect") & theme_bw(base_size = 18)

(
  plot_qq(all_fits$godly) +
  plot_qq(all_fits$happy)
) + patchwork::plot_layout(axes = "collect") & theme_bw(base_size = 18)

## Assumption 3: Homoscedastic errors
(
  plot_hist(all_fits$typical) +
  plot_hist(all_fits$smart)
) + patchwork::plot_layout(axes = "collect") & theme_bw(base_size = 18)

(
  plot_qq(all_fits$typical) +
  plot_qq(all_fits$smart)
) + patchwork::plot_layout(axes = "collect") & theme_bw(base_size = 18)

(
  plot_resid(all_fits$typical) +
  plot_resid(all_fits$smart)
) + patchwork::plot_layout(axes = "collect") & theme_bw(base_size = 18)



ggplot(resid_tib_sd |> filter(nm == "typical"), aes(x = i, y = sd_r, color = nm, group = nm)) +
  geom_line()
ggplot(resid_tib_sd |> filter(nm == "smart"), aes(x = i, y = sd_r, color = nm, group = nm)) +
  geom_line()
ggplot(resid_tib_sd, aes(x = i, y = sd_r, color = nm, group = nm)) +
  geom_line()
ggplot(resid_tib_sd |> filter(nm == "familiar"), aes(x = i, y = sd_r, color = nm, group = nm)) +
  geom_line()

resid_tib_sd_sum <- resid_tib_sd |> 
  summarise(
    var = var(sd_r, na.rm = TRUE), 
    range = diff(range(sd_r, na.rm = TRUE)),
    .by = nm)  

resid_tib_sd_sum[order(resid_tib_sd_sum$var, decreasing = TRUE), ]

i_smart <- findInterval(dat$typical, seq(0, 100, 10))
tibble(r = residuals(all_fits$typical), i = i_typical) |> 
  group_by(i) |> 
  summarise(sd(r))

i_smart <- findInterval(dat$smart, seq(0, 100, 10))
tibble(r = residuals(all_fits$smart), i = i_smart) |> 
  group_by(i) |> 
  summarise(sd(r))


sapply(split(dat$smart, i_smart),range)
split_smart <- split(residuals(all_fits$smart), i_smart)
sapply(, sd)


shaps[, c("godly", "happy")]
  
plot_resid(all_fits$attractive)
plot_hist(all_fits$attractive)
plot_qq(all_fits$attractive)

plot_resid(all_fits$happy)
plot_hist(all_fits$happy)
plot_qq(all_fits$happy)

plot_resid(all_fits$electable)
plot_hist(all_fits$electable)
plot_qq(all_fits$electable)

plot_resid(all_fits$outgoing)
plot_hist(all_fits$outgoing)
plot_qq(all_fits$outgoing)

plot_resid(all_fits$age)
plot_hist(all_fits$age)
plot_qq(all_fits$age)

plot_resid(all_fits$`long-haired`)
plot_hist(all_fits$`long-haired`)
plot_qq(all_fits$`long-haired`)


plot_resid(all_fits$dominant)
plot_qq(all_fits$dominant)


all_fits$godly
dat$stimulus[wminmax(dat$godly)]
dat$stimulus[wminmax(dat$gender)]

plot(kss["p.value", ], shaps["p.value", ]);abline(0,1)
table(kss["p.value", ] < .05, shaps["p.value", ] < .05)

all_fits[[1]]
all_fits[[2]]
ks_test <- function(x) {
  ks.test(x, "pnorm", 0, 1)
}

ks.test(scale(residuals(all_fits[[1]])), "pnorm", 0, 1) # two-sided, exact


plot(residuals(fit1))
qqnorm(residuals(fit2)); qqline(residuals(fit2))
qqnorm(residuals(fit3)); qqline(residuals(fit3))
