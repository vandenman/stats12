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
ggsave(filename = src, pp123, width = (50*3) / 3, height = (25*3) / 3, units = "cm")
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
plot_hist <- function(fit) {
  r <- residuals(fit)
  ggplot(data.frame(residual = r), aes(residual)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "grey80", colour = "grey40") +
    stat_function(fun = dnorm, args = list(mean = mean(r), sd = sd(r)),
                  colour = "red") +
    labs(x = "Residual", y = "Density", title = "Histogram")
}
plot_qq <- function(fit) {
  ggplot(data.frame(residual = residuals(fit)), aes(sample = residual)) +
    stat_qq(size = 0.7, alpha = 0.6) +
    stat_qq_line(colour = "red") +
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
ggsave(file.path(out_dir, "residual_diagnostics_33x4.pdf"), residual_grid,
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
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)
fig_linear_12 <- ggplot(dat, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)

dat_cluster_curve <- tibble(
  x = seq(0, 28, length.out = 200),
  y = 65 + 20 * (x / 28)^10
)
fig_linear_13 <- ggplot(dat, aes(x = electable, y = trustworthy)) +
  geom_point(shape = 21, fill = scales::alpha("black", .5)) + 
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

## Assumption 2: Normal errors
(
  plot_hist(all_fits$godly) +
  plot_hist(all_fits$happy)
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
