rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggimage)
library(xml2)
library(magick)
library(base64enc)

out_dir <- "lecture10/figures"
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

optimize_svg <- function(src, dest, images,
                         format = "jpeg",
                         width = 256,
                         quality = 85) {
  
  doc <- read_xml(src)
  nodes <- xml_find_all(doc, ".//*[local-name()='image']")
  stopifnot(length(nodes) == length(images))
  
  mime <- switch(format,
                 jpeg = "image/jpeg",
                 webp = "image/webp",
                 avif = "image/avif"
  )
  
  xlink <- c(xlink = "http://www.w3.org/1999/xlink")
  
  for (i in seq_along(nodes)) {
    img <- image_read(images[i]) |>
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


path <- "~/Downloads/igd_data.csv"
dat <- readr::read_csv(path)

dat$GPA <- dat$`Your  current CGPA?`
dat$time_played <- dat$`The average time you spend playing games?`
fit <- lm(GPA~time_played, dat)
plot(fit)

path <- "~/github/education/omi/attribute_means.csv"
dat <- readr::read_csv(path)
head(dat)
colnames(dat)

dat2 <- dat |> select(trustworthy:outdoors)
cormat <- cor(as.matrix(dat), use = "pairwise")
cormat[, "trustworthy"]

which.min(dat$trustworthy)
which.max(dat$trustworthy)

which.min(dat$attractive)
which.max(dat$attractive)
wminmax <- function(x) c(which.min(x), which.max(x))
wminmax(dat$dominant)
wminmax(dat$`well-groomed`)
wminmax(dat$`dominant`)
wminmax(dat$cute)
wminmax(dat$outgoing)
wminmax(dat$smart)
wminmax(dat$memorable)

t.test(dat$attractive, dat$g)

keep <- c(
  "trustworthy", 
  "attractive", "dominant", "age",
  "happy", "well-groomed", "smug"
)

head(dat)

lm(trustworthy ~ attractive, dat)
lm(trustworthy ~ dominant, dat)
lm(trustworthy ~ age, dat)
lm(trustworthy ~ happy, dat)
lm(trustworthy ~ `well-groomed`, dat)
lm(trustworthy ~ smug, dat)

scales <- list(
  scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)),
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))
)
ggplot(dat, aes(attractive, trustworthy)) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)
ggplot(dat, aes(`well-groomed`, trustworthy)) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)
ggplot(dat, aes(`well-groomed`, trustworthy)) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)



dat <- data.frame(
  x = 1:3,
  y = c(2, 3, 1),
  image = file.path(img_dir, paste0(1:3, ".jpg"))
)

p <- ggplot(dat, aes(x, y)) +
  geom_image(aes(image = image), size = 0.15)

ggsave(file.path(out_dir, "faces_test_plot.svg"), p)

ggplot(dat, aes(attractive, trustworthy)) +
  geom_point(shape = 21, fill = "grey") +
  geom_smooth(method = "lm", se = FALSE, colour = regressioncol) +
  scales +
  theme_bw(base_size = 24)


library(magick)

dat_small <- dat
dat_small$image <- vapply(dat$image, function(path) {
  img <- image_read(path) |>
    image_resize("256x256>")
  
  dest <- tempfile(fileext = ".jpg")
  image_write(img, dest, format = "jpeg", quality = 60)
  dest
}, character(1))

p_small <- ggplot(dat_small, aes(x, y)) +
  ggimage::geom_image(aes(image = image), size = 0.15)

ggsave(
  file.path(out_dir, "faces_small.svg"),
  p_small,
  device = svglite::svglite
)

library(xml2)
library(fs)

src <- file.path(out_dir, "faces_test_plot.svg")
doc <- read_xml(src)

imgs <- xml_find_all(doc, ".//*[local-name()='image']")
stopifnot(length(imgs) == nrow(dat))

# Paths relative to the SVG file
urls <- vapply(dat$image, function(path) {
  as.character(fs::path_rel(
    normalizePath(path),
    start = normalizePath(out_dir)
  ))
}, character(1))

urls <- c(
  "https://raw.githubusercontent.com/jcpeterson/omi/main/images/305.jpg",
  "https://raw.githubusercontent.com/jcpeterson/omi/main/images/306.jpg",
  "https://raw.githubusercontent.com/jcpeterson/omi/main/images/307.jpg"
)

names(xml_attrs(imgs[[1]]))

for (i in seq_along(imgs)) {
  xml_set_attr(imgs[[i]], "href", urls[i])
}

write_xml(doc, file.path(out_dir, "faces_linked.svg"))



library(xml2)

doc <- read_xml(file.path(out_dir, "faces_test_plot.svg"))
imgs <- xml_find_all(doc, ".//*[local-name()='image']")

stopifnot(length(imgs) == nrow(dat))

xlink <- c(xlink = "http://www.w3.org/1999/xlink")

for (i in seq_along(imgs)) {
  # Remove both versions of href
  xml_set_attr(imgs[[i]], "href", NULL)
  xml_set_attr(imgs[[i]], "xlink:href", NULL, ns = xlink)
  
  # Add the external path
  xml_set_attr(imgs[[i]], "href", urls[i])
}

dest <- file.path(out_dir, "faces_linked.svg")
write_xml(doc, dest)

file.size(dest) / 1024








src <- file.path(out_dir, "faces_test_plot.svg")

for (fmt in c("jpeg", "webp", "avif")) {
  optimize_svg(
    src,
    file.path(out_dir, paste0("faces_", fmt, ".svg")),
    dat$image,
    format = fmt,
    width = 256,
    quality = 85
  )
}

files <- file.path(
  out_dir,
  paste0("faces_", c("jpeg", "webp", "avif"), ".svg")
)

data.frame(
  format = c("JPEG", "WebP", "AVIF"),
  size_KB = round(file.size(files) / 1024, 1)
)
