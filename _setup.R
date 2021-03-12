knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  dev = "png",
  dpi = 108,
  fig.width = 6,
  fig.height = 4.5,
  fig.align = 'center',
  width = 120
)
options(htmltools.dir.version = FALSE, htmltools.preserve.raw = FALSE)
cdw <- function(...) countdown::countdown(...)
rfa <- function(...) icons::fontawesome(...)
emj <- function(...) emo::ji(...)
kig <- function(...) knitr::include_graphics(...)
gh <- function() rfa("github")
rp <- function() rfa("r-project")
ch <- function() rfa("check")
ck <- function() rfa("clock")
db <- function() rfa("database")
nf <- function() rfa("info-circle")
lk <- function() rfa("link")
bk <- function() rfa("book")
fl <- function() rfa("file")
pd <- function() rfa("file-pdf")
ar <- function() rfa("arrow-right")
cg <- function() rfa("cogs")
tr <- function() rfa("exclamation-triangle")
cb <- function() rfa("clipboard")