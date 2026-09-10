## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
foo <- new_class("foo")
class(foo())

mean.foo <- function(x, ...) {
  "mean of foo"
}

mean(foo())

## -----------------------------------------------------------------------------
rle <- function(x) {
  if (!is.vector(x) && !is.list(x)) {
    stop("'x' must be a vector of an atomic type")
  }
  n <- length(x)
  if (n == 0L) {
    new_rle(integer(), x)
  } else {
    y <- x[-1L] != x[-n]
    i <- c(which(y | is.na(y)), n)
    new_rle(diff(c(0L, i)), x[i])
  }
}
new_rle <- function(lengths, values) {
  structure(
    list(
      lengths = lengths,
      values = values
    ),
    class = "rle"
  )
}

## -----------------------------------------------------------------------------
new_rle <- new_class("rle",
  parent = class_list,
  constructor = function(lengths, values) {
    new_object(list(lengths = lengths, values = values))
  }
)
rle(1:10)

## -----------------------------------------------------------------------------
new_rle <- new_class("rle", properties = list(
  lengths = class_integer,
  values = class_atomic
))

## -----------------------------------------------------------------------------
method(`$`, new_rle) <- prop
rle(1:10)

## -----------------------------------------------------------------------------
class1 <- new_class("class1")
class2 <- new_class("class2")
union1 <- new_union(class1, class2)

foo <- new_generic("foo", "x")
method(foo, union1) <- function(x) ""
foo

