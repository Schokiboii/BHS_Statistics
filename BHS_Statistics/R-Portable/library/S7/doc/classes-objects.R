## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
range <- new_class("range",
  properties = list(
    start = class_double,
    end = class_double
  ),
  validator = function(self) {
    if (length(self@start) != 1) {
      "@start must be length 1"
    } else if (length(self@end) != 1) {
      "@end must be length 1"
    } else if (self@end < self@start) {
      sprintf(
        "@end (%i) must be greater than or equal to @start (%i)",
        self@end,
        self@start
      )
    }
  }
)

## ---- error = TRUE------------------------------------------------------------
x <- range(1, 2:3)
x <- range(10, 1)

x <- range(1, 10)
x@start <- 20

## ---- error = TRUE------------------------------------------------------------
x <- range(1, 2)
attr(x, "start") <- 3
validate(x)

## -----------------------------------------------------------------------------
shift <- function(x, shift) {
  x@start <- x@start + shift
  x@end <- x@end + shift
  x
}
shift(range(1, 10), 1)

## ---- error = TRUE------------------------------------------------------------
shift(range(1, 10), 10)

## -----------------------------------------------------------------------------
shift <- function(x, shift) {
  props(x) <- list(
    start = x@start + shift,
    end = x@end + shift
  )
  x
}
shift(range(1, 10), 10)

## -----------------------------------------------------------------------------
range <- new_class("range",
  properties = list(
    start = new_property(class_double),
    end = new_property(class_double)
  )
)

## -----------------------------------------------------------------------------
empty <- new_class("empty",
  properties = list(
    x = class_double,
    y = class_character,
    z = class_logical
  ))
empty()

## -----------------------------------------------------------------------------
empty <- new_class("empty",
  properties = list(
    x = new_property(class_numeric, default = 0),
    y = new_property(class_character, default = ""),
    z = new_property(class_logical, default = NA)
  )
)
empty()

## -----------------------------------------------------------------------------
range <- new_class("range",
  properties = list(
    start = class_double,
    end = class_double,
    length = new_property(
      getter = function(self) self@end - self@start,
    )
  )
)

x <- range(start = 1, end = 10)
x

## ---- error = TRUE------------------------------------------------------------
x@length <- 20

## -----------------------------------------------------------------------------
range <- new_class("range",
  properties = list(
    start = class_double,
    end = class_double,
    length = new_property(
      class = class_double,
      getter = function(self) self@end - self@start,
      setter = function(self, value) {
        self@end <- self@start + value
        self
      }
    )
  )
)

x <- range(start = 1, end = 10)
x

x@length <- 5
x

## -----------------------------------------------------------------------------
range@constructor

## -----------------------------------------------------------------------------
range <- new_class("range",
  properties = list(
    start = class_numeric,
    end = class_numeric
  ),
  constructor = function(x) {
    new_object(S7_object(), start = min(x, na.rm = TRUE), end = max(x, na.rm = TRUE))
  }
)

range(c(10, 5, 0, 2, 5, 7))

