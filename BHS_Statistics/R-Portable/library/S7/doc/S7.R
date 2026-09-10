## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(S7)

## -----------------------------------------------------------------------------
dog <- new_class("dog", properties = list(
  name = class_character,
  age = class_numeric
))
dog

## -----------------------------------------------------------------------------
lola <- dog(name = "Lola", age = 11)
lola

## -----------------------------------------------------------------------------
lola@age <- 12
lola@age

## ---- error = TRUE------------------------------------------------------------
lola@age <- "twelve"

## -----------------------------------------------------------------------------
S7_class(lola)

## -----------------------------------------------------------------------------
class(lola)

## -----------------------------------------------------------------------------
speak <- new_generic("speak", "x")

## -----------------------------------------------------------------------------
method(speak, dog) <- function(x) {
  "Woof"
}

## -----------------------------------------------------------------------------
speak(lola)

## -----------------------------------------------------------------------------
cat <- new_class("cat", properties = list(
  name = class_character,
  age = class_double
))
method(speak, cat) <- function(x) {
  "Meow"
}

fluffy <- cat(name = "Fluffy", age = 5)
speak(fluffy)

## ---- error = TRUE------------------------------------------------------------
speak(1)

## -----------------------------------------------------------------------------
pet <- new_class("pet",
  properties = list(
    name = class_character,
    age = class_numeric
  )
)

## -----------------------------------------------------------------------------
cat <- new_class("cat", parent = pet)
dog <- new_class("dog", parent = pet)

cat
dog

## -----------------------------------------------------------------------------
lola <- dog(name = "Lola", age = 11)
fluffy <- cat(name = "Fluffy", age = 5)

## -----------------------------------------------------------------------------
describe <- new_generic("describe", "x")
method(describe, pet) <- function(x) {
  paste0(x@name, " is ", x@age, " years old")
}
describe(lola)
describe(fluffy)

method(describe, dog) <- function(x) {
  paste0(x@name, " is a ", x@age, " year old dog")
}
describe(lola)
describe(fluffy)

## -----------------------------------------------------------------------------
method(describe, S7_object) <- function(x) {
  "An S7 object"
}

cocktail <- new_class("cocktail",
  properties = list(
    ingredients = class_character
  )
)
martini <- cocktail(ingredients = c("gin", "vermouth"))
describe(martini)

## -----------------------------------------------------------------------------
describe

## -----------------------------------------------------------------------------
method(describe, pet)

