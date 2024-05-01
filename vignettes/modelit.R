## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.align = 'center',
  fig.width = 7, fig.height = 5, out.width = "100%")
library(modelit)
library(tabularise)
library(chart)
library(dplyr)

## -----------------------------------------------------------------------------
library(modelit)
library(tabularise)
library(chart)
library(dplyr)

## -----------------------------------------------------------------------------
data("Loblolly", package = "datasets")
# convert feet to meters
Loblolly$height <- round(Loblolly$height * 0.3048, 2)
# Add labels and units
Loblolly <- data.io::labelise(Loblolly,
  label = list(height = "Heigth", age = "Age", Seed = "Seed"),
  units = list(height = "m", age = "years"))

# First and last rows of the dataset with tabularise
tabularise$headtail(Loblolly)

## -----------------------------------------------------------------------------
chart(Loblolly, height ~ age %col=% Seed) +
  geom_line()

## -----------------------------------------------------------------------------
set.seed(3652)
pine <- dplyr::slice_sample(Loblolly, n = 1, by = Seed)
chart(pine, height ~ age) + 
  geom_point()

## -----------------------------------------------------------------------------
pine_lm <- lm(data = pine, height ~ age + I(age^2))
summary(pine_lm)

## -----------------------------------------------------------------------------
summary(pine_lm) |> 
  tabularise() 

## -----------------------------------------------------------------------------
chart(pine_lm) # Equivalent to : chart$model(pine_lm)

## -----------------------------------------------------------------------------
chart$residuals(pine_lm)

## -----------------------------------------------------------------------------
pine_nls <- nls(data = pine, height ~ SSgompertz(age, a, b1, b2))
summary(pine_nls) |> 
  tabularise()

## -----------------------------------------------------------------------------
chart(pine_nls, lang = "fr")

