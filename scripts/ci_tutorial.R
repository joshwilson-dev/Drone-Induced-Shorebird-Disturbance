## packages
library('readr')
library('tibble')
library('dplyr')
library('ggplot2')

rm(list = ls())
theme_set(theme_bw())
wasp <- read_csv("darlingtonia.csv")
wasp <- mutate(wasp, lvisited = as.logical(visited))

mod <- glm(lvisited ~ leafHeight, data = wasp, family = binomial())
summary(mod)

## some data to predict at: 100 values over the range of leafHeight
ndata <- with(wasp, data_frame(leafHeight = seq(min(leafHeight), max(leafHeight),
                                                length = 100)))

## grad the inverse link function
ilink <- family(mod)$linkinv

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(mod, newdata = ndata, type = 'response'))

## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(mod, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
## show
ndata

plt <- ggplot(ndata, aes(x = leafHeight, y = fit)) +
    geom_line() +
    geom_rug(aes(y = visited, colour = lvisited), data = wasp) +
    scale_colour_discrete(name = 'Visited') +
    labs(x = 'Leaf height (cm.)', y = 'Probability of visitation')

plt + geom_ribbon(data = ndata,
                  aes(ymin = right_lwr, ymax = right_upr),
                  alpha = 0.1)
