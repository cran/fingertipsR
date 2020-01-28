## ----packages-----------------------------------------------------------------
library(fingertipsR)

## ----indicators---------------------------------------------------------------
inds <- indicators_unique()
life_expectancy <- inds[grepl("life expectancy", tolower(inds$IndicatorName)),]

## ----display_indicators, echo=FALSE-------------------------------------------
knitr::kable(life_expectancy, row.names = FALSE)

## ----area type----------------------------------------------------------------
areaTypes <- area_types()

## ----display_area_types, echo=FALSE-------------------------------------------
DT::datatable(areaTypes, filter = "top", rownames = FALSE)

## ----dist, echo=FALSE---------------------------------------------------------
knitr::kable(areaTypes[areaTypes$AreaTypeID == 202, ], 
             row.names = FALSE) 

## ----extract------------------------------------------------------------------
indicators <- c(90362, 90366)
data <- fingertips_data(IndicatorID = indicators,
                        AreaTypeID = 202)

## ----display_fingertips_data_outputs, echo=FALSE------------------------------
pander::pandoc.table(tail(data), 
                     style="rmarkdown",
                     split.tables = 90, 
                     keep.line.breaks = TRUE)

## ----refine variables---------------------------------------------------------
cols <- c("IndicatorID", "AreaCode", "ParentName", "Sex", "Timeperiod", "Value")

area_type_name <- table(data$AreaType) # tally each group in the AreaType field

area_type_name <- area_type_name[area_type_name == max(area_type_name)] # pick the group with the highest frequency
area_type_name <- names(area_type_name) # retrieve the name

data <- data[data$AreaType == area_type_name & 
               data$Timeperiod == "2012 - 14", cols]


## ----plot, fig.width=8, fig.height=5, message=FALSE, warning=FALSE------------
library(ggplot2)
ggplot(data, aes(x = reorder(ParentName, Value, median), y = Value, col = factor(IndicatorID))) + 
        geom_boxplot(data = data[data$IndicatorID == 90366, ]) +
        geom_boxplot(data = data[data$IndicatorID == 90362, ]) +
        facet_wrap(~ Sex) +
        scale_colour_manual(name = "Indicator",
                            breaks = c("90366", "90362"),
                            labels = c("Life expectancy", "Healthy life expectancy"),
                            values = c("#128c4a", "#88c857")) +
        labs(x = "Region",
             y = "Age",
             title = "Life expectancy and healthy life expectancy at birth \nfor Upper Tier Local Authorities within England regions (2012 - 2014)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1))

