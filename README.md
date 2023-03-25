# Prediction of wellbeing outcomes based on indicators for basic human needs satisfaction

### Master's thesis

Lea Tamberg, ETH Zurich, Master's programme in Data Science

Supervisors:

* Professor Nicolai Meinshausen, ETH Zurich
* Professor Julia Steinberger, University of Lausanne
* Professor Jason Hickel, Autonomous University of Barcelona

## Summary of the thesis

This thesis explores the relationship between basic human needs and wellbeing outcomes and examines whether this relationship explains the empirical link between GDP per capita and wellbeing. To this end, it analyses an international cross-sectional and panel data set based on a mapping of different basic human needs to indicators reflecting their satisfaction. The data is used to test whether there is still a significant statistical effect of GDP per capita on life expectancy and life satisfaction when controlling for levels of basic human need satisfaction. In addition, the thesis investigates whether the inclusion of need satisfiers significantly improves the prediction of the wellbeing outcomes and whether the predictive performance can be further improved by classical machine learning approaches. Finally, it examines whether there are differences between life expectancy and life satisfaction regarding the importance of different basic human needs. 


According to the results, the effect of GDP is smaller for both measures of wellbeing when need satisfaction indicators are added to the linear model, and it becomes insignificant in the case of life expectancy. Moreover, need satisfaction indicators significantly improve the prediction of both life expectancy and life satisfaction, with small additional improvements from machine learning methods compared to the ordinary least squares model. Different basic needs have varying importance for life expectancy and life satisfaction but universal health coverage is crucial for both wellbeing outcomes. The results suggest that basic human need satisfaction is likely to be a relevant determinant of human wellbeing. However, the dimensions of human needs considered in the analysis are also likely to be incomplete. Moreover, in the case of life satisfaction, other factors than need satisfiers might need to be included to explain its strong correlation with GDP per capita.


## Contents of this repository

* an [overview](indicators.xlsx) of all indicators and their sources
* the [datasets](data), including the original data files from different sources
* the [R notebooks](notebooks) allowing to reproduce the analysis
* saved [results](results), including figures
* several [utility functions](utils), including implementations of customised preprocessing steps for tidymodels recipes

