# Pretty Minimalistic Plots
## Description
The package aims to be an easy method to create plots in minimalistic format. This is an extension from 'ggplot2' and 'ggthemes' package.

## Reference
So far, this package is inspired by suggestions in the book "The Visual Display of Quantitative Information" by Edward Tufte.

## Examples

### Simple bar plot
```R
tufteBar(msleep, x = "order", x.text.rotate = 45)
```
![alt tag](https://github.com/dungkhuc/PersonalRLib/blob/master/SimpleBar.png) 

### Dodged bar plot, mean value of each case
```R
tufteBar(diamonds, "cut", "price", classify = "clarity", summary = "mean")
```
![alt tag](https://github.com/dungkhuc/PersonalRLib/blob/master/DodgedBar_mean.png))

### Dodged bar plot, sum values of each case
```R
dat = movies
dat[dat$rating < 5] = "bad"
dat$sentiment[dat$rating >= 5 && dat < 7] = "average"
dat$sentiment[dat$rating >=7] = "good"
tufteBar(dat, "sentiment", "length", "as.factor(Comedy)")
```
![alt tag](https://github.com/dungkhuc/PersonalRLib/blob/master/DodgedBar_sum.png)