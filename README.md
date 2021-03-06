Heart Disease Classification
================
Jamie Hargreaves

## Loading the data

For this project we’re going to use a [dataset taken from
Kaggle](https://www.kaggle.com/ronitf/heart-disease-uci) which contains
the attributes of patients admitted to hospital with suspected heart
problems, along with a flag indicating whether or not the patient
ultimately received a diagnosis of heart disease. We’ll start by reading
the data:

``` r
library(tidyverse)

# get data 
data <- read_csv("heart.csv")
```

## Exploratory analysis

Let’s look at the data quality and variable types:

``` r
# check the type of each column and for the number of NAs
tibble(
  colnames(data),
  map_chr(
    1:ncol(data),
    function(x){
      data[[x]] %>% typeof()
    }
  ),
  map_int(
    1:ncol(data),
    function(x){
      is.na(data[[x]]) %>% sum()
    }
  )
)
```

    ## # A tibble: 14 x 3
    ##    `colnames(data)` `map_chr(...)` `map_int(...)`
    ##    <chr>            <chr>                   <int>
    ##  1 age              double                      0
    ##  2 sex              double                      0
    ##  3 cp               double                      0
    ##  4 trestbps         double                      0
    ##  5 chol             double                      0
    ##  6 fbs              double                      0
    ##  7 restecg          double                      0
    ##  8 thalach          double                      0
    ##  9 exang            double                      0
    ## 10 oldpeak          double                      0
    ## 11 slope            double                      0
    ## 12 ca               double                      0
    ## 13 thal             double                      0
    ## 14 target           double                      0

``` r
# get number of observations
nrow(data)
```

    ## [1] 303

We can see that each of the 14 variables are numeric and that we have no
missing values in any of the 303 observations. Whilst all of the
variables have been identified as numeric, this doesn’t really make
sense since a number are actually integer representations of categorical
variables, for example, the chest pain type `cp` can take values of `0`,
`1`, `2` or `3`, but a value of say `2.6` doesn’t really mean anything.
We therefore need to convert variables like this to categorical
variables:

``` r
library(magrittr)

# convert relevant variables to characters
data %<>%
  mutate(
    sex = as.character(sex),
    cp = as.character(cp),
    fbs = as.character(fbs),
    restecg = as.character(restecg),
    exang = as.character(exang),
    slope = as.character(slope),
    thal = as.character(thal),
    target = as.character(target)
  )
```

### Continuous variables

We’ll begin our analysis by looking at the continuous variables in the
data, starting with their distribution:

``` r
library(hrbrthemes)
library(gridExtra)

# histograms
histograms <- map(
  data %>%
    select_if(is.numeric) %>%
    colnames(),
  function(x){
    data %>%
      ggplot(aes(x = !!as.name(x), y = ..density..)) + 
      geom_histogram(colour = "#00BFC4", fill = "white") + 
      geom_density(colour = "#00BFC4", fill = "#00BFC4", alpha = 0.2) + 
      labs(
        y = "Density",
        title = x
      ) + 
      theme_ipsum()
  }
)

#plot
do.call(grid.arrange, histograms)
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" width="960" style="display: block; margin: auto;" />

From the plots we can see that:

  - Patients in the sample population are most likely to be in their
    late 50s, though we can also see a smaller peak in the density curve
    in the mid-40s and in general ages range anywhere between very late
    20s to late 70s;

  - The resting blood pressure `trestbps` of patients has a peak around
    `130`, though from the histogram we can see some large groupings
    interspersed between `100` and `140`, followed by a fairly swift
    drop;

  - The distributions for both cholesterol `chol` and maximum heart rate
    `thalach` look to be fairly smooth with peaks around `240` and
    `160`, respectively;

  - The histogram for ST depression `oldpeak` (a measurement taken from
    an electrocardiogram), is heavily concentrated at `0`, though has a
    fairly wide range; and

  - Finally, the number of major vessels looks to actually be a
    categorical variable despite the fact that we’ve treated is as
    numeric, however a number of papers
    ([here](http://csjournals.com/IJCSC/PDF7-1/18.%20Tejpal.pdf), for
    example), are clear that this variable should be treated as
    continuous so we’ll follow that convention. It’s worth noting also
    that numerous papers describe this variable as taking values between
    `0` and `3` in the data set, however we can see a small number of
    instances where `ca = 4`. Given that the varibale is taken to be
    numeric, this isn’t necessarily a problem so we’ll leave these
    observations as they are since it’s not immediately obvious what the
    sensible way to replace instances of `ca = 4` would be and, given
    that we only have `303` observations, we don’t want to lose data if
    we can avoid it.

We can use box plots to look at the difference in the these numeric
variables when we split the population based on `sex`:

``` r
# boxplots
box_plots <- map(
  map_chr(
  1:ncol(data),
  function(x){
    ifelse(
      is.numeric(data[[x]]),
      names(data[, x]),
      FALSE
    )
  }
) %>%
  as_tibble() %>%
  filter(value != FALSE) %>%
  pull(),
  function(x){
    data %>%
      ggplot(aes(x = sex, y = !!as.name(x), colour = sex)) + 
      geom_boxplot(alpha = 0.3, show.legend = FALSE) + 
      geom_jitter(alpha = 0.1, show.legend = FALSE) + 
      coord_flip() +
      scale_x_discrete(label = c("Female", "Male")) + 
      labs(
        x = NULL,
        title = x
      ) +
      theme_ipsum()
  }
)

#plot
do.call(grid.arrange, box_plots)
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="960" style="display: block; margin: auto;" />

We can see from the box plots that overall there is little difference
between male and female patients in the population. The median age of
females is slightly higher than males, as is the cholosterol level and
maximum heartrate, whilst the ST depression is slightly lower. Whilst
the difference might not be large in magnitude, we can check if the
differences are statistically significant, i.e. whether we’re likely to
observe them in a world where there really is no difference. How we do
that depends on the distributions of the variables, if they’re normally
distributed then we can use an unpaired two-samples t-test, whereas if
the variables are non-normally distributed, then we can use a
two-samples Wilcoxon rank test. We therefore need to check for normality
in each variable split between male and female
subsets:

``` r
# Shapiro-Wilk test for normality, p > 0.05 ==> data normally distributed
num_vars <- data %>%
  select_if(is.numeric) %>%
  names()

data %>%
  group_by(sex) %>%
  group_split() %>%
  map_df(
    function(x){
      tibble(
        sex = unlist(rep(distinct(x[, "sex"]), length(num_vars))),
        var = num_vars,
        p.value = map_dbl(
          num_vars,
          function(y){
            shapiro.test(x[[y]])$p.value
          }
        ),
        is.normal = ifelse(p.value > 0.05, "yes", "no")
      )
    }
  )
```

    ## # A tibble: 12 x 4
    ##    sex   var       p.value is.normal
    ##    <chr> <chr>       <dbl> <chr>    
    ##  1 0     age      1.39e- 1 yes      
    ##  2 0     trestbps 6.32e- 3 no       
    ##  3 0     chol     3.02e- 5 no       
    ##  4 0     thalach  4.75e- 4 no       
    ##  5 0     oldpeak  7.89e-11 no       
    ##  6 0     ca       1.45e-13 no       
    ##  7 1     age      4.00e- 2 no       
    ##  8 1     trestbps 1.07e- 4 no       
    ##  9 1     chol     5.27e- 1 yes      
    ## 10 1     thalach  8.71e- 3 no       
    ## 11 1     oldpeak  1.90e-12 no       
    ## 12 1     ca       2.41e-17 no

Whilst two of the variables (female `age` and male `chol`) are normally
distributed, those same variables are non-normally distributed for the
opposite sex and the other variables in the data set are also
non-normally distributed, so we’ll opt for the two-samples Wilcoxon rank
test:

``` r
# calculate the Wilcoxon test statistic, p > 0.05 ==> insignificant
tibble(
  var = num_vars,
  p.value = map_dbl(
    num_vars,
    function(x){
     wilcox.test(as.formula(paste0(x, "~ sex")), data = data)$p.value
    }
  ),
  is.sig = ifelse(p.value < 0.05, "yes", "no")
)
```

    ## # A tibble: 6 x 3
    ##   var      p.value is.sig
    ##   <chr>      <dbl> <chr> 
    ## 1 age      0.0851  no    
    ## 2 trestbps 0.358   no    
    ## 3 chol     0.00856 yes   
    ## 4 thalach  0.489   no    
    ## 5 oldpeak  0.0802  no    
    ## 6 ca       0.0381  yes

We can see that the difference in median cholesterol `chol` and median
number of major vessels `ca` for males and females is significant,
whilst the differences between the remaining medians aren’t
statistically significant.

It’d be interesting to understand how the different variables correlate
with the `target` variable which indicates whether or not a patient had
heart disease (`0` indicating no heart disease and `1` indicating heart
disease), and we can do this by calculating the point-biserial
correlation coefficients:

``` r
# numerical variables
num_vars <- data %>%
  select_if(is.numeric)

# plot the correlation
tibble(
  var = names(num_vars),
  corr = map_dbl(
    1:ncol(num_vars),
    function(x){
      cor.test(as.numeric(data$target), num_vars[[x]])$estimate
    }
  ),
  is.sig = map_dbl(
    1:ncol(num_vars),
    function(x){
      ifelse(
        cor.test(as.numeric(data$target), num_vars[[x]])$p.value < 0.05,
        1,
        0
      )
    }
  )
) %>%
  ggplot(aes(x = var, y = corr, fill = factor(is.sig, levels = c(0,1), 
                                              label = c("No", "Yes")), 
                                              colour = ..fill..)) +
  geom_col(alpha = 0.3) + 
  coord_flip() + 
  scale_fill_discrete(name = "Is significant?") + 
  scale_color_discrete(guide = FALSE) + 
  xlab(NULL) + 
  ylab("Correlation coefficient") + 
  theme_ipsum()
```

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" width="672" style="display: block; margin: auto;" />

We can see that all the correlations are significant (i.e. have a
![p](https://latex.codecogs.com/png.latex?p "p")-value less than
![5\\%](https://latex.codecogs.com/png.latex?5%5C%25 "5\\%")), except
for the correlation with cholesterol `chol`, however this is a
negligible correlation regardless. In fact in general the correlations
are mostly negligible, though there are weak correlations
(![\\approx0.4](https://latex.codecogs.com/png.latex?%5Capprox0.4
"\\approx0.4") in absolute value), with both the maximum heartrate
`thalach` and the ST depression `oldpeak`. We can interpret this to mean
that moving from a diagnosis of “does not have heart disease” to “has
heart disease” is typically associated with an increase in maximum
heartrate and a decrease in ST depression.

### Categorical variables

We’ve done some analysis of the numeric variables in the data set, so
now let’s move on to looking at the categorical variables. A key first
consideration is how skewed our data set is towards either people with
heart disease or those without?

``` r
# plot 
data %>%
  group_by(target) %>%
  summarise(count = n()) %>%
  mutate(per = str_c(round(100 * count / sum(count), 1), "%")) %>%
  ggplot(aes(x = factor(target, levels = c(0,1), label = c("No heart disease", "Heart disease")), 
             y = count, label = per)) + 
  geom_col(colour = "#00BFC4", fill = "#00BFC4", alpha = 0.3, width = 0.5) + 
  geom_text(hjust = -0.2) + 
  coord_flip(clip = "off") + 
  xlab(NULL) + 
  ylab("Number of Occurrences") +
  theme_ipsum()
```

<img src="README_files/figure-gfm/unnamed-chunk-9-1.png" width="672" style="display: block; margin: auto;" />

From the above plot, we can see that there are slightly more patients
with heart disease than without, though the data set isn’t massively
skewed, with only a ![9\\%](https://latex.codecogs.com/png.latex?9%5C%25
"9\\%") difference. Similarly, we can look at the proportion of patients
in each of a variable’s classes split by `target` classification:

``` r
# get the categorical variables
cat_vars <- data %>%
  select_if(is.character) %>%
  select(-one_of("target"))

# plots
cat_plots <- map(
  names(cat_vars),
  function(x){
    data %>%
      group_by(target, !!as.name(x)) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = factor(target, levels = c(0,1), 
                            label = c("No heart disease", "Heart disease")), 
                            y = count, fill = !!as.name(x), 
                            colour = ..fill..)) + 
      geom_col(position = "dodge", alpha = 0.3) + 
      coord_flip() + 
      scale_fill_discrete(name = NULL) +
      scale_color_discrete(guide = FALSE) + 
      labs(
        x = NULL,
        title = x
      ) +
      ylab("Number of Occurrences") + 
      theme_ipsum() + 
      theme(legend.position = "bottom")
  }
)

# print
do.call(grid.arrange, cat_plots)
```

<img src="README_files/figure-gfm/unnamed-chunk-10-1.png" width="960" style="display: block; margin: auto;" />

A number of things stand out from the plots above:

  - There is a similar proportion of men in both subsets of patients,
    but a much larger proportion of women in the subet with heart
    disease than the subset without;

  - There is a clear difference between the types of chest pain
    experienced by those with and without heart disease. Patients
    without heart disease primarily experience typical angina (`cp
    = 0`), whilst those with heart disease predomninantly experience
    nonanginal pain (`cp = 3`) - see
    [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4468223/) for
    reference to the types of chest pain measured in this data set;

  - There is almost no difference in the threshold blood sugar levels of
    patients with and without heart disease;

  - The proportion of patients with normal ECG results decreases and the
    proportion with abnormal ECG results increases between patients
    without heart disease and patients with heart disease;

  - The proportion of patients without exercise induced angina is
    noticeably higher in patients with heart disease;

  - For patients without heart disease, the slope of the ST curve peak
    is predominantly flat (`slope = 1`), whilst for those with heart
    disease, the curve peak is predominantly downward-sloping (`slope
    = 2`); and

  - Finaly, patients with heart disease primarily display a fixed
    abnormality in the Thallium-201 stress scintigraphy test (`thal
    = 2`), whilst those without heart disease typically display a
    reversible abnormality (`thal = 3`).

It’s also noticeable that there looks to be at least one instance in
both the “no heart disease” and “heart disease” subsets where `thal
= 0`. The `thal` variable is only defined for classes `1`, `2` and `3`
which means we’ve actually got some missing values, so before we carry
on we’ll identify and replace them.

``` r
# count the number of observations in each level
data %>%
  group_by(thal) %>%
  summarise(n()) 
```

    ## # A tibble: 4 x 2
    ##   thal  `n()`
    ##   <chr> <int>
    ## 1 0         2
    ## 2 1        18
    ## 3 2       166
    ## 4 3       117

As we thought, there are only two instances where `thal = 0`. To replace
them, we’ll take the most frequent value for `thal` within the
respective `target` subet, since we already saw from the plots that one
of the missing values occurs in the subset of patients with heart
disease and the other occurs in the subset without.

``` r
# get most frequent level in each group
data %>%
  group_by(target, thal) %>%
  summarise(n())
```

    ## # A tibble: 8 x 3
    ## # Groups:   target [2]
    ##   target thal  `n()`
    ##   <chr>  <chr> <int>
    ## 1 0      0         1
    ## 2 0      1        12
    ## 3 0      2        36
    ## 4 0      3        89
    ## 5 1      0         1
    ## 6 1      1         6
    ## 7 1      2       130
    ## 8 1      3        28

We can see that for patients without heart disease, the most frequent
`thal` category is `3`, whilst for patients with heart disease the most
frequent category is `2`.

``` r
# update missing values 
data %<>%
  mutate(
    thal = ifelse(
      target == "0" & thal == "0",
      "3",
      ifelse(
        target == "1" & thal == "0",
        "2",
        thal
      )
    )
  )
```

Now that we’ve looked at both the continuous and categorical variables,
we have a better understanding of the data and can move on to modelling.

## Training a model

To create a model we’ll randomly partition the data into train and test
sets, allocating ![70\\%](https://latex.codecogs.com/png.latex?70%5C%25
"70\\%") of the data to train, then train a model using repeated
![k](https://latex.codecogs.com/png.latex?k "k")-fold cross validation.
Under this method, we take the training set and randomly partition it
into ![k](https://latex.codecogs.com/png.latex?k "k") approximately
equally sized folds (or groups). We then select one fold to use as a
validation or hold out set and train a model on the remaining
![k-1](https://latex.codecogs.com/png.latex?k-1 "k-1") folds. We repeat
the process, each time using a different fold as a validation set so
that in total each fold is used once as a validation set and
![k-1](https://latex.codecogs.com/png.latex?k-1 "k-1") times as a
training set. We then compute the cross validated error by taking the
average over the errors of each individual model so that the cross
validated error over ![k](https://latex.codecogs.com/png.latex?k "k")
models,
![CV\_{(k)}](https://latex.codecogs.com/png.latex?CV_%7B%28k%29%7D
"CV_{(k)}"), is given by

<center>

![CV\_{(k)} = \\frac{1}{k} \\sum\_{i=1}^k
\\textrm{Err}\_i,](https://latex.codecogs.com/png.latex?CV_%7B%28k%29%7D%20%3D%20%5Cfrac%7B1%7D%7Bk%7D%20%5Csum_%7Bi%3D1%7D%5Ek%20%5Ctextrm%7BErr%7D_i%2C
"CV_{(k)} = \\frac{1}{k} \\sum_{i=1}^k \\textrm{Err}_i,")

</center>

where
![\\textrm{Err}\_i](https://latex.codecogs.com/png.latex?%5Ctextrm%7BErr%7D_i
"\\textrm{Err}_i") is the classification error of the
![i](https://latex.codecogs.com/png.latex?i "i")th model, i.e. the
proportion of observations incorrectly classified. For a training set
consisting of ![n](https://latex.codecogs.com/png.latex?n "n")
observations, the classification error is defined by

<center>

![\\textrm{Err} = \\frac{1}{n}\\sum\_{i=1}^n
I\_i,](https://latex.codecogs.com/png.latex?%5Ctextrm%7BErr%7D%20%3D%20%5Cfrac%7B1%7D%7Bn%7D%5Csum_%7Bi%3D1%7D%5En%20I_i%2C
"\\textrm{Err} = \\frac{1}{n}\\sum_{i=1}^n I_i,")

</center>

where

<center>

![I\_i = \\begin{cases} 0, &\\text{if } y\_i = \\hat{y}\_i\\\\ 1,
&\\text{if } y\_i \\neq
\\hat{y\_i}\\end{cases},](https://latex.codecogs.com/png.latex?I_i%20%3D%20%5Cbegin%7Bcases%7D%200%2C%20%26%5Ctext%7Bif%20%7D%20y_i%20%3D%20%5Chat%7By%7D_i%5C%5C%201%2C%20%26%5Ctext%7Bif%20%7D%20y_i%20%5Cneq%20%5Chat%7By_i%7D%5Cend%7Bcases%7D%2C
"I_i = \\begin{cases} 0, &\\text{if } y_i = \\hat{y}_i\\\\ 1, &\\text{if } y_i \\neq \\hat{y_i}\\end{cases},")

</center>

and where ![y\_i](https://latex.codecogs.com/png.latex?y_i "y_i") is the
actual value of the ![i](https://latex.codecogs.com/png.latex?i "i")th
training observation and
![\\hat{y}\_i](https://latex.codecogs.com/png.latex?%5Chat%7By%7D_i
"\\hat{y}_i") is the predicted value of the
![i](https://latex.codecogs.com/png.latex?i "i")th training observation.
Since we’re using repeated ![k](https://latex.codecogs.com/png.latex?k
"k")-fold cross validation, we then repeat the process
![N](https://latex.codecogs.com/png.latex?N "N") times and take the
final cross validated error
![RCV\_{(N)}](https://latex.codecogs.com/png.latex?RCV_%7B%28N%29%7D
"RCV_{(N)}") to be the average of each of the individual cross validated
errors

<center>

![RCV\_{(N)} = \\frac{1}{N} \\sum\_{i=0}^N
CV\_{(i)}.](https://latex.codecogs.com/png.latex?RCV_%7B%28N%29%7D%20%3D%20%5Cfrac%7B1%7D%7BN%7D%20%5Csum_%7Bi%3D0%7D%5EN%20CV_%7B%28i%29%7D.
"RCV_{(N)} = \\frac{1}{N} \\sum_{i=0}^N CV_{(i)}.")

</center>

Throughout, we’ll take ![k
= 10](https://latex.codecogs.com/png.latex?k%20%3D%2010 "k = 10") and
![N = 5](https://latex.codecogs.com/png.latex?N%20%3D%205 "N = 5"). This
then gives us a robust way to compare the performance of different
models based on an estimate of the test error.

### Baseline model

Before we actually train any models, it’s worth coming up with a
baseline model to compare them to. The baseline model should be a naive
prediction and if our models can’t beat the accuracy of the baseline,
then it suggests that our approach may be wrong or potentially that the
response variable just isn’t something that can be easily predicted
based on the data we have available.

This is a binary classification problem since, given the set of patient
attributes, we want to answer the question “does the patient have heart
disease?” As such, an obvious, simple prediction would be to predict the
most common result. Let’s start by creating our training and test sets:

``` r
library(caret)
set.seed(100)

# split the data into test and train, with 70% train
train_index <- createDataPartition(data$target, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# proportion of heart disease vs. no heart disease
train_data %>%
  group_by(target) %>%
  tally() %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 2 x 3
    ##   target     n  prop
    ##   <chr>  <int> <dbl>
    ## 1 0         97 0.455
    ## 2 1        116 0.545

We can see from above that the most frequent value for `target` in the
training set is `1` and that if we were to simply predict every value of
`target` to be `1`, then we’d achieve a classification accuracy of
roughly ![55\\%](https://latex.codecogs.com/png.latex?55%5C%25 "55\\%"),
so this will form the baseline against which we’ll compare any other
model.

### Multiple logistic regression

We’re going to try two different models, the first is one of the
simplest methods for classification, multiple logistic regression, so
let’s talk a little about how it works.

With a typical regression model we’re trying to predict the value of a
continuous response variable, for example, a person’s salary given their
age and the number of years they spent in higher education. In this case
it’s completely reasonable get a prediction of say `27,836.89`. However,
for the same reason that we encoded some of the variables in the data
set as characters previously, it makes no sense if our model were to
predict a value such as `0.768` in response to the question of whether
or not a person has heart disease - they either do or they don’t.

With multiple logistic regression, rather than predicting a `0` or a
`1`, we predict the probability that an observation belongs to one of
the categories. We could use multiple linear regression to model this
probability, but this approach suffers from the fact that we could
return probabilities that were either less than `0` or greater than `1`,
which would be essentially meangingless. Multiple logistic regression
avoids this problem by constraining all predictions to the interval
![\[0,1\]](https://latex.codecogs.com/png.latex?%5B0%2C1%5D "[0,1]"),
modelling the probaility using the logistic function

<center>

![p(\\textbf{X})= \\frac{e^{\\beta\_0 + \\beta\_1\\textbf{X}}}{1 +
e^{\\beta\_0 +
\\beta\_1\\textbf{X}}},](https://latex.codecogs.com/png.latex?p%28%5Ctextbf%7BX%7D%29%3D%20%5Cfrac%7Be%5E%7B%5Cbeta_0%20%2B%20%5Cbeta_1%5Ctextbf%7BX%7D%7D%7D%7B1%20%2B%20e%5E%7B%5Cbeta_0%20%2B%20%5Cbeta_1%5Ctextbf%7BX%7D%7D%7D%2C
"p(\\textbf{X})= \\frac{e^{\\beta_0 + \\beta_1\\textbf{X}}}{1 + e^{\\beta_0 + \\beta_1\\textbf{X}}},")

</center>

where ![\\textbf{X} = (X\_1, X\_2, \\ldots,
X\_p)](https://latex.codecogs.com/png.latex?%5Ctextbf%7BX%7D%20%3D%20%28X_1%2C%20X_2%2C%20%5Cldots%2C%20X_p%29
"\\textbf{X} = (X_1, X_2, \\ldots, X_p)") is the vector of
![p](https://latex.codecogs.com/png.latex?p "p") predictors and
![p(\\textbf{X}) = \\rm{Pr}(Y = 1 \\,|\\,
\\textbf{X})](https://latex.codecogs.com/png.latex?p%28%5Ctextbf%7BX%7D%29%20%3D%20%5Crm%7BPr%7D%28Y%20%3D%201%20%5C%2C%7C%5C%2C%20%5Ctextbf%7BX%7D%29
"p(\\textbf{X}) = \\rm{Pr}(Y = 1 \\,|\\, \\textbf{X})") is the
probability that the response variable (in this case `target`), takes a
value of ![1](https://latex.codecogs.com/png.latex?1 "1") given some
![X\_i](https://latex.codecogs.com/png.latex?X_i "X_i"). In general if
we don’t have any domain knowledge which suggests that all the features
in our data set should be important in our model, then we might want to
find an optimal feature set, but since we have good reason to believe
that each of the features in the data set are indicators of heart
disease, we’ll use them all. We’ll use the `caret` library to train the
model:

``` r
# repeated k-fold cross validation
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)  

# train the model
model_log <- train(
  target ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl
)

# check the accuracy
model_log
```

    ## Generalized Linear Model 
    ## 
    ## 213 samples
    ##  13 predictor
    ##   2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 192, 191, 191, 192, 192, 192, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.8359524  0.6687658

We can see from the above results that the multiple logistic regression
model has an accuracy of roughly
![84\\%](https://latex.codecogs.com/png.latex?84%5C%25 "84\\%") which is
impressive for such a simple model and represents a nearly
![53\\%](https://latex.codecogs.com/png.latex?53%5C%25 "53\\%") increase
in accuracy compared to the baseline model. The model has also returned
a `Kappa` value of roughly `0.67` - this is Cohen’s kappa which
essentially gives a measure of the model accuracy taking into account
the likelihood that the model could’ve classified some observations
correctly by chance. Let’s see which of the features were most
influential by looking at the variable importance scores:

``` r
# get variable importance scores
var_imp <- varImp(model_log)$importance

# plot
tibble(
  var = rownames(var_imp),
  score = var_imp$Overall
) %>%
  ggplot(aes(x = reorder(var, score), y = score)) + 
  geom_col(colour = "#00BFC4", fill = "#00BFC4", alpha = 0.3, width = 0.8) + 
  coord_flip() + 
  xlab(NULL) +
  ylab("Variable Importance Score") + 
  theme_ipsum()
```

<img src="README_files/figure-gfm/unnamed-chunk-16-1.png" width="672" style="display: block; margin: auto;" />

The most influential predictor in the model is whether or not the
patient experienced nonanginal pain (`cp = 3`), closely followed by
whether or not they experienced atypical angina (`cp = 2`), and this
ties back to the visualisations we saw previously which showed the
proportions of each chest pain type experienced by each subset of
patients (heart disease or no heart disease). This is followed by the
number of major vessels `ca`, after which things gradually start to drop
off. It’s also noticeable that whether or not the patient was male (`sex
= 1`), looks to be a fairly important predictor and this is sensible
since again we saw in the visualisation that there were significantly
more male patients in the “no heart disease” subset than female.

Whilst the overall classifiation accuracy is a useful metric, it can
also be misleading. For example, if our data set were heavily skewed
towards one class, then we could potentially mis-classify an entire
category, but still achieve a high overall classification accuracy. This
is particularly important in areas of medical diagnoses where only a
small number of observations of a disease are present in the data. We
alredy checked previously that our data wasn’t highly skewed, but even
so it’s useful to understand how the model classified the different
sets, and we can do this by looking at the confusion matrix:

``` r
# confusion matrix for the training set
confusionMatrix.train(model_log)
```

    ## Cross-Validated (10 fold, repeated 5 times) Confusion Matrix 
    ## 
    ## (entries are percentual average cell counts across resamples)
    ##  
    ##           Reference
    ## Prediction    0    1
    ##          0 36.7  7.6
    ##          1  8.8 46.9
    ##                             
    ##  Accuracy (average) : 0.8357

From the confusion matrix we can see that the sensitivity of the model
is ![87\\%](https://latex.codecogs.com/png.latex?87%5C%25 "87\\%"),
i.e. the percentage of true heart disease patients that were correctly
identified, whilst the specificity of the model (the percentage of
patients correctly identified as not having heart disease), is
approximately ![80\\%](https://latex.codecogs.com/png.latex?80%5C%25
"80\\%"). This is positive since in reality we’d likely be most
concerned about correctly identifying all patients who do have heart
disease, even if in practice that means mis-identifying some who don’t.
If we think about the baseline model, we’d achieve a sensitivity of
![100\\%](https://latex.codecogs.com/png.latex?100%5C%25 "100\\%")
since, by our definition of the model, we assume that every patient had
heart disease so we’d always classify any true occurrence of heart
disease correctly. However, we’d have a specificity of
![0](https://latex.codecogs.com/png.latex?0 "0") since we’d incorrectly
classify every patient without heart disease and this probably isn’t a
reasonable trade-off in order to achieve a high sensitivity.

``` r
# confusion matrix for the baseline model
conf_matrix_base <- confusionMatrix(
  factor(rep(1, nrow(test_data)), levels = c(0, 1)),
  as.factor(test_data$target)
)

conf_matrix_base$table
```

    ##           Reference
    ## Prediction  0  1
    ##          0  0  0
    ##          1 41 49

The confusion matrix for the baseline shows exactly what we said above.
In total there were `49` true instances of heart disease, and the
baseline classifies them all correctly. However, there were also `41`
instances of patients without heart disease, but the baseline classifies
all of these incorrectly. The multiple logistic regression model is
therefore more accurate overall than the baseline and, whilst its
sensitivity is lower, it is much more balanced than the baseline with a
signifiantly higher specificity.

### Decision tree

Next we’re going to use a decision tree to classify patients. Unlike the
multiple logistic regression model we used previously, the decision tree
classifier directly assigns each observation to a specific class, rather
than assigning a probability.

A decision tree works by posing an initial binary question about the
data in the root node, then based on the response, splits the data into
one of two leaf nodes. At each node, this binary splitting occurs until
eventually (according to some stopping criteria, e.g. maximum tree depth
or a complexity parameter), we reach a terminal node, at which point our
prediction is the most common classification of the observations in the
remaining subset of the data in that node.

To select the predictor on which the first split should be made, we
compare how well each of the features splits the data using a quantity
known as the *Gini index* which gives us a measure of the impurity of a
node (though there are alternative splitting criteria such as
cross-entropy). A Gini index of `0` would indicate a completely pure
node and would mean that the node was made up only a single class. The
best predictor for a given node is the predictor that results in the
lowest Gini index. For a given node containing
![K](https://latex.codecogs.com/png.latex?K "K") distinct classes, the
Gini index, ![G](https://latex.codecogs.com/png.latex?G "G"), is defined
by

<center>

![G = 1 - \\sum\_{i=1}^K
p\_i^2,](https://latex.codecogs.com/png.latex?G%20%3D%201%20-%20%5Csum_%7Bi%3D1%7D%5EK%20p_i%5E2%2C
"G = 1 - \\sum_{i=1}^K p_i^2,")

</center>

where ![p\_i](https://latex.codecogs.com/png.latex?p_i "p_i") is the
proportion of samples of class
![i](https://latex.codecogs.com/png.latex?i "i"). For example, if we
were trying to determine which feature to use in the root node of the
tree, we could take the `fbs` variable and imagine our node poses the
question “does `fbs = 1`?”. To determine the Gini index of the node, we
look at how `fbs` splits the data:

``` r
# look at the split
data %>%
  group_by(fbs, target) %>%
  summarise(n())
```

    ## # A tibble: 4 x 3
    ## # Groups:   fbs [2]
    ##   fbs   target `n()`
    ##   <chr> <chr>  <int>
    ## 1 0     0        116
    ## 2 0     1        142
    ## 3 1     0         22
    ## 4 1     1         23

The first branch from the node would be the “yes” response - we can see
from the table above that when `fbs = 1`, we have a total of `45`
patients, `23` with heart disease and `22` without. Similarly, in the
“no” branch we can see a total of `258` patients, `142` with heart
disease and `116` without. This is a little easier to see
visually:

<img src="README_files/figure-gfm/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

The Gini scores for the leaf nodes are given by:

``` r
# first node
gini_1 <- 1 - (23 / 45)^2 - (22 / 45)^2

# second node
gini_2 <- 1 - (116 / 258)^2 - (142 / 258)^2

# overall 
gini_overall <- (45 / (45 + 258)) * gini_1 + (258 / (45 + 258)) * gini_2
gini_overall
```

    ## [1] 0.4956396

The overall Gini index for the `fbs` node (the weighted averages of the
two leaf nodes), is therefore `0.496` and we would compare this to the
Gini index of each of the other predictors to determine whether or not
`fbs` resulted in the purest node possible. This approach to splitting
is called *greedy* because at any step, we’re only looking at which
feature gives the best split there and then, rather than trying to work
out which feature is best over the tree further down the line.

The `fbs` variable is categorical but we can calculate the Gini index of
a continuous variable in a similar way. Say we want to calculate the
index for `chol`, we first need to decide what threshold to consider as
our criteria, since the node needs to perform a binary split. To do this
we first order the values of `chol` from smallest to largest:

``` r
# arrange cholesterol from lowest to highest
chol_order <- data %>%
  select(chol) %>%
  arrange(chol)

chol_order %>%
  head()
```

    ## # A tibble: 6 x 1
    ##    chol
    ##   <dbl>
    ## 1   126
    ## 2   131
    ## 3   141
    ## 4   149
    ## 5   149
    ## 6   157

We then calculate the average of values one and two, then of values two
and three and so on:

``` r
# calculate pairwise means
chol_order %<>%
  mutate(lagged_mean = 0.5 * (chol + lag(chol))) %>%
  mutate(lagged_mean = round(lagged_mean, 1))

chol_order %>%
  head()
```

    ## # A tibble: 6 x 2
    ##    chol lagged_mean
    ##   <dbl>       <dbl>
    ## 1   126         NA 
    ## 2   131        128.
    ## 3   141        136 
    ## 4   149        145 
    ## 5   149        149 
    ## 6   157        153

We would then use each value of `lagged_mean` as the threshold in a
node, i.e. we’d take a node which asked “is `chol < 128.5`?”, “is `chol
< 136`?” etc. For each of those nodes we’d then calculate the Gini index
like we did above and this would tell us which value of `chol` to use as
the threshold value. Once we know the threshold value, we can compare
its Gini index to the index of the various other variables like `fbs` to
decide if it should be in our root node.

Finally, there are also variables in our data set like `thal` which can
take values of `1`, `2` or `3`. For these kinds of variables we work out
the Gini index by splitting on each combination of the variable. In the
case of `thal`, `thal = 1`, `thal = 2`, `thal = 3`, `thal = 1` or `thal
= 2`, `thal = 1` or `thal = 3`, or `thal = 2` or `thal = 3`.

Let’s now move on and train our tree:

``` r
set.seed(100)

# train the model
model_tree <- train(
  x = train_data[, -which(names(train_data) %in% "target")],
  y = train_data$target,
  method = "rpart",
  trControl = ctrl
)

# check the accuracy
model_tree
```

    ## CART 
    ## 
    ## 213 samples
    ##  13 predictor
    ##   2 classes: '0', '1' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 5 times) 
    ## Summary of sample sizes: 191, 191, 191, 191, 193, 193, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          Accuracy   Kappa    
    ##   0.01546392  0.7628831  0.5203895
    ##   0.04123711  0.7376190  0.4701111
    ##   0.52577320  0.6319913  0.2233102
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.01546392.

Again, despite such a relatively simple model, we’ve managed to achieve
an accuracy of approximately
![76\\%](https://latex.codecogs.com/png.latex?76%5C%25 "76\\%"). Whilst
this is decrease in accuracy of roughly
![10\\%](https://latex.codecogs.com/png.latex?10%5C%25 "10\\%") compared
to the ![84\\%](https://latex.codecogs.com/png.latex?84%5C%25 "84\\%")
accuracy we were able to achieve from the multiple logistic regression
model, as we’ll see further down, the decision tree approach benefits
from significantly greater interpretability, not least because we can
easily visualise the train of logic that goes into a classification, and
this shouldn’t be underestimated, especially for a problem like this
where one of the key interests may actually be gaining an understanding
of the driving factors behind heart disease as opposed to purely the raw
predictive power of the model.

``` r
library(rattle)

# visualise the decision tree
fancyRpartPlot(model_tree$finalModel, sub = "")
```

<img src="README_files/figure-gfm/unnamed-chunk-25-1.png" width="672" style="display: block; margin: auto;" />

We can see from the visualisation of the final decision tree that
`thal`, `cp`, `oldpeak` and `thalach` are the only predictors considered
when classifying the patient. This isn’t totally desimilar to the top
features selected using the multiple logistic regression model, though
our tree hasn’t used the number of major vessels `ca`. We can see that
![85\\%](https://latex.codecogs.com/png.latex?85%5C%25 "85\\%") of
patients with a fixed defect, `thal = 2`, were categorised has having
heart disease, though the ST depression `oldpeak` is a necessary
differentiator. Similarly,
![74\\%](https://latex.codecogs.com/png.latex?74%5C%25 "74\\%") of
patients with either normal or reversible defects, `thal = 1` or `thal
= 3`, are ultimately categorised as not having heart disease, though
here two additional categories are necessary, namely the type of chest
pain `cp` and maximum heart rate `thalach`. This makes sense since in
the visualisations we looked at previously, we saw that the vast
majority of patients without hard disease had `cp = 0`, and from the
point-biserial correlation coefficients we saw moving from `target = 0`
to `target = 1` was associated with an increase in `thalach`.

As we did for the multiple logistic regression model, we should also
look at the confusion matrix:

``` r
# confusion matrix for decision tree 
confusionMatrix.train(model_tree)
```

    ## Cross-Validated (10 fold, repeated 5 times) Confusion Matrix 
    ## 
    ## (entries are percentual average cell counts across resamples)
    ##  
    ##           Reference
    ## Prediction    0    1
    ##          0 33.1 11.2
    ##          1 12.5 43.3
    ##                             
    ##  Accuracy (average) : 0.7634

From the confusion matrix we can see that the model has a sensitivity of
![79\\%](https://latex.codecogs.com/png.latex?79%5C%25 "79\\%") which is
a drop of approximately
![9\\%](https://latex.codecogs.com/png.latex?9%5C%25 "9\\%") compared to
the multiple logistic regression model. The specificty is
![73\\%^](https://latex.codecogs.com/png.latex?73%5C%25%5E "73\\%^")
which also represents a drop of around
![9\\%](https://latex.codecogs.com/png.latex?9%5C%25 "9\\%") from the
multiple logistic regression model. The decision tree model is therefore
worse than the logistic regression model, both in terms of its overall
classification accuracy, as well as in its ability to correctly identify
true instances of either class, though it’s arguably much easier to
interpret and therefore potentially more informative.

## Choosing a final model

We’ve now trained both a logistic regression model and a decision tree
model. We’ve seen that despite the relative simplicity of the both
methods, each is able to achieve a surprisingly good overall accuracy
and neither seems to suffer from skewed accuracy between either class.
Since the multiple logistic regression model was the most accuracte
(both in terms of overall classification accuracy, as well as
sensitivity and specificity), we’ll use it as the final model and test
on the test set to see how well it performs on unseen data:

``` r
# predict on the test data
pred <- predict(model_log, test_data)

# confusion matrix
confusionMatrix(pred, as.factor(test_data$target), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 30  5
    ##          1 11 44
    ##                                           
    ##                Accuracy : 0.8222          
    ##                  95% CI : (0.7274, 0.8948)
    ##     No Information Rate : 0.5444          
    ##     P-Value [Acc > NIR] : 2.84e-08        
    ##                                           
    ##                   Kappa : 0.6373          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.2113          
    ##                                           
    ##             Sensitivity : 0.8980          
    ##             Specificity : 0.7317          
    ##          Pos Pred Value : 0.8000          
    ##          Neg Pred Value : 0.8571          
    ##              Prevalence : 0.5444          
    ##          Detection Rate : 0.4889          
    ##    Detection Prevalence : 0.6111          
    ##       Balanced Accuracy : 0.8148          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

From the confusion matrix and other accuracy metrics we can see that on
the test set, the logistic regression model achieved a classification
accuracy of ![82\\%](https://latex.codecogs.com/png.latex?82%5C%25
"82\\%") which is extremely close to the training set accuracy of
![84\\%](https://latex.codecogs.com/png.latex?84%5C%25 "84\\%"). In
addition, there’s actually been an increase in the sensitivity from
![87\\%](https://latex.codecogs.com/png.latex?87%5C%25 "87\\%") to
![89\\%](https://latex.codecogs.com/png.latex?89%5C%25 "89\\%"), but a
noticeable decrease in the specificity, from
![80\\%](https://latex.codecogs.com/png.latex?80%5C%25 "80\\%") to
![73\\%](https://latex.codecogs.com/png.latex?73%5C%25 "73\\%"). We can
see from this that the cross validation approach to training has in fact
provided an extremely good estimate of the true test error. We can also
be confident that our model is not overfit, since we’ve achieved a high
accuracy on the training set, suggesting we haven’t oversimilified
things too much, as well as low variance since the error was similar on
the unseen data.

## Conclusion

We’ve trained two different types of classification models: a multiple
logistic regression model and a decision tree. Whilst both models were
able to significantly ourperform the baselines model, ultimately, it was
the logistic regression model that proved to be the most accurate, both
in terms of overall classification accuracy, achieving an accuracy of
![82\\%](https://latex.codecogs.com/png.latex?82%5C%25 "82\\%") on the
test set, as well as sensitivity and specificity. Despite this, we also
saw that whilst it was less accurate, the decision tree was much more
interpretable than the logistic regression model, so depending on the
purpose of our model (either prediction or inference), we could use
either and be confident.
