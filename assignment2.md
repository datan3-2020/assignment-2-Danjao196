Data analysis assignment 2
================
Daniel Orchard
06 Feb 2020

In this assignment you will work with relational data, i.e. data coming
from different data tables that you can combine using keys. Please read
ch.13 from R for Data Science before completing this assignment –
<https://r4ds.had.co.nz/relational-data.html>.

## Read data

We will work with three different tables: household roster from wave 8
(*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and
household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

``` r
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("h_egoalt.tab")
Stable <- read_tsv("xwavedat.tab")
Hh8 <- read_tsv("h_hhresp.tab")
```

## Filter household roster data (10 points)

The **egoalt8** data table contains data on the kin and other
relationships between people in the same household. In each row in this
table you will have a pair of individuals in the same household: ego
(identified by *pidp*) and alter (identified by *apidp*).
*h\_relationship\_dv* shows the type of relationship between ego and
alter. You can check the codes in the Understanding Society codebooks
here –
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and
wives or cohabiting partners (codes 1 and 2). For convenience, we also
want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household
identifier), *h\_relationship\_dv*, *h\_esex* (ego’s sex), and *h\_asex*
(alter’s sex).

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1 | h_relationship_dv == 2) %>%
        select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex)
```

Each couple now appears in the data twice: 1) with one partner as ego
and the other as alter, 2) the other way round. Now we will only focus
on heterosexual couples, and keep one observation per couple with women
as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%
        # filter out same-sex couples
        filter(h_sex != h_asex) %>%
        # keep only one observation per couple with women as egos
        filter(h_sex == 2)
```

## Recode data on ethnicity (10 points)

In this assignment we will explore ethnic endogamy, i.e. marriages and
partnerships within the same ethnic group. First, let us a create a
version of the table with stable individual characteristics with two
variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

Let’s code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable
with the following values: “White” (codes 1 to 4) and “non-White” (all
other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = recode(racel_dv,
                
 `1` = "White",
 `2` = "White",
 `3` = "White",
 `4` = "White",
 .default = "Non-White"
        ))

table(Stable2$race)
```

    ## 
    ## Non-White     White 
    ##     18121     81327

## Join data (30 points)

Now we want to join data from the household roster (*Hetero8*) and the
data table with ethnicity (*Stable2*). First let us merge in the data on
ego’s ethnicity. We want to keep all the observations we have in
*Hetero8*, but we don’t want to add any other individuals from
*Stable2*.

``` r
JoinedEthn <- Hetero8 %>%
        left_join(Stable2, by = "pidp")
```

Let us rename the variables for ethnicity to clearly indicate that they
refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter’s ethnicity. Note that in this
case the key variables have different names in two data tables; please
refer to the documentation for your join function (or the relevant
section from R for Data Science) to check the solution for this problem.

``` r
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c("apidp" = "pidp"))
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

Let us start by looking at the joint distribution of race (White
vs. non-White) of both partners.

``` r
TableRace <- JoinedEthn %>%
        # filter out observations with missing data
        filter(!is.na(egoRace), !is.na(alterRace)) %>%
        count(egoRace, alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 Non-White Non-White  1790
    ## 2 Non-White White       326
    ## 3 White     Non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have
a White partner, 2) for a White woman to have a non-White partner, 3)
for a non-White woman to have a White partner, 4) for a non-White woman
to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the
code will not be reproducible: if the data change the code will need to
be changed, too. Your task is to write reproducible code producing a
table with the required four probabilities.

``` r
TableRace %>%
        # group by ego's race to calculate sums
        group_by(egoRace) %>%
        # create a new variable with the total number of women by race
        mutate(women_totals = sum(n)) %>%
        
        # create a new variable with the required probabilities 
          mutate(probabilities = n/women_totals)
```

    ## # A tibble: 4 x 5
    ## # Groups:   egoRace [2]
    ##   egoRace   alterRace     n women_totals probabilities
    ##   <chr>     <chr>     <int>        <int>         <dbl>
    ## 1 Non-White Non-White  1790         2116        0.846 
    ## 2 Non-White White       326         2116        0.154 
    ## 3 White     Non-White   266         9960        0.0267
    ## 4 White     White      9694         9960        0.973

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

1)  Join the individual-level file with the household-level data from
    wave 8 (specifically, we want the variable for the number of
    children in the household).
2)  Select only couples that are ethnically endogamous (i.e. partners
    come from the same ethnic group) for the following groups: White
    British, Indian, and Pakistani.
3)  Produce a table showing the mean and median number of children in
    these households by ethnic group (make sure the table has meaningful
    labels for ethnic groups, not just numerical codes).
4)  Write a short interpretation of your results. What could affect your
    findings?

<!-- end list -->

``` r
children <- Hh8 %>% select("h_hidp", "h_nkids_dv")
# making a database with only h_hidp and number of children

indi_house <- JoinedEthn %>%
        left_join(children, by = "h_hidp" )
# joining the children database with our ethn

indi_house <- indi_house %>% filter(egoRacel_dv == alterRacel_dv)
## removing results where partner races are not the same
indi_house <- indi_house %>% filter(egoRacel_dv == 1 | egoRacel_dv == 9 | egoRacel_dv == 10)
## removing results which are not White British, Pakistani or Indian 

indi_house <- indi_house %>% mutate(egoRacel_dv = recode(egoRacel_dv,
                                                    `1` = "White British",
                                                    `9` = "Indian",
                                                    `10`= "Pakistani"
                                                        ))

indi_house %>% group_by(egoRacel_dv) %>% summarise(mean(h_nkids_dv, na.rm = TRUE), median(h_nkids_dv, na.rm = TRUE))
```

    ## # A tibble: 3 x 3
    ##   egoRacel_dv   `mean(h_nkids_dv, na.rm = TRUE~ `median(h_nkids_dv, na.rm = TRU~
    ##   <chr>                                   <dbl>                            <dbl>
    ## 1 Indian                                  0.955                                1
    ## 2 Pakistani                               1.81                                 2
    ## 3 White British                           0.565                                0

## Findings

This data shows us that Pakistani couples have the highest number of
children on average when using both mean and median to measure. Indian
couples are next with a mean of just under one and a median of one.
White British couples have the lowest number of children with a mean of
0.56 children per couple and a median of 0. This means only every other
couple have a child.

The means and medians are quite close to each other for each ethnic
group which means there is not a lot of skew. There are a few caveats.
This is only for heterosexual ethnically endogenous couples. This means
there is no data for interacial couples or homosexual couples who decide
to adopt/ use a surrogate.
