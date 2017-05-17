Community Differences in Library Use and Services
================

Here I'll be detailing the analyses I performed to investigate how a library's services can differ depending on its surrounding community's demographic makeup as well as how different demographic communities might vary in their use of library resources. For this particular set of analyses, I'll be focusing on libraries located in cities; follow-up work will be done on libraries located in other locales (e.g., rural areas, suburbs, etc.).

To generate the approprate dataset needed for the following analyses, I used information spread across four datasets: (1) **The Public Library Survey** collected for the year 2012 that provides information about libraries' services, collections, etc.; (2) **US 2010 Decennial Census data**; (3) List of **zip codes and the corresponding Zip Code Tabulation Area (ZCTA)**; (4) List of **Zip Code Tabulation Areas (ZCTA) and their corresponding longitudinal and latitude information**. For more information on each of those datasets and how I created the dataset for the current analyses, please see the `Creating_dataset` R-markdown file.

In short, the dataset that I created involves the original Public Library Survey 2012 dataset combined with information about which is the majority race/ethnicity of the community surrounding each library. "Community" here is defined as any area that falls within a 2 mile radius of a library. This is not to say that libraries only service the population that is located within a 2 mile radius. However, a cut-off was needed and 2 miles did not appear to be completely unreasonable given that in cities, libraries are typically more clustered in comparison to libraries located in other locales (e.g., rural areas). Suggestions for what may be a better cut-off is welcomed!

First to load all the necessary packages:

``` r
library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(car)
library(dunn.test)
```

Next, the dataset we'll be using:

``` r
lib_2012s<-read.table("lib_2012_and_demo.txt", sep="\t", header=T)
```

Do the total number of annual visits and registered users differ by the demographic of a library's community?
=============================================================================================================

Here, we'll look at number of annual visits per capita (e.g., based on the population of legal service area).

``` r
lib_2012_visits<-lib_2012s %>%
  mutate(Visits_prop=VISITS/POPU_UND) %>%
  filter(Majority_demo!="NH_Asian") %>%
  mutate(Majority_demo=factor(Majority_demo, labels=c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic"))) %>%
  select(Visits_prop, Majority_demo)
```

We can then use a non-parametric test (i.e., Kruskal Wallis Test) to determine if libraries located in majority Hispanic, White, Non-Hispanic, and Black, Non-Hispanic communities differ in the total number of annual visits to the library. A non-parametric test is used rather than an one-way ANOVA because preliminary work revealed that some assumptions of ANOVA were violated here (e.g., variance of the demographic groups differed significantly based on Levene's Test).

``` r
kruskal.test(Visits_prop~Majority_demo, lib_2012_visits)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Visits_prop by Majority_demo
    ## Kruskal-Wallis chi-squared = 49.733, df = 2, p-value = 1.587e-11

``` r
with(lib_2012_visits, dunn.test(Visits_prop, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                   Comparison of Visits_prop by Majority_demo                   
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   6.810700
    ##          |     0.0000
    ##          |
    ## White, N |   2.857848  -2.918644
    ##          |     0.0064     0.0053

From this, we can see that the three communities differed significantly from each other, with libraries in White, Non-Hispanic majority communities having the highest mean rank of annual visits per capita (*M* = 270.31), followed by libraries in Black, Non-Hispanic majority communities (*M* = 215.03), and finally libraries in Hispanic majority communities (*M* = 142.76).

We can then look at the number of annual registered users per capita:

``` r
lib_2012_reg<-lib_2012s %>%
  mutate(Reg_prop=REGBOR/POPU_UND) %>%
  filter(Majority_demo!="NH_Asian") %>%
  mutate(Majority_demo=factor(Majority_demo, labels=c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic"))) %>%
  select(Reg_prop, Majority_demo)
```

Again, using the Kruskal Wallis Test, we find that the communities differ signficantly in their number of annual registered users per capita. However, a pattern slightly different from the annual visits emerged here. Libraries in communities with a Hispanic majority (*M* = 181.37) differed significantly from libraries in communities with a White majority (*M* = 260.80) in their mean rank of annual registered users per capita. However, libaries in Hispanic and Black majority communities (*M* = 227.54) did not differ significantly; Libaries in Black majority communities also did not differ significantly from those in White majority communities.

``` r
kruskal.test(Reg_prop~Majority_demo, lib_2012_reg)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Reg_prop by Majority_demo
    ## Kruskal-Wallis chi-squared = 19.155, df = 2, p-value = 6.928e-05

``` r
with(lib_2012_reg, dunn.test(Reg_prop, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                     Comparison of Reg_prop by Majority_demo                    
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   4.241641
    ##          |     0.0000
    ##          |
    ## White, N |   1.719618  -1.864747
    ##          |     0.1283     0.0933

We can also graph both annual visits and annual registered users: <img src="Demographic_Differences_Across_Library_Use_Services_files/figure-markdown_github/Annual Visits and Registered Users Graph-1.png" style="display: block; margin: auto;" />

Does the library's collection vary by the demographic of a library's community?
===============================================================================

Additionally, given the rise of digital content, a question of interest might be whether libraries provide similar amount of digital content across communities?

To account for different sizes of the libraries, we'll look at the size of different collection types per capita.

``` r
lib_2012_collection<-lib_2012s %>%
  select(POPU_UND, BKVOL, EBOOK, AUDIO_PH, AUDIO_DL, VIDEO_PH, VIDEO_DL, DATABASE, Majority_demo) %>%
  mutate_each(funs(replace(., .<0, NA))) %>%
  filter(Majority_demo!="NH_Asian") %>%
  mutate(Majority_demo=factor(Majority_demo, labels=c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic"))) %>%
  mutate(Books=BKVOL/POPU_UND, Ebooks=EBOOK/POPU_UND,
         Audio_PH=AUDIO_PH/POPU_UND, Audio_DL=AUDIO_DL/POPU_UND,
         Video_PH=VIDEO_PH/POPU_UND, Video_DL=VIDEO_DL/POPU_UND,
         Database=DATABASE/POPU_UND) %>%
  select(Books, Ebooks, Audio_PH, Audio_DL,  Video_PH, Video_DL, Database, Majority_demo)
```

Given our dataset and questions, a MANOVA might be the statistical test to use. However, violations of some assumptions (e.g., lack of linear relationship between DVs within each IVs, unequal variances, etc.), performing a MANOVA is not actually appropriate here. Thus, I ended up running multiple Kruskal-Wallis tests instead. However, instead of running six individual tests, I collapsed Audio and Video (physical) collections into one category and Audio and Video (digital) collections into another category. Additionally, because I ran four Kruskal-Wallis tests, I used the adjusted alpha of 0.0125.

``` r
kruskal.test(Books~Majority_demo, lib_2012_collection)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Books by Majority_demo
    ## Kruskal-Wallis chi-squared = 50.883, df = 2, p-value = 8.93e-12

``` r
with(lib_2012_collection, dunn.test(Books, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                      Comparison of Books by Majority_demo                      
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   7.130240
    ##          |     0.0000
    ##          |
    ## White, N |   0.887732  -4.716083
    ##          |     0.5620     0.0000

``` r
kruskal.test(Ebooks~Majority_demo, lib_2012_collection)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Ebooks by Majority_demo
    ## Kruskal-Wallis chi-squared = 34.679, df = 2, p-value = 2.948e-08

``` r
with(lib_2012_collection, dunn.test(Ebooks, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                      Comparison of Ebooks by Majority_demo                     
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   5.448579
    ##          |     0.0000
    ##          |
    ## White, N |   3.047696  -1.740156
    ##          |     0.0035     0.1227

``` r
kruskal.test((Audio_PH+Video_PH)~Majority_demo, lib_2012_collection)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  (Audio_PH + Video_PH) by Majority_demo
    ## Kruskal-Wallis chi-squared = 56.325, df = 2, p-value = 5.878e-13

``` r
with(lib_2012_collection, dunn.test((Audio_PH+Video_PH), Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                        Comparison of x by Majority_demo                        
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   7.170600
    ##          |     0.0000
    ##          |
    ## White, N |   3.287824  -2.878578
    ##          |     0.0015     0.0060

``` r
kruskal.test((Audio_DL+Video_DL)~Majority_demo, lib_2012_collection)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  (Audio_DL + Video_DL) by Majority_demo
    ## Kruskal-Wallis chi-squared = 36.283, df = 2, p-value = 1.322e-08

``` r
with(lib_2012_collection, dunn.test((Audio_DL+Video_DL), Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                        Comparison of x by Majority_demo                        
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   5.252381
    ##          |     0.0000
    ##          |
    ## White, N |   3.708689  -1.136040
    ##          |     0.0003     0.3839

With physical print materials, libraries in communities with majority Hispanic members (*M* = 136.76) had a significantly lower mean rank of physical print materials compared to libraries in communities with majority Black (*M* = 248.44) and with majority White (*M* = 265.58) members.

With Ebooks, libraries in communities with majority White members (*M* = 267.23) had significantly higher mean rank of physical print materials compared to libraries in communities with majority Black (*M* = 208.31) and with majority Hispanic (*M* = 165.24) members.

With physical audio and video content, communities differed significantly from each other. Libraries in communities with majority White members (*M* = 271.65) had the highest mean rank of such content, followed by libraries in communities with majority Black members (*M* = 208.19), then libraries in communities with majority Hispanic members (*M* = 142.00).

With digital audio and video content, libraries in communities with majority White members (*M* = 264.53) had a sigificantly higher mean rank of such content than libraries in communities with majority Black (*M* = 200.14) and with majority Hispanic members (*M* = 187.04).

We can also graph this: <img src="Demographic_Differences_Across_Library_Use_Services_files/figure-markdown_github/Collections graph-1.png" style="display: block; margin: auto;" />

Do the library services used by members of a community differ depending on its demographics?
============================================================================================

For this, we'll look at (1) total annual circulation transactions, (2) total audience at programs, (3) total reference transactions, and (4) total uses of public internet computers. To take into account the differing sizes of libraries, we'll look at these variables per the total annual number of visits.

``` r
lib_2012_services<-lib_2012s %>%
  mutate(Cir=TOTCIR/VISITS, Prog_attend=TOTATTEN/VISITS, Reference=REFERENC/VISITS, Pub_Internet=PITUSR/VISITS) %>%
  filter(Majority_demo!="NH_Asian") %>%
  mutate(Majority_demo=factor(Majority_demo, labels=c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic"))) %>%
  select(Majority_demo, Cir, Prog_attend, Reference, Pub_Internet) 

#to compare total annual circulation transactions across communities
kruskal.test(Cir~Majority_demo, lib_2012_services)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Cir by Majority_demo
    ## Kruskal-Wallis chi-squared = 49.505, df = 2, p-value = 1.779e-11

``` r
with(lib_2012_services, dunn.test(Cir, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                       Comparison of Cir by Majority_demo                       
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |   5.905124
    ##          |     0.0000
    ##          |
    ## White, N |   4.690266  -0.802374
    ##          |     0.0000     0.6335

``` r
#to compare total program attendence across communities
kruskal.test(Prog_attend~Majority_demo, lib_2012_services)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Prog_attend by Majority_demo
    ## Kruskal-Wallis chi-squared = 2.8107, df = 2, p-value = 0.2453

``` r
#to compare total reference transactions across communities
kruskal.test(Reference~Majority_demo, lib_2012_services)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Reference by Majority_demo
    ## Kruskal-Wallis chi-squared = 23.368, df = 2, p-value = 8.428e-06

``` r
with(lib_2012_services, dunn.test(Reference, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                    Comparison of Reference by Majority_demo                    
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |  -1.635520
    ##          |     0.1529
    ##          |
    ## White, N |  -4.746735  -2.470890
    ##          |     0.0000     0.0202

``` r
#to compare total uses of public internet computers across communities
kruskal.test(Pub_Internet~Majority_demo, lib_2012_services)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  Pub_Internet by Majority_demo
    ## Kruskal-Wallis chi-squared = 31.91, df = 2, p-value = 1.177e-07

``` r
with(lib_2012_services, dunn.test(Pub_Internet, Majority_demo, kw=F, method="bonferroni"))
```

    ## 
    ##                   Comparison of Pub_Internet by Majority_demo                  
    ##                                  (Bonferroni)                                  
    ## Col Mean-|
    ## Row Mean |   Black, N   Hispanic
    ## ---------+----------------------
    ## Hispanic |  -4.132601
    ##          |     0.0001
    ##          |
    ## White, N |  -4.442389  -0.344580
    ##          |     0.0000     1.0000

Libraries located in communities with a majority of White members had a higher mean rank in the number of circulation transactions (*M* = 272.51) than those located in communities with a majority of Black members (*M* = 181.79) and Hispanic members (*M* = 161.93). With regards to the number of reference transactions, libraries located in communities with majority Black members (*M* = 321.25) had higher mean rank than those located in communities with a majority of White (*M* = 229.45) and Hispanic (*M* = 260.07) members. With regards to use of public internet computers, communities with a majority of Hispanic (*M* = 301.10) and Black (*M* = 309.63) had higher mean ranks than those located in communities with a majority of White memberes (*M* = 223.71). Fially, program attendence did not differ across the three different communities.

To graph this: <img src="Demographic_Differences_Across_Library_Use_Services_files/figure-markdown_github/Services graph-1.png" style="display: block; margin: auto;" />
