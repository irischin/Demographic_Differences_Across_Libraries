Creating our dataset
================

To create the dataset needed for the main analyses, I used information extracted from four datasets: 1. **The Public Library Survey** collected for the year 2012. The survey collects data from more than 9,000 libraries across the United States. Examples of information provided in the survey include number of annual visits, total number of programs provided, size of collections, etc. You can read more about it [here](https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey/explore-pls-data/about). The year 2012 was selected as this analysis was largely inspired by Pew Research Report, [*Library Services in the Digital Age*](http://libraries.pewinternet.org/2013/01/22/library-services/) and the dataset used in that particular report was collected between Oct.-Nov. 2012. (Note: the dataset that Pew Research Center used, which surveyed individual people, differs from the current dataset, which examines the libraries themselves.)

1.  **US 2010 Decennial Census data** to obtain demographic information

2.  List of **zip codes and the corresponding Zip Code Tabulation Area (ZCTA)** they fall in. ZCTA are aggregated zip-code like entities created by the United States Census Bureau (for more information, see [here](https://www.census.gov/geo/reference/zctas.html)). The US 2010 Census provides demographic data based on ZCTAs rather than zip codes. Thus, to be able to match a library's location (using its zip code location) to the US 2010 Census data, I needed information regarding which zip codes fell into which ZCTA. The data was retrieved from the [Missouri Census Data Center](http://mcdc.missouri.edu/allabout/zipcodes_2010supplement.shtml).

3.  List of **Zip Code Tabulation Areas (ZCTA) and their corresponding longitudinal and latitude information**. To find demographic information of areas *surrounding* a particular library, again I used the US Census data, aggregated by ZCTAs. I needed to determine which ZCTAs fell within a specified radius of a library (here, I'm using a radius area of 2 miles from a library). The ZCTAs' longitudinal and latitude information was compared to that of the libraries' to determine which ZCTA should be included and deemed as part of the "surrounding" area of a library. The longitudinal/latitude information was gathered from the [US Census Bureau](https://www.census.gov/geo/maps-data/data/gazetteer2010.html). Note: here I'm using the radius cut-off of 2 miles since I'm focusing on libraries in cities, which are generally more closely clustered than libraries in other locales (e.g., rural areas). This is not to say that libraries only service the population that is located within a 2 mile radius. However, a cut-off was needed and 2 miles did not appear to be completely unreasonable.

First to load packages that we'll be using:

``` r
library(geosphere)
```

    ## Loading required package: sp

Now we can import the four different datasets we'll be using. First is the Public Library Survey for 2012. I've also subsetted the data so that it includes only libraries that are located in a city. This is indicated under the `LOCALE` column, with values 11-13 corresponding to a city.

``` r
#The Public Library Survey for 2012
lib_2012<-read.csv("Pupld12a.csv", colClasses = c("ZIP"="character"))

#Subsetting only libraries that are located in a city
lib_2012s<-subset(lib_2012, LOCALE<=13)
head(lib_2012s)
```

    ##     STABR FSCSKEY      LIBID                                     LIBNAME
    ## 2      AK  AK0002 AK0002-011                    ANCHORAGE PUBLIC LIBRARY
    ## 15     AK  AK0023 AK0023-002 FAIRBANKS NORTH STAR BOROUGH PUBLIC LIBRARY
    ## 103    AL  AL0019    101-009                       OXFORD PUBLIC LIBRARY
    ## 132    AL  AL0052    104-004                       AUBURN PUBLIC LIBRARY
    ## 133    AL  AL0053    104-005 OPELIKA - LEWIS COOPER JR. MEMORIAL LIBRARY
    ## 138    AL  AL0059    516-001        FLORENCE - LAUDERDALE PUBLIC LIBRARY
    ##                   ADDRESS      CITY   ZIP ZIP4            ADDRES_M
    ## 2      3600 DENALI STREET ANCHORAGE 99503 6055  3600 DENALI STREET
    ## 15     1215 COWLES STREET FAIRBANKS 99701 4313  1215 COWLES STREET
    ## 103   110 EAST 6TH STREET    OXFORD 36203 1628 110 EAST 6TH STREET
    ## 132 749 EAST THACH AVENUE    AUBURN 36830 5545   749 E. THACH AVE.
    ## 133  200 SOUTH 6TH STREET   OPELIKA 36801 5030        P.O. BOX 125
    ## 138 350 NORTH WOOD AVENUE  FLORENCE 35630 4709    350 N. WOOD AVE.
    ##        CITY_M ZIP_M ZIP4_M                 CNTY      PHONE C_RELATN
    ## 2   ANCHORAGE 99503   6055            ANCHORAGE 9073432982       NO
    ## 15  FAIRBANKS 99701   4313 FAIRBANKS NORTH STAR 9074591022       NO
    ## 103    OXFORD 36203   1628              CALHOUN 2568311750       ME
    ## 132    AUBURN 36830   5545                  LEE 3345013190       ME
    ## 133   OPELIKA 36803   0125                  LEE 3347055380       ME
    ## 138  FLORENCE 35630   4709           LAUDERDALE 2567646564       ME
    ##     C_LEGBAS C_ADMIN C_FSCS GEOCODE LSABOUND   STARTDAT F_STDAT    ENDDATE
    ## 2         CO      MO      Y     MA1        N 01/01/2011    R_12 12/31/2011
    ## 15        CO      MO      Y     CO1        N 07/01/2011    R_12 06/30/2012
    ## 103       CI      SO      Y     CI1        N 10/01/2011    R_12 09/30/2012
    ## 132       CI      SO      Y     CI1        N 10/01/2011    R_12 09/30/2012
    ## 133       CI      SO      Y     CI1        N 10/01/2011    R_12 09/30/2012
    ## 138       MJ      SO      Y     CO2        N 10/01/2011    R_12 09/30/2012
    ##     F_ENDDAT POPU_LSA F_POPLSA POPU_UND F_POPUND CENTLIB F_CENLIB BRANLIB
    ## 2       R_12   298842     R_12   298842     R_12       1     R_12       4
    ## 15      R_12   100343     R_12   100343     R_12       1     R_12       1
    ## 103     R_12    21348     R_12    21348     R_12       1     R_12       0
    ## 132     R_12    53380     R_12    53380     R_12       1     R_12       0
    ## 133     R_12    26477     R_12    26477     R_12       1     R_12       0
    ## 138     R_12    89609     R_12    89609     R_12       1     R_12       0
    ##     F_BRLIB BKMOB F_BKMOB MASTER F_MASTER LIBRARIA F_LIBRAR OTHPAID
    ## 2      R_12     0    R_12  22.98     R_12    27.73     R_12   51.33
    ## 15     R_12     1    R_12  12.00     R_12    13.00     R_12   35.85
    ## 103    R_12     0    R_12   1.00     R_12     1.00     R_12    7.00
    ## 132    R_12     0    R_12   5.00     R_12     7.00     R_12   13.00
    ## 133    R_12     0    R_12   2.00     R_12     4.00     R_12    8.50
    ## 138    R_12     0    R_12   3.50     R_12    16.00     R_12    5.00
    ##     F_OTHSTF TOTSTAFF F_TOTSTF  LOCGVT F_LOCGVT STGVT F_STGVT FEDGVT
    ## 2       R_12    79.06     R_12 9570884     R_12 51268    R_12 109425
    ## 15      R_12    48.85     R_12 5559210     R_12 21324    R_12 102425
    ## 103     R_12     8.00     R_12  434000     R_12 18556    R_12      0
    ## 132     R_12    20.00     R_12 1515127     R_12 22336    R_12  11000
    ## 133     R_12    12.50     R_12  538711     R_12 25933    R_12   2436
    ## 138     R_12    21.00     R_12  910450     R_12 65064    R_12  15000
    ##     F_FEDGVT OTHINCM F_OTHINC TOTINCM F_TOTINC SALARIES F_SALX BENEFIT
    ## 2       R_12  263659     R_12 9995236     R_12  3657988   R_12 2621241
    ## 15      R_12  144941     R_12 5827900     R_12  2842706   R_12 1782945
    ## 103     R_12       0     R_12  452556     R_12   210000   R_12       0
    ## 132     R_12       0     R_12 1548463     R_12   955961   R_12  236249
    ## 133     R_12   36350     R_12  603430     R_12   381390   R_12  110921
    ## 138     R_12  149364     R_12 1139878     R_12   463232   R_12  143560
    ##     F_BENX STAFFEXP F_TOSTFX PRMATEXP F_PRMATX ELMATEXP F_ELMATX OTHMATEX
    ## 2     R_12  6279229     R_12   525401     R_12   169568     R_12   184851
    ## 15    R_12  4625651     R_12   323708     R_12   117278     R_12   199817
    ## 103   R_12   210000     R_12    27059     R_12        0     R_12     4224
    ## 132   R_12  1192210     R_12    88395     R_12    30496     R_12     6197
    ## 133   R_12   492311     R_12    41133     R_12     1623     R_12    10986
    ## 138   R_12   606792     R_12    59354     R_12     5930     R_12    20188
    ##     F_OTMATX TOTEXPCO F_TOCOLX OTHOPEXP F_OTHOPX TOTOPEXP F_TOTOPX
    ## 2       R_12   879820     R_12  3193524     R_12 10352573     R_12
    ## 15      R_12   640803     R_12   543271     R_12  5809725     R_12
    ## 103     R_12    31283     R_12    38397     R_12   279680     R_12
    ## 132     R_12   125088     R_12   231165     R_12  1548463     R_12
    ## 133     R_12    53742     R_12   141716     R_12   687769     R_12
    ## 138     R_12    85472     R_12   444834     R_12  1137098     R_12
    ##     LCAP_REV F_LCAPRV SCAP_REV F_SCAPRV FCAP_REV F_FCAPRV OCAP_REV
    ## 2          0     R_12  1956000     R_12        0     R_12        0
    ## 15         0     R_12        0     R_12        0     R_12  8713437
    ## 103        0     R_12        0     R_12        0     R_12        0
    ## 132        0     R_12        0     R_12        0     R_12        0
    ## 133        0     R_12        0     R_12        0     R_12        0
    ## 138        0     R_12        0     R_12        0     R_12        0
    ##     F_OCAPRV CAP_REV F_TCAPRV CAPITAL F_TCAPX  BKVOL F_BKVOL EBOOK F_EBOOK
    ## 2       R_12 1956000     R_12       0    R_12 509906    R_12 54298    R_12
    ## 15      R_12 8713437     R_12 1082970    R_12 358168    R_12 11967    R_12
    ## 103     R_12       0     R_12       0    R_12  51576    R_12     0    R_12
    ## 132     R_12       0     R_12       0    R_12  71753    R_12  1330    R_12
    ## 133     R_12       0     R_12       0    R_12  70639    R_12   156    R_12
    ## 138     R_12       0     R_12       0    R_12  74805    R_12 41586    R_12
    ##     AUDIO_PH F_AUD_PH AUDIO_DL F_AUD_DL VIDEO_PH F_VID_PH VIDEO_DL
    ## 2      30791     R_12     7871     R_12    59076     R_12        0
    ## 15     20398     R_12     8709     R_12    28654     R_12        0
    ## 103     2964     R_12        0     R_12     2243     R_12        0
    ## 132     3202     R_12     1638     R_12     3197     R_12        0
    ## 133     3945     R_12       79     R_12     3285     R_12        0
    ## 138     3657     R_12      380     R_12     4516     R_12        4
    ##     F_VID_DL DB_LO_OT F_DB_L_O DB_ST F_DB_ST DATABASE F_DBASE SUBSCRIP
    ## 2       R_12       17     R_12    49    R_12       66    R_12      880
    ## 15      R_12       27     R_12    49    R_12       76    R_12     1038
    ## 103     R_12        0     R_12    70    R_12       70    R_12       32
    ## 132     R_12        5     R_12    70    R_12       75    R_12      159
    ## 133     R_12        2     R_12    70    R_12       72    R_12       88
    ## 138     R_12        3     R_12    70    R_12       73    R_12       87
    ##     F_PRSUB HRS_OPEN F_HRS_OP VISITS F_VISITS REFERENC F_REFER REGBOR
    ## 2      R_12    10608     R_12 739884     R_12   145964    R_12 202827
    ## 15     R_12     7124     R_12 364424     R_12   125009    R_12  56352
    ## 103    R_12     2236     R_12  28000     R_12     1106    R_12  17751
    ## 132    R_12     3411     R_12 217062     R_12    16889    R_12  40349
    ## 133    R_12     3500     R_12 125160     R_12     5059    R_12  17250
    ## 138    R_12     2754     R_12 223228     R_12    80422    R_12  43905
    ##     F_REGBOR  TOTCIR F_TOTCIR KIDCIRCL F_KIDCIR LOANTO F_LOANTO LOANFM
    ## 2       R_12 1609402     R_12   650437     R_12   6092     R_12   1153
    ## 15      R_12  677826     R_12   274523     ID12   6640     R_12   3272
    ## 103     R_12   87093     R_12    32604     R_12     19     R_12     72
    ## 132     R_12  240787     R_12   108477     R_12    334     R_12    248
    ## 133     R_12  153385     R_12    54950     R_12     74     R_12    183
    ## 138     R_12  239530     R_12    82351     R_12      0     R_12     50
    ##     F_LOANFM TOTPRO F_TOTPRO KIDPRO F_KIDPRO YAPRO F_YAPRO TOTATTEN
    ## 2       R_12   1005     R_12    712     R_12   159    R_12    51452
    ## 15      R_12    952     R_12    807     R_12    17    R_12    36031
    ## 103     R_12    188     R_12     50     R_12     0    R_12     3593
    ## 132     R_12    392     R_12    362     R_12    21    R_12    13738
    ## 133     R_12     95     R_12     72     R_12     7    R_12     2957
    ## 138     R_12    573     R_12    274     R_12    13    R_12    15528
    ##     F_TOTATT KIDATTEN F_KIDATT YAATTEN F_YAATT GPTERMS F_GPTERM PITUSR
    ## 2       R_12    33222     R_12    3521    R_12     130     R_12 137135
    ## 15      R_12    31708     R_12     933    R_12      27     R_12 178415
    ## 103     R_12     2461     R_12       0    R_12      53     R_12  20862
    ## 132     R_12    13205     R_12     482    R_12      81     R_12 121578
    ## 133     R_12     2628     R_12      75    R_12      21     R_12  33997
    ## 138     R_12     9138     R_12      72    R_12      43     R_12  41866
    ##     F_PITUSR YR_SUB OBEREG RSTATUS STATSTRU STATNAME STATADDR   LONGITUD
    ## 2       R_12   2013      8       1        0        0        0 -149.87678
    ## 15      R_12   2013      8       2        0        0        0 -147.73862
    ## 103     R_12   2013      5       1        0        0        0  -85.83552
    ## 132     R_12   2013      5       1        0        0        0  -85.46636
    ## 133     R_12   2013      5       1        0        0        0  -85.37595
    ## 138     R_12   2013      5       1        0        0        0  -87.67465
    ##     LATITUDE FIPSST FIPSCO FIPSPLAC CNTYPOP LOCALE CENTRACT CENBLOCK
    ## 2   61.18768      2     20     3000  298294     11    19.00     3002
    ## 15  64.83832      2     90    24230  100141     13     1.00     2027
    ## 103 33.60972      1     15    57576  117253     13    12.01     2072
    ## 132 32.60441      1     81     3076  147808     13   403.00     2000
    ## 133 32.64914      1     81    57048  147808     13   416.00     2035
    ## 138 34.80356      1     77    26896   92682     13   106.00     4013
    ##     CDCODE  CBSA MICROF          GAL GALMS POSTMS
    ## 2      200 11260      0 addresspoint   STD    NND
    ## 15     200 21820      0 addresspoint   STD    NND
    ## 103    103 11500      0        house   STD    NND
    ## 132    103 12220      0        house   STD    NND
    ## 133    103 12220      0        house   STD    NND
    ## 138    105 22520      0        house   STD    NND

Then, we can import the US 2010 Decennial Census data. I've excluded the second row because it simply contains more detailed column ID information. Additionally, I've excluded columns that included additional demographic divisions that I won't be using for my analyses (e.g., population information of those identifying as three or more races/ethnicities).

``` r
zcta_demo<-read.csv("DEC_10_SF1_P9.csv")
zcta_demo<-zcta_demo[-1,1:14]
zcta_demo<-data.frame(zcta_demo[,1:3], sapply(zcta_demo[,4:14], function(x) as.numeric(as.character(x))))
names(zcta_demo)[4:14]<-c("Total", "Hispanic_Latino", "Not_Hispanic", "NH_One_total", "NH_White", "NH_Black_or_AA", "NH_AI_and_AN", "NH_Asian", "NH_NH_and_Other_PI", "NH_Other", "NH_Two_or_more")
head(zcta_demo)
```

    ##           GEO.id GEO.id2 GEO.display.label Total Hispanic_Latino
    ## 2 8600000US00601   00601       ZCTA5 00601 18570           18486
    ## 3 8600000US00602   00602       ZCTA5 00602 41520           41265
    ## 4 8600000US00603   00603       ZCTA5 00603 54689           53877
    ## 5 8600000US00606   00606       ZCTA5 00606  6615            6575
    ## 6 8600000US00610   00610       ZCTA5 00610 29016           28789
    ## 7 8600000US00612   00612       ZCTA5 00612 67010           66435
    ##   Not_Hispanic NH_One_total NH_White NH_Black_or_AA NH_AI_and_AN NH_Asian
    ## 2           84           84       80              2            1        1
    ## 3          255          248      216             13            0       15
    ## 4          812          790      628            101            2       48
    ## 5           40           39       32              3            0        3
    ## 6          227          222      187             22            0        8
    ## 7          575          551      439             45            5       47
    ##   NH_NH_and_Other_PI NH_Other NH_Two_or_more
    ## 2                  0        0              0
    ## 3                  0        4              7
    ## 4                  2        9             22
    ## 5                  1        0              1
    ## 6                  0        5              5
    ## 7                  1       14             24

Next, we can import the dataset that provides information on which ZCTA a particular zip code falls under.

``` r
zip_to_zcta<-read.table("Zip_to_ZCTA_crosswalk_2010_JSI.csv", sep=",", header=T, colClasses = c("ZIP"="character", "ZCTA_USE....from.http...udsmapper.org.docs.Zip_to_ZCTA_crosswalk_2010_JSI.xls.5.23.2012.10.02.13.AM.."="character"))
names(zip_to_zcta)[5]<-"ZCTA_USE"
head(zip_to_zcta)
```

    ##     ZIP ZIPType   CityName StateAbbr ZCTA_USE
    ## 1 00501       U Holtsville        NY    11742
    ## 2 00544       U Holtsville        NY    11742
    ## 3 00601       S   Adjuntas        PR    00601
    ## 4 00602       S     Aguada        PR    00602
    ## 5 00603       S  Aguadilla        PR    00603
    ## 6 00604       P  Aguadilla        PR    00603

Finally, we can import the data file containing longitude and latitude information for each ZCTA.

``` r
zcta_geoid<-read.table("Gaz_zcta_national.txt", header=T, sep="\t", colClasses = c("GEOID"="character") )
head(zcta_geoid)
```

    ##   GEOID POP10  HU10     ALAND  AWATER ALAND_SQMI AWATER_SQMI INTPTLAT
    ## 1 00601 18570  7744 166659789  799296     64.348       0.309 18.18055
    ## 2 00602 41520 18073  79288158 4446273     30.613       1.717 18.36227
    ## 3 00603 54689 25653  81880442  183425     31.614       0.071 18.45518
    ## 4 00606  6615  2877 109580061   12487     42.309       0.005 18.15835
    ## 5 00610 29016 12618  93021467 4172001     35.916       1.611 18.29096
    ## 6 00612 67010 30992 175106243 9809163     67.609       3.787 18.40224
    ##   INTPTLONG
    ## 1 -66.74996
    ## 2 -67.17613
    ## 3 -67.11989
    ## 4 -66.93291
    ## 5 -67.12587
    ## 6 -66.71140

Determining demographic majority of the communities around libraries
====================================================================

One of the first things to do is to determine which ZCTAs are located within 2 miles of a library based on its longitudinal and latitude information. You'll notice that in the for-loop, I've also added code such that it will print the progress of the loop (e.g., for every 10% of the loop that is completed).

``` r
lib_zcta_around<-data.frame()

for (i in 1:length(lib_2012s$LIBID)) {
  lib_zcta_around<-rbind(lib_zcta_around, distm(lib_2012s[i, c("LONGITUD", "LATITUDE")], zcta_geoid[,c("INTPTLONG", "INTPTLAT")]))
  if( ( (i/length(lib_2012s$LIBID))*100)%%10==0 ){
    print( (i/length(lib_2012s$LIBID))*100 )
  }
}
```

    ## [1] 10
    ## [1] 20
    ## [1] 30
    ## [1] 40
    ## [1] 50
    ## [1] 60
    ## [1] 70
    ## [1] 80
    ## [1] 90
    ## [1] 100

``` r
#Now to convert meters to miles
lib_zcta_around_mi<-lib_zcta_around*0.000621371

#To get only ZCTAs that are within 2 miles of a library
lib_zcta_around_sub<-apply(lib_zcta_around_mi, 1, function(x) which(x<=2))
```

Now using the ZCTAs, we can determine the demographic makeup of the communities surrounding each library.

``` r
lib_demo_sums_complete<-data.frame()

for (i in 1:length(lib_zcta_around_sub)) {
  lib_ZCTA<-subset(zip_to_zcta, ZIP==lib_2012s[i, "ZIP"])$ZCTA_USE #gives us the lib ZCTA
  geoid_indices<-c(lib_ZCTA, zcta_geoid[lib_zcta_around_sub[[i]], ]$GEOID) #gives us list of ZCTA of lib + areas within 2 miles of lib
  lib_demo<-subset(zcta_demo, GEO.id2 %in% geoid_indices) #gather corresponding demo info for those ZCTA areas
  lib_demo_sums<-colSums(lib_demo[,4:14]) #get total for each demo groups across the ZCTA areas
  lib_demo_sums_complete<-rbind(lib_demo_sums_complete, lib_demo_sums)
}

names(lib_demo_sums_complete)<-names(zcta_demo)[4:14]

head(lib_demo_sums_complete)
```

    ##   Total Hispanic_Latino Not_Hispanic NH_One_total NH_White NH_Black_or_AA
    ## 1 24788            2453        22335        20421    13245           1223
    ## 2 19019            1317        17702        16333    11692           1436
    ## 3 18799            1466        17333        17122    14294           2539
    ## 4 36351            1168        35183        34677    27156           5249
    ## 5 22214            1080        21134        20921    10726           9758
    ## 6 32629            1300        31329        30747    23014           7110
    ##   NH_AI_and_AN NH_Asian NH_NH_and_Other_PI NH_Other NH_Two_or_more
    ## 1         2548     2655                715       35           1914
    ## 2         2396      678                 93       38           1369
    ## 3           58      204                  7       20            211
    ## 4           90     2113                 11       58            506
    ## 5           47      358                 12       20            213
    ## 6          124      465                 13       21            582

This will then allow us to determine what is the majority race/ethic group of the communities surrounding each library.

``` r
lib_demo_sums_comp_prop<-sweep(lib_demo_sums_complete[,c(2,5:11)], 1, lib_demo_sums_complete$Total, "/")

majority_demo<-factor(apply(lib_demo_sums_comp_prop, 1, which.max))

levels(majority_demo)<-list("Hispanic_Latino"=1, "NH_White"=2, "NH_Black_or_AA"=3, "NH_Asian"=5)

table(majority_demo)
```

    ## majority_demo
    ## Hispanic_Latino        NH_White  NH_Black_or_AA        NH_Asian 
    ##              75             359              63               3

As we can see, there's only three libraries in which the majority race/ethnic group surrounding the area is Asian. Given that there are so few, it may be better for now to exclude them from future analyses (three libraries may be too few to be able to draw generalizable conclusions).

Next, we can combine this demographic information with our Public Library Survey dataset and further subset the data so that we exclude libraries that don't have information on the population of legal service or number of annual visits (as those pieces of information will be important factors to consider for our analyses).

``` r
lib_2012s$Majority_demo<-majority_demo

lib_2012s<-subset(lib_2012s, !(POPU_UND==-1|VISITS==-1))
```

We can then export this dataframe.

``` r
write.table(lib_2012s, file="lib_2012_and_demo.txt", sep="\t", row.names=F, col.names=T)
```
