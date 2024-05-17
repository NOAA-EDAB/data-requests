Workflow notes
================
Sarah Gaichas
2024-05-17

# Preliminary analysis steps

1.  What did Sean do? Find old scripts (best guess is
    `Andre_data_pulls_v2.R` in 2014request folder)
2.  Compare inputs, 2014 `survdat_lw.RData` which did not use current
    `survdat` package vs today’s `survdat_lw.rds`
3.  If inputs look similar, run old script and see how far we get

## 1. What did Sean do?

<details>
<summary>
Old script (we think, there was also one called `Andre_data_pulls.R` in
a different folder)
</summary>
</details>

## 2. Compare inputs

<details>
<summary>
Read in 2014 and 2024 survdat_lw files.
</summary>

``` r
#Grab survdat.r (object is called survdat.lw)
load(file.path(here::here('EwE-menhaden-AndreBuchheister/2014request/survdat_lw.RData')))

survdat24 <- readRDS(here::here('EwE-menhaden-AndreBuchheister/survdat_lw.rds'))

survdat.lw24 <- survdat24$survdat
```

</details>
<details>
<summary>
Column names, types, and number of rows, columns in `survdat.lw` from
2014:
</summary>

``` r
cat("Names\n")
```

    ## Names

``` r
names(survdat.lw)
```

    ##  [1] "SVSPP"       "CATCHSEX"    "CRUISE6"     "STATION"     "STRATUM"    
    ##  [6] "TOW"         "SVVESSEL"    "YEAR"        "SEASON"      "LAT"        
    ## [11] "LON"         "EST_TOWDATE" "DEPTH"       "SURFTEMP"    "SURFSALIN"  
    ## [16] "BOTTEMP"     "BOTSALIN"    "ABUNDANCE"   "BIOMASS"     "LENGTH"     
    ## [21] "NUMLEN"      "INDWT"       "WGTLEN"      "SIZECAT"

``` r
str(survdat.lw)
```

    ## Classes 'data.table' and 'data.frame':   2892498 obs. of  24 variables:
    ##  $ SVSPP      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CATCHSEX   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CRUISE6    : int  196721 196817 196817 196817 196817 196817 196817 196817 196817 196817 ...
    ##  $ STATION    : int  9 8 8 8 38 41 41 43 46 50 ...
    ##  $ STRATUM    : int  1080 1080 1080 1080 1720 1720 1720 1700 1680 1680 ...
    ##  $ TOW        : int  2 1 1 1 2 1 1 1 2 1 ...
    ##  $ SVVESSEL   : Factor w/ 12 levels "AL","AT","BG",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ YEAR       : int  1967 1968 1968 1968 1968 1968 1968 1968 1968 1968 ...
    ##  $ SEASON     : Factor w/ 2 levels "FALL","SPRING": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LAT        : num  40 40 40 40 38 ...
    ##  $ LON        : num  -71.2 -70.6 -70.6 -70.6 -73.9 ...
    ##  $ EST_TOWDATE: POSIXct, format: "1967-10-18 14:47:00" "1968-10-11 06:19:00" ...
    ##  $ DEPTH      : int  308 325 325 325 293 325 325 88 375 220 ...
    ##  $ SURFTEMP   : num  16.4 18.2 18.2 18.2 19.2 19.3 19.3 19.6 20.4 21.1 ...
    ##  $ SURFSALIN  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ BOTTEMP    : num  NA 9.4 9.4 9.4 9.1 8.2 8.2 9.5 NA 11.9 ...
    ##  $ BOTSALIN   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ABUNDANCE  : num  1 3 3 3 1 2 2 1 1 2 ...
    ##  $ BIOMASS    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LENGTH     : num  23 11 14 22 9 23 25 28 15 22 ...
    ##  $ NUMLEN     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ INDWT      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WGTLEN     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SIZECAT    : Factor w/ 5 levels "XS","S","M","L",..: 3 2 2 3 2 3 3 3 2 3 ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "sorted")= chr [1:2] "SVSPP" "CATCHSEX"

``` r
cat("N rows, N columns\n")
```

    ## N rows, N columns

``` r
dim(survdat.lw)
```

    ## [1] 2892498      24

</details>
<details>
<summary>
Column names, types, and number of rows, columns in `survdat.lw24` from
2024:
</summary>

``` r
cat("Names\n")
```

    ## Names

``` r
names(survdat.lw24)
```

    ##  [1] "SVSPP"       "CATCHSEX"    "CRUISE6"     "STATION"     "STRATUM"    
    ##  [6] "TOW"         "SVVESSEL"    "YEAR"        "SEASON"      "LAT"        
    ## [11] "LON"         "EST_TOWDATE" "DEPTH"       "SURFTEMP"    "SURFSALIN"  
    ## [16] "BOTTEMP"     "BOTSALIN"    "ABUNDANCE"   "BIOMASS"     "LENGTH"     
    ## [21] "NUMLEN"      "PREDWT"      "WGTLEN"

``` r
str(survdat.lw24)
```

    ## Classes 'data.table' and 'data.frame':   3112779 obs. of  23 variables:
    ##  $ SVSPP      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CATCHSEX   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CRUISE6    : num  196721 196817 196817 196817 196817 ...
    ##  $ STATION    : num  9 8 8 8 38 41 41 43 46 50 ...
    ##  $ STRATUM    : num  1080 1080 1080 1080 1720 1720 1720 1700 1680 1680 ...
    ##  $ TOW        : num  2 1 1 1 2 1 1 1 2 1 ...
    ##  $ SVVESSEL   : chr  "AL" "AL" "AL" "AL" ...
    ##  $ YEAR       : num  1967 1968 1968 1968 1968 ...
    ##  $ SEASON     : chr  "FALL" "FALL" "FALL" "FALL" ...
    ##  $ LAT        : num  40 40 40 40 38 ...
    ##  $ LON        : num  -71.2 -70.6 -70.6 -70.6 -73.9 ...
    ##  $ EST_TOWDATE: POSIXct, format: "1967-10-18 15:47:00" "1968-10-11 07:19:00" ...
    ##  $ DEPTH      : int  308 325 325 325 293 325 325 88 375 220 ...
    ##  $ SURFTEMP   : num  16.4 18.2 18.2 18.2 19.2 19.3 19.3 19.6 20.4 21.1 ...
    ##  $ SURFSALIN  : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ BOTTEMP    : num  NA 9.4 9.4 9.4 9.1 8.2 8.2 9.5 NA 11.9 ...
    ##  $ BOTSALIN   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ABUNDANCE  : num  1 3 3 3 1 2 2 1 1 2 ...
    ##  $ BIOMASS    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LENGTH     : num  23 11 14 22 9 23 25 28 15 22 ...
    ##  $ NUMLEN     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PREDWT     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WGTLEN     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  - attr(*, ".internal.selfref")=<externalptr> 
    ##  - attr(*, "index")= int(0) 
    ##   ..- attr(*, "__SEASON")= int [1:3112779] 1 2 3 4 5 6 7 8 9 10 ...

``` r
cat("N rows, N columns\n")
```

    ## N rows, N columns

``` r
dim(survdat.lw24)
```

    ## [1] 3112779      23

</details>

The 2014 pull has an differently named column, `INDWT` which might be
`PREDWT` now, need to check.

The 2014 pull has an additional column, `SIZECAT` which is definitely
used in the script. Need to see how to recreate this.

We don’t have the sql code that created the 2014 pull.

<details>
<summary>
Attributes of the 2024 pull here:
</summary>

``` r
cat("SQL query\n")
```

    ## SQL query

``` r
cat(paste(names(survdat24$sql), "\n", unlist(survdat24$sql), collapse = "\n\n"))
```

    ## catch 
    ##  select cruise6, station, stratum, tow, svspp, catchsex,
    ##                      expcatchnum as abundance, expcatchwt as biomass
    ##                      from svdbs.UNION_FSCS_SVCAT
    ##                      where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                      and stratum not like 'YT%'
    ##                      order by cruise6, station, svspp
    ## 
    ## cruise 
    ##  select unique year, cruise6, svvessel, season
    ##       from svdbs.mstr_cruise
    ##       where purpose_code = 10
    ##       and year >= 1963and (season = 'FALL'
    ##         or season = 'SPRING')
    ##       order by year, cruise6
    ## 
    ## station 
    ##  select unique cruise6, svvessel, station, stratum,
    ##                                tow, decdeg_beglat as lat, decdeg_beglon as lon,
    ##                                begin_est_towdate as est_towdate, avgdepth as depth,
    ##                                surftemp, surfsalin, bottemp, botsalin
    ##                                from svdbs.UNION_FSCS_SVSTA
    ##                                where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                                and (SHG <= 136 and cruise6 <= 200900)
    ##                                or (TOGA <= 1324 and cruise6 > 200900)
    ##                                order by cruise6, station
    ## 
    ## length 
    ##  select cruise6, station, stratum, tow, svspp, catchsex,
    ##                       length, expnumlen as numlen
    ##                       from svdbs.UNION_FSCS_SVLEN
    ##                       where cruise6 in ('196307','196413','196514','196614','196721','196803','196817','196902','196911','197003','197006','197006','197101','197106','197202','197208','197208','197208','197303','197303','197303','197308','197308','197404','197404','197404','197411','197411','197503','197503','197512','197512','197602','197602','197609','197702','197702','197712','197804','197806','197904','197904','197910','197910','198002','198002','198007','198102','198106','198106','198202','198206','198303','198306','198402','198405','198502','198508','198508','198603','198606','198606','198702','198702','198705','198801','198803','198901','198904','199002','199004','199102','199105','199202','199206','199302','199306','199402','199406','199503','199507','199602','199604','199702','199706','199802','199804','199902','199908','200002','200005','200102','200109','200202','200209','200303','200306','200402','200407','200503','200510','200604','200610','200703','200709','200803','200807','200902','200904','201001','201004','201102','201105','201202','201204','201302','201304','201402','201404','201502','201504','201602','201604','201702','201704','201802','201804','201902','201904','202002','202004','202102','202104','202202','202204','202302','202304','202402','202404')
    ##                       and stratum not like 'YT%'
    ##                       order by cruise6, station, svspp, length
    ## 
    ## weightlength 
    ##  select svspp, sex, svlwexp, svlwcoeff, svlwexp_spring, svlwcoeff_spring,
    ##                svlwexp_fall, svlwcoeff_fall, svlwexp_winter, svlwcoeff_winter,
    ##                svlwexp_summer, svlwcoeff_summer
    ##                from svdbs.length_weight_coefficients
    ## 
    ## sad 
    ##  
    ## 
    ## conversions 
    ##  select *
    ##     from svdbs.survan_conversion_factors

``` r
cat("\n\nDate pulled\n")
```

    ## 
    ## 
    ## Date pulled

``` r
cat(survdat24$pullDate)
```

    ## Fri May 17 09:16:30 2024

``` r
cat("\n\nFunction call\n")
```

    ## 
    ## 
    ## Function call

``` r
print(survdat24$functionCall)
```

    ## survdat::get_survdat_data(channel = channel, filterByYear = NA, 
    ##     all.season = F, shg.check = T, conversion.factor = T, use.SAD = F, 
    ##     getBio = F, getLengths = T, getWeightLength = T)

</details>

Some types are different. If the nums need to change to ints in the
script we can do that.

## Run old script

<details>
<summary>
As far as we can…
</summary>
</details>

# Detailed analysis
