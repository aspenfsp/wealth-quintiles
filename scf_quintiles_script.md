Survey of Consumer Finances Net Worth Quintile Analysis
================
Shehryar Nabi, Senior Research Associate, Aspen Institute Financial
Security Program

# Solutions Scan

## New asset and debt categories

I first combine existing asset and debt types into new categories to
simplify the portfolio analysis.

``` r
# Create new asset categories 

securities <- select(scf_19, c(NMMF, SAVBND, STOCKS, BOND, WGT))
other_fin <- select(scf_19, c(CDS, CASHLI, OTHMA, OTHFIN, WGT))
nonres <- select(scf_19, c(ORESRE, NNRESRE, WGT))

scf_19$SECURITIES <- rowSums(securities[1:4])
scf_19$OTHFIN_COMPLETE <- rowSums(other_fin[1:4])
scf_19$NONRES <- rowSums(nonres[1:2])


# Create new debt category 

otherdebt <- scf_19 %>%
  select(OTHLOC, OTH_INST, ODEBT)

scf_19$OTHDBT <- rowSums(otherdebt)
```

## Median net worth by quintile for each year

After identifying the net worth quintiles, I calculate the median net
worth by quintile for 1989, 2007, and 2019.

``` r
# 1989 (below procedure repeated for each year)

# Adding net worth quintile ranges

nw_quintile_breaks_89 <- c(weighted_networth_quantiles(scf_89$NETWORTH, 
                                                  scf_89$WGT, 1/5))
nw_quintile_breaks_89[1] <- nw_quintile_breaks_89[1] - 1
scf_89$NETWORTH_quintiles <- cut(scf_89$NETWORTH,
                            c(nw_quintile_breaks_89),
                            labels = c("0-19.9", "20-39.9",
                                   "40-59.9", "60-79.9", 
                                   "80-100"))

# Calculating median net worth by quintile 

med_nw_89 <- scf_89 %>%
  group_by(scf_89$NETWORTH_quintiles) %>%
  summarise(nw = weighted.median(NETWORTH, WGT)/1000)


# 2007 

nw_quintile_breaks_07 <- c(weighted_networth_quantiles(scf_07$NETWORTH, 
                                                  scf_07$WGT, 1/5))
nw_quintile_breaks_07[1] <- nw_quintile_breaks_07[1] - 1
scf_07$NETWORTH_quintiles <- cut(scf_07$NETWORTH,
                            c(nw_quintile_breaks_07),
                            labels = c("0-19.9", "20-39.9",
                                   "40-59.9", "60-79.9", 
                                   "80-100"))

med_nw_07 <- scf_07 %>%
  group_by(scf_07$NETWORTH_quintiles) %>%
  summarise(nw = weighted.median(NETWORTH, WGT)/1000)


# 2019 

nw_quintile_breaks_19 <- c(weighted_networth_quantiles(scf_19$NETWORTH, 
                                                  scf_19$WGT, 1/5))
nw_quintile_breaks_19[1] <- nw_quintile_breaks_19[1] - 1
scf_19$NETWORTH_quintiles <- cut(scf_19$NETWORTH,
                            c(nw_quintile_breaks_19),
                            labels = c("0-19.9", "20-39.9",
                                   "40-59.9", "60-79.9", 
                                   "80-100"))

med_nw_19 <- scf_19 %>%
  group_by(scf_19$NETWORTH_quintiles) %>%
  summarise(nw = weighted.median(NETWORTH, WGT)/1000)
```

## Asset ownership and value by type and net worth quintile

I find the median value and prevalence (% holding any) of assets in 2019
broken down by type and for each net worth quintile.

``` r
# Median value

assets_med_19 <- scf_19 %>%
         group_by(NETWORTH_quintiles) %>%
         summarise(transaction = weighted.median(LIQ, WGT)/1000,
                   securities = weighted.median(SECURITIES, WGT)/1000,
                   ret = weighted.median(RETQLIQ, WGT)/1000,
                   othfin = weighted.median(OTHFIN_COMPLETE, WGT)/1000,
                   vehic = weighted.median(VEHIC, WGT)/1000,
                   res = weighted.median(HOUSES, WGT)/1000,
                   nonres = weighted.median(NONRES, WGT)/1000,
                   business = weighted.median(BUS, WGT)/1000,
                   othnfin = weighted.median(OTHNFIN, WGT)/1000)


# % holding

assets_pct <- scf_19 %>%
         group_by(NETWORTH_quintiles) %>%
         summarise(transaction = wpct(LIQ > 0)[1],
                   securities = wpct(SECURITIES > 0)[1],
                   ret = wpct(RETQLIQ > 0)[1],
                   othfin = wpct(OTHFIN_COMPLETE > 0)[1],
                   vehic = wpct(VEHIC > 0)[1],
                   res = wpct(HOUSES > 0)[1],
                   nonres = wpct(NONRES > 0)[1],
                   business = wpct(BUS > 0)[1],
                   othnfin = wpct(OTHNFIN > 0)[1])
```

## Median value of assets conditional on having them

For each asset type, I first isolate households that own it and then
calculate its median value for each net worth quintile.

``` r
# Isolating those who own the asset 

scf_19_vehic <- filter(scf_19, scf_19$VEHIC > 0)

# Getting median value by net worth quintile 

vehic_assets <- scf_19_vehic %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(med = weighted.median(VEHIC, WGT)/1000)


# This is repeated for each asset type

scf_19_bus <- filter(scf_19, scf_19$BUS > 0)
bus_equity <- scf_19_bus %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(BUS, WGT)/1000)

scf_19_tra <- filter(scf_19, scf_19$LIQ > 0)
tra <- scf_19_tra %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(LIQ, WGT)/1000)

scf_19_sec <- filter(scf_19, scf_19$SECURITIES > 0)
sec <- scf_19_sec %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(SECURITIES, WGT)/1000)

scf_19_ret <- filter(scf_19, scf_19$RETQLIQ > 0)
ret <- scf_19_ret %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(RETQLIQ, WGT)/1000)

scf_19_ofi <- filter(scf_19, scf_19$OTHFIN_COMPLETE > 0)
ofi <- scf_19_ofi %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(OTHFIN_COMPLETE, WGT)/1000)

scf_19_res <- filter(scf_19, scf_19$HOUSES > 0)
res <- scf_19_res %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(HOUSES, WGT)/1000)

scf_19_opr <- filter(scf_19, scf_19$NONRES > 0)
opr <- scf_19_opr %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(NONRES, WGT)/1000)

scf_19_onf <- filter(scf_19, scf_19$OTHNFIN > 0)
onf <- scf_19_onf %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(OTHNFIN, WGT)/1000)
```

## Debt ownership and value by type and net worth quintile

I find the % holding and median value of debt by type and net worth
quintile for 2019.

``` r
debt_med <- scf_19 %>%
         group_by(NETWORTH_quintiles) %>%
         summarise(res = weighted.median(MRTHEL, WGT)/1000,
                   nonprime = weighted.median(RESDBT, WGT)/1000,
                   ccbal = weighted.median(CCBAL, WGT)/1000,
                   educ = weighted.median(EDN_INST, WGT)/1000,
                   vehic = weighted.median(VEH_INST, WGT)/1000,
                   othdebt = weighted.median(OTHDBT, WGT)/1000)


debt_pct <- scf_19 %>%
         group_by(NETWORTH_quintiles) %>%
         summarise(res = wpct(MRTHEL > 0)[1],
                   nonprime = wpct(RESDBT > 0)[1],
                   ccbal = wpct(CCBAL > 0)[1],
                   educ = wpct(EDN_INST > 0)[1],
                   vehic = wpct(VEH_INST > 0)[1],
                   othdebt = wpct(OTHDBT > 0)[1])


write.csv(debt_pct, paste(getwd(), 
                '\\debt_pct.csv', 
                sep=''))
```

## Median value of debts conditional on having them

``` r
# Same process as above to calculate asset value conditional on holding

scf_19_res <- filter(scf_19, scf_19$MRTHEL > 0)
res_debt <- scf_19_res %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(MRTHEL, WGT),
            n = n())

scf_19_nonprime <- filter(scf_19, scf_19$RESDBT > 0)
nonprime_debt <- scf_19_nonprime %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(RESDBT, WGT),
            n = n())

scf_19_ccbal <- filter(scf_19, scf_19$CCBAL > 0)
ccbal_debt <- scf_19_ccbal %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(CCBAL, WGT),
            n = n())

scf_19_veh <- filter(scf_19, scf_19$VEH_INST > 0)
veh_debt <- scf_19_veh %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(VEH_INST, WGT),
            n = n())

scf_19_edu <- filter(scf_19, scf_19$EDN_INST > 0)
edu_debt <- scf_19_edu %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(EDN_INST, WGT),
            n = n())

scf_19_oth <- filter(scf_19, scf_19$OTHDBT > 0)
oth_debt <- scf_19_oth %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(OTHDBT, WGT)/1000,
            n = n())
```

## Median net worth of people with businesses by net worth quintile

Using a similar process as above, I isolate business owners to find
their median wealth by net worth quintile.

``` r
with_bus <- filter(scf_19, scf_19$BUS > 0)

biz_nw_quintile <- with_bus %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(med = weighted.median(NETWORTH, WGT))

biz_value_quintile <- with_bus %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(med = weighted.median(BUS, WGT))

biz_pct_quintile <- scf_19 %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(pct = wpct(BUS > 0, WGT)[1])
```

# Supplemental Figures

## 20-60-20 median debt values and holding rates

I calculate weighted median debt values and holding rates for the bottom
20%, middle 60%, and top 20% of households.

``` r
debt_med_bmt <- scf_19 %>%
         group_by(nw_bmt_breaks) %>%
         summarise(res = weighted.median(MRTHEL, WGT)/1000,
                   nonprime = weighted.median(RESDBT, WGT)/1000,
                   ccbal = weighted.median(CCBAL, WGT)/1000,
                   educ = weighted.median(EDN_INST, WGT)/1000,
                   vehic = weighted.median(VEH_INST, WGT)/1000,
                   othdebt = weighted.median(OTHDBT, WGT)/1000)


hold_debt_bmt <- scf_19 %>%
  group_by(nw_bmt_breaks) %>%
  summarise(hold_mort = wpct(MRTHEL > 0, WGT)[1],
            hold_edu = wpct(EDN_INST > 0, WGT)[1],
            hold_ccbal = wpct(CCBAL > 0 , WGT)[1],
            hold_auto = wpct(VEH_INST > 0, WGT)[1])
```

## 20-60-20 net worth by year

``` r
# 1989

# weighted.quantile(scf_89$NETWORTH, scf_89$WGT, probs = .8)

scf_89$nw_bmt_breaks <- cut(scf_89$NETWORTH,
    c(-100000000, 4375.7, 376060.3, 1967199002),
    labels = c("Bottom 20%", "Middle 60%",
                "Top 20%"), na.rm = TRUE)

med_nw_89_bmt <- scf_89 %>%
  group_by(nw_bmt_breaks) %>%
  summarise(weighted.median(NETWORTH, WGT))


# 2007

# weighted.quantile(scf_07$NETWORTH, scf_07$WGT, probs = 0)

scf_07$nw_bmt_breaks <- cut(scf_07$NETWORTH,
    c(-684740.4, 9019.158, 613378.7, 1967199002),
    labels = c("Bottom 20%", "Middle 60%",
                "Top 20%"), na.rm = TRUE)

med_nw_07_bmt <- scf_07 %>%
  group_by(nw_bmt_breaks) %>%
  summarise(weighted.median(NETWORTH, WGT))


# 2019

# weighted.quantile(scf_19$NETWORTH, scf_19$WGT, probs = .8)

scf_19$nw_bmt_breaks <- cut(scf_19$NETWORTH,
    c(-1055500, 6370, 557160, 2967199002),
    labels = c("Bottom 20%", "Middle 60%",
                "Top 20%"), na.rm = TRUE)

med_nw_19_bmt <- scf_19 %>%
  group_by(nw_bmt_breaks) %>%
  summarise(weighted.median(NETWORTH, WGT))
```
