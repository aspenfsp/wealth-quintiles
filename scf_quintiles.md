Survey of Consumer Finances Net Worth Quintile Analysis
================
Shehryar Nabi, Senior Research Associate, Aspen Institute Financial
Security Program

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
         summarise(transaction = sum(LIQ > 0)/n(),
                   securities = sum(SECURITIES > 0)/n(),
                   ret = sum(RETQLIQ > 0)/n(),
                   othfin = sum(OTHFIN_COMPLETE > 0)/n(),
                   vehic = sum(VEHIC > 0)/n(),
                   res = sum(HOUSES > 0)/n(),
                   nonres = sum(NONRES > 0)/n(),
                   business = sum(BUS > 0)/n(),
                   othnfin = sum(OTHNFIN > 0)/n())
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
         summarise(res = sum(MRTHEL > 0)/n(),
                   nonprime = sum(RESDBT > 0)/n(),
                   ccbal = sum(CCBAL > 0)/n(),
                   educ = sum(EDN_INST > 0)/n(),
                   vehic = sum(VEH_INST > 0)/n(),
                   othdebt = sum(OTHDBT > 0)/n())
```

## Median value of debts conditional on having them

``` r
# Same process as above to calculate asset value conditional on holding

scf_19_res <- filter(scf_19, scf_19$MRTHEL > 0)
res_debt <- scf_19_res %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(MRTHEL, WGT)/1000,
            n = n())

scf_19_nonprime <- filter(scf_19, scf_19$RESDBT > 0)
nonprime_debt <- scf_19_nonprime %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(RESDBT, WGT)/1000,
            n = n())

scf_19_ccbal <- filter(scf_19, scf_19$CCBAL > 0)
ccbal_debt <- scf_19_ccbal %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(CCBAL, WGT)/1000,
            n = n())

scf_19_veh <- filter(scf_19, scf_19$VEH_INST > 0)
veh_debt <- scf_19_veh %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(VEH_INST, WGT)/1000,
            n = n())

scf_19_edu <- filter(scf_19, scf_19$EDN_INST > 0)
edu_debt <- scf_19_edu %>%
  group_by(NETWORTH_quintiles) %>%
  summarise(median_equity = weighted.median(EDN_INST, WGT)/1000,
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
```
