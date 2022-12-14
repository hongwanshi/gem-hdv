
#### Common ####
#days <- 352:355
#days <- c(1) 
#days <- c(74:81,147:154,260:267,351:358) # 8 per week
#days <- c(74:80,147:153,260:266,351:357) # 7 per week
#days <- c(74:77,79,148:152,260:264,351:355) # 5 per week, 4 weekday, 1 weekend
# DEFAULT TIME PERIOD FOR PAPERS / FINAL ANALYSIS
#days <- c(74:77,148:150,260:262,351:354) # 3 per week, 2 weekday, 1 weekend plus 2 for buffer on ends
# DEFAULT TIME PERIOD FOR PAPERS / FINAL ANALYSIS CORRECTED
#days <- c(70:73,176:178,260:262,351:354) # 3 per week, 2 weekday, 1 weekend plus 2 for buffer on ends
# days <- c(70:71) # 3 per week, 2 weekday, 1 weekend plus 2 for buffer on ends

#days <- c(75:76,149:150,261:262,352:353) # 2 per week, 2 weekday

#days <- c(75,149,261,352) # 1 per week, 1 weekday
days <- c(75,149,261) # 1 per week, 1 weekday
#days <- c(74:77,79)
#days <- c(148:152) 
#days <- c(74:77,79,148:152)
#days <- c(260:264)
#days <- c(351:355)
#days <- c(260:264,351:355)
group.days <- 0 # set this to 0 to run all "days" at once, set to non-zero to run "grouped.days" at a time over the full range of "days" overlap occurs for one day on end-points

year <- 2040
discountRate <- 0.05

#### Mobility ####
electrificationPenetration <- 1.0 # how many of total trips are electrified
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW

chargerLifetime <- 10
sharingFactor <- 1.5
vmtReboundFactor <- 1.0 # scales VMT in the system, imperfectly, by scaling # trips for SAEVs and energy for private charging
batteryCapitalCost <- 150
vehicleCapitalCost <- 30000
b150ConversionEfficiency <- 0.324 # What is average conversion efficiency for a 150-mile BEV? kwh/mile, all other vehicle ranges
                                  # will be scaled proportionally assuming 0.324 as the default for BEV150
privateBEVConversionEfficiency <- 0.325 # EVI-Pro default was 0.325, this will adjust charging load proportionally for private BEVs
scale.urban.form.factor <- 1.0
includeTransitDemand <- 1 # 0 or 1
fractionSAEVs <- 0.75

fractionSmartCharging <- 0.5
privateFleetWeights <- "fleet_weights_base" # name (without extension) of file in {gem.raw.inputs}/NREL-EVI-Pro-Preprocessed-Profiles/data
congestion <- 'Freeflow'
l10ChargerCost <- 500 # $/kW
chargerCostSuperlinear <- 3 # rate of increase beyond linear from low to high power chargers
# Following is based on NHTS demand without transit and assumes 10 holidays per year and splits difference in remainder
weekday.to.year.factor <- 250.5 + 114.5*(19861098473/22232870872) # N_weekdays + N_weekends*(Demand_weekends/Demand_weekdays)
# Following is based on above but adjusted to account for runs with 2 weekdays + 1 weekend
weekday.to.year.factor <- 250.5*(250.5/365/.66666) + 114.5*(114.5/365/.33333)*(19861098473/22232870872) 

chts.trips <- fread(pp(gem.raw.inputs,'chts_triptimes.csv'))

#### Grid ####
generators <- fread(pp(gem.raw.inputs,'gem_gridInputs_generators.csv'))
load <- fread(pp(gem.raw.inputs,'gem_gridInputs_load.csv'))
transmission <- fread(pp(gem.raw.inputs,'gem_gridInputs_transmission.csv'))
transmissionScalingFactor <- 1.0
renewableCF <- fread(pp(gem.raw.inputs,'gem_gridInputs_renewableCF.csv'))

fuels <- data.frame('FuelType'=c('Wind','Waste Coal','Tire','Solar','Pumps','Pet. Coke','Oil','Nuclear','None','Non-Fossil','NaturalGas','MSW','LF Gas','Hydro','Geothermal','Fwaste','Coal','Biomass','Tires'),'Simplified'=c('Wind','Coal','Other','Solar','Other','Other','Other','Nuclear','Other','Other','Natural Gas','Other','Natural Gas','Hydro','Other','Other','Coal','Other','Other'))
meritOrder <- c('Solar','Wind','Hydro','Other','Natural Gas','Coal','Nuclear')

####### trucks
truckcharger.levels <- c(50,100,250,500,1000) #kW
truckbattery.levels <- c(250,400,500,750,1000)
tb250cost <- 300
tblinear <- 0

truckbatteryCapitalCost <- 500

truckchargerLifetime <- 10

truckvehicleCapitalCost <- 70000

trucksharingFactor <- 1

truckvmtReboundFactor <- 1.0 
truckelectrificationPenetration <- 1.0
truckfractionSAEVs <- 0.5
truckfractionSmartCharging <- 0.5
truckprivateFleetWeights <- "truck_fleet_weights_base"
tl10ChargerCost <- 1000 # $/kW
tchargerCostSuperlinear <- 2 # rate of increase beyond linear from low to high power chargers
truckhumanchargingfactor <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)#c(1.05,1.05,1.05,1,1,1.1,1.1,1.15,1.15,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1.1,1.0,1.1,1.1,1.05,1.05)#c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)#c(1.05,1.05,1.05,1,1,1.1,1.1,1.15,1.15,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1.1,1.0,1.1,1.1,1.05,1.05)
ldvprivatechargingfactor <- c(0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7,1,1,1,1,1,0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7)#c(1,1,0.9,0.8,0.7,0.6,0.6,0.8,1,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1,1.0,1,1,1,1)#c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)#c(1.05,1.05,1.05,1,1,1.1,1.1,1.15,1.15,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1.1,1.0,1.1,1.1,1.05,1.05)
ldvhumanchargingfactor <- c(1.0,1.0,0.9,0.9,0.8,0.8,0.8,0.8,0.7,0.9,0.7,0.4,0.4,0.4,0.5,0.5,0.5,0.5,0.5,0.6,0.7,0.8,0.8,0.9)#c(1.0,1.0,0.9,0.9,0.8,0.8,0.8,0.8,0.7,0.9,0.7,0.6,0.4,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.7,0.8,0.8,0.9)#c(1,1,0.9,0.8,0.7,0.6,0.6,0.8,1,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1,1.0,1,1,1,1)#c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)#c(1.05,1.05,1.05,1,1,1.1,1.1,1.15,1.15,1.0,0.9,0.8,0.8,0.7,0.8,0.9,0.9,1.0,1.1,1.0,1.1,1.1,1.05,1.05)
human_factor <- 0.5
#### bikes ####
bikebatteryCapitalCost <- 10
bikechargerLifetime <- 10
bikevehicleCapitalCost <- 1000
bikesharingFactor <- 1 # ??
bikeelectrificationPenetration <- 1.0 #how many trips are electrified
bikevmtReboundFactor <- 1.0 # scales VMT in the system, imperfectly, by scaling # trips for SAEVs and energy for private charging
bikefractionSAEVs <- 0.5
bikecharger.levels <- 1 #kW
bikebattery.levels <- 40 #mi
#bikebatteryCapitalCost <- 10 #usd
bikechargerCapitalCost <- 10 #usd
bikechargercost <- 0.25 #coefficient

biketocar <- c(0.036,0.013,0.005,0.004,0.002)
biketocarfactor <- 1
