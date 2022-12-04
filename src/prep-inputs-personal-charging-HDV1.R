###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the personal EV charging inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.personal.charging.hdv <- function(exper.row,param.names,common.inputs,inputs.mobility){
  ###########################################################
  # Setup variables and environment
  ###########################################################
  memory.limit(9999999999)
  workingDir <- pp(gem.raw.inputs,"NREL-EVI-Pro-Preprocessed-Profiles")

  #Directory and file containing NREL EVIPro data
  evi_data_dir <- pp(workingDir,"/data/sessions/")    #Directory of files containing raw charging session data
  dvmt_data_file <- pp(workingDir,"/data/chts_dvmt.csv")  #File containing CHTS total daily vmt by vid to append to EVI data

  source("src/eviPro/func_LoadEVIPro.R")            #loads EVIPro data and stores in a single data table
  source("src/eviPro/func_EVIFleetGen.R")           #Generates a fleet of EVIPro vids
  source("src/eviPro/func_HEVIFleetGen.R")           #Generates a fleet of EVIPro vids
  source("src/eviPro/func_calcBaseEVILoad.R")       #Pre-calculates load profile for all unique_vid in evi_raw
  source("src/eviPro/func_measureFleetWeights.R")   #Creates statistics of generated fleet
  source("src/eviPro/func_LoadFleetWeights.R")      #Loads .csv file where fleet characteristic weights are stored
  source("src/eviPro/func_GenVmtWeights.R")         #Generates vmt distribution for fleet generation
  source("src/eviPro/func_CreateFleetWeights.R")    #Creates fleet weights from values hard coded in this function. Use this instead of loading fleet weights from a file if desired.
  #source("src/eviPro/func_truckassumEVIFleetGen.R")    #Creates fleet weights from values hard coded in this function. Use this instead of loading fleet weights from a file if desired.
  
  if('truckelectrificationPenetration' %in% names(exper.row)){
    truckelectrificationPenetration <- exper.row$truckelectrificationPenetration
  }
  if('truckvmtReboundFactor' %in% param.names){
    truckvmtReboundFactor <- exper.row$truckvmtReboundFactor
  }
  if('truckfractionSAEVs' %in% names(exper.row)){
    truckfractionSAEVs <- exper.row$truckfractionSAEVs
  }
  if('truckfractionSmartCharging' %in% names(exper.row)){
    truckfractionSmartCharging <- exper.row$truckfractionSmartCharging
  }
  if('truckprivateFleetWeights' %in% names(exper.row)){
    truckprivateFleetWeights <- exper.row$truckprivateFleetWeights
  }
  if('truckprivateBEVConversionEfficiency' %in% names(exper.row)){
    truckconversionEfficiencyScalingFactor <- exper.row$truckprivateBEVConversionEfficiency/0.325
  }else{
    truckconversionEfficiencyScalingFactor <- 1.0
  }
  
  if(truckfractionSAEVs < 1){
    ###########################################################
    # Optionally load EVI charging and load profile data
    ###########################################################
    # Desired size of fleet. The 3.37 is the average number of trips per driver based on https://nhts.ornl.gov/assets/2017_nhts_summary_travel_trends.pdf
    load.data.needed <- F
    for(the.region in common.inputs$sets$rmob){
      # Look for the results in the gem cache, otherwise process and load
      cache.file <- pp(workingDir,'/data/gem-cache/region-',the.region,'-truckprivateFleetWeights-',truckprivateFleetWeights,'-smart-',truckfractionSmartCharging,'.Rdata')
      if(!file.exists(cache.file))load.data.needed<-T
    }
  
    if(load.data.needed){
      cat(pp('Personal truck EV charging profiles need to be created. This will take a while to load the data and process but results will be cached and future runs for this fleet size will be very fast.\n'))
      Sys.sleep(0.2)
      run_evi_load_func <- FALSE #Set to true to re-create evi_raw data table. Set to false to load .RData file
      run_load_calc_func <- FALSE #Set to true if you want to re-calc. This takes about 3.5 hours. Set to false to load .RData file
  
      #Load raw EVI data
      raw_data_file_name <- paste0(workingDir,"/data/evi_raw_data_2018-11-26.RData") #Source of data, or file name to save to if re-calc
      if(!exists("truck_evi_raw")) {
        if(run_evi_load_func) {
          truck_evi_raw <- load_EVIPro(evi_data_dir,dvmt_data_file,vmt_bin_size)
          #Make sure no NAs exist in the data set
          print(truck_evi_raw[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=1:ncol(truck_evi_raw)])
          save(truck_evi_raw, file=raw_data_file_name)
        } else {
          load(raw_data_file_name,envir=globalenv())
          truck_evi_raw <- evi_raw
        }
      }
      levels(truck_evi_raw$pev_type) <- c("PHEV20","PHEV50","BEV600","BEV800")
      #Load power profile for all unique_vids
      load_file_name <- paste0(workingDir,"/data/evi_load_profile_data_2019-01-06.RData") #Source of data, or file name to save to if re-calc
      if(!exists("truck_evi_load_profiles")) {
        if(run_load_calc_func) {
          print(Sys.time())
          truck_evi_load_profiles <- calcBaseEVILoad(truck_evi_raw,time_step) 
          # Adjust the start and tend time of the charing session as much as possible
          evi_delayed <- copy(truck_evi_raw)
          evi_delayed[,start_time:=start_time+(end_time_prk-end_time_chg)]
          evi_delayed[,end_time_chg:=end_time_prk]
          truck_evi_delayed_load_profiles <- calcBaseEVILoad(evi_delayed,time_step) 
          truck_evi_load_profiles <- truck_evi_load_profiles[,list(unique_vid,session_id,time_of_day,schedule_vmt,avg_kw,kwh,level=dest_chg_level,plugged_kw)]
          truck_evi_delayed_load_profiles <- truck_evi_delayed_load_profiles[,list(unique_vid,session_id,time_of_day,schedule_vmt,avg_kw,kwh,level=dest_chg_level,plugged_kw)]
          save(truck_evi_load_profiles,truck_evi_delayed_load_profiles,file=load_file_name)
          print(Sys.time())
        } else {
          load(load_file_name,envir=globalenv())
          truck_evi_load_profiles <- evi_load_profiles
          truck_evi_delayed_load_profiles <- evi_delayed_load_profiles
        }
      }
      
      load_file_name <- pp(gem.raw.inputs,'nhts/daily_vmt_per_person.Rdata')
      if(!file.exists(load_file_name)){
        dfs <- list()
        to.load <- c('avg_daily_vmt_perperson_region_season_day_no_transit.csv','avg_daily_vmt_perperson_region_season_day_with_transit.csv')
        for(file in to.load){
          df <- data.table(read.csv(pp(gem.raw.inputs,'nhts/',file),stringsAsFactors=F))
          df[,transit:=pp(str_split(file,"_")[[1]][8:9],collapse='_')]
          df[,transit:=substr(transit,1,nchar(transit)-4)]
          dfs[[length(dfs)+1]] <- df
        }
        dvmt <- rbindlist(dfs)
        dvmt[,rmob:=pp(CDIVLS,'-',URBRURS)]
        save(dvmt,file=load_file_name)
        rm('dfs')
      }
      load(load_file_name)
    }
  
    ###########################################################
    # Configure variables and process profiles for every region
    ###########################################################
    #INTERNAL TOOL SETTING: Specifiy the vmt bin size to use for creating the fleet vmt distribution
    vmt_bin_size <- 10 #mile
    #INTERNAL TOOL SETTING: Set time series resolution over which kWh and average kW will be calculated
    # DO NOT CHANGE THIS UNLESS YOU PLAN TO RE-RUN PRECALCULATED LOAD PROFILES.
    time_step <- 1 #hours
    #TOOL USER INPUT: Load .csv file containing desired weights of fleet characteristics
    weights_file <- paste0(workingDir,"/data/",truckprivateFleetWeights,".csv")
    if(file.exists(weights_file)) {
      fleet_weights <- load_fleet_weights(weights_file)
    } else {
      cat("WARNING: no fleet weights file found, creating defaults with 'create_fleet_weights'....")
      fleet_weights <- create_fleet_weights()
    }
    transit.type <- ifelse(includeTransitDemand,'with_transit','no_transit')
    # We assume 4.17 trips per person, this is based on NHTS overall trips per person 3.46 but then rescaled by 1/0.83 to account for the people who take no trips (which we effectively remove)
    fleet.sizes <- inputs.truck$parameters$truckdemandUnscaled[,list(n.vehs=round(sum(value)/length(days)*truckelectrificationPenetration*(1-truckfractionSAEVs)*0.5,0)),by='rmob']
    all.all.energy.constraints <- list()
    all.unmanaged.loads <- list()
    all.all.energy.constraints.human <- list()
    all.unmanaged.loads.human <- list()
    all.fleets <- list()
    all.chargers <- list()
    the.region <- common.inputs$sets$rmob[1]
    for(the.region in common.inputs$sets$rmob){
      fleet_size <- fleet.sizes[rmob==the.region]$n.vehs
      
      ##############################################################################
      # Create or load fleet characteristics weights for each DVMT value of interest
      ##############################################################################
      energy.and.power.constraints <- list()
      dates <- date.info(days,year)
      season <- u(toupper(dates$seasons))[1]
      for(season in u(toupper(dates$seasons))){
        energy.and.power.constraints[[season]] <- list()
        weekday.type <- u(dates$day.types)[1]
        for(weekday.type in u(dates$day.types)){
          energy.and.power.constraints[[season]][[weekday.type]] <- list()
  
          # Look for the results in the gem cache, otherwise process and load
          cache.dir <- pp(workingDir,'/data/gem-cache/',the.region,'/')
          make.dir(cache.dir)
          cache.dir <- pp(cache.dir,truckprivateFleetWeights,'/')
          make.dir(cache.dir)
          cache.file <- pp(cache.dir,'truck_smart-',truckfractionSmartCharging,'-season-',season,'-day-',str_replace_all(weekday.type,"/",""),'-transit-',transit.type,'.Rdata')
          if(file.exists(cache.file)){
            load(cache.file)
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]] <- energy.and.power.constraints.cache[[season]][[weekday.type]][[transit.type]]
          }else{
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]] <- list()
            
            #TOOL USER INPUT: Define average per-vehicle total dvmt from which a dvmt distribution will be created
            # Note that vmt distribution width is hard coded in the vmt_WeightDistGen() function. This has yet to be verified.
            # avg_dvmt <- 28.5 #miles
            avg_dvmt <- dvmt[rmob==the.region & SEASON==season & WKTIME == weekday.type & transit == transit.type]$TRPMILES
            if(length(avg_dvmt)==0)avg_dvmt <- mean(dvmt[rmob==the.region & transit == transit.type]$TRPMILES) # handle missing data
            #cat(pp(the.region,",",season,",",weekday.type,",",transit.type,"\n"))
            #cat(pp("Avg DVMT:",avg_dvmt,"\n"))
      
            #Take an average per-vehicle dvmt and generate a fleet dvmt distribution
            fleet_weights$vmt_weights <- vmt_WeightDistGen(avg_dvmt,vmt_bin_size)
      
            ###########################################################
            # Generate fleet and associated load profile
            ###########################################################
            #Create fleet
            evi_fleet <- evi_fleetGen(truck_evi_raw,
                                        20000,
                                        fleet_weights$pev_weights,
                                        fleet_weights$pref_weights,
                                        fleet_weights$home_weights,
                                        fleet_weights$work_weights,
                                        fleet_weights$public_weights,
                                        fleet_weights$vmt_weights
                                        )
            
            # Collect data on charging infrastructure requirements
            n.home <- fleet_weights$home_weights[,.(dest_type='Home',dest_chg_level=ifelse(name=='HomeL1','L1',ifelse(name=='HomeL2','L2',NA)),req=weight*50e3)][!is.na(dest_chg_level)]
            evi_fleet[,row:=1:nrow(evi_fleet)]
            evi_fleet[,start_time_hr:=start_time*24+3]
            evi_fleet[,end_time_prk_hr:=end_time_prk*24+6]
            evi_fleet[,end_time_chg_hr:=end_time_chg*24+6]
            occupied <- evi_fleet[dest_type!='Home',.(t=seq(start_time_hr,end_time_prk_hr,by=.25)),by=c('row','dest_type','dest_chg_level')]
            occupied[,t15:=round(t*4,0)/4]
            n.non.home <- occupied[,.(n=.N * 1.5 ),by=c('dest_type','t15','dest_chg_level')][,.(req=max(n)),by=c('dest_type','dest_chg_level')]
            unscaled.ch.requirements <- rbindlist(list(n.non.home,n.home))
            the.weights <- measureFleetWeights(evi_fleet)
            private.fleet <- rbindlist(list(the.weights$weekend$pev_weights[,days:=2],the.weights$weekday$pev_weights[,days:=5]))[,.(share=weighted.mean(weight,days)),by='name']
            unmanaged.load.by.ch <- evi_fleet[!is.na(start_time) & !is.na(end_time_chg),.(t=seq(start_time_hr,end_time_chg_hr,by=.25)),by=c('row','dest_chg_level','avg_kw')]
            unmanaged.load.by.ch[,hr:=floor(t)%%24]
            unmanaged.load.by.ch <- unmanaged.load.by.ch[,.(power=sum(.N * avg_kw / 4)),by=c('hr','dest_chg_level')]
            unmanaged.load.by.ch <- join.on(data.table(expand.grid(list(hr=0:23,l=u(unmanaged.load.by.ch$dest_chg_level)))),unmanaged.load.by.ch,c('hr','l'),c('hr','dest_chg_level'))
            unmanaged.load.by.ch[is.na(power),power:=0]
      
            #-----Generate 48-hour Fleet Load Profile----------
            
            #Handle missing data
            #Build list of unique_vid from evi_fleet that we'll use to construct the full load profile
            id_key <- evi_fleet[,.(unique_vid = unique(unique_vid)),by=fleet_id]
            setkey(id_key,unique_vid)
            #Subset load profiles specific to those unique_vids in the evi_fleet. This is the full load profile of the fleet
            setkey(truck_evi_load_profiles,unique_vid,session_id)
            fleet_load_profiles <- truck_evi_load_profiles[unique_vid %in% evi_fleet[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
            fleet_load_profiles <- id_key[fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id
            fleet_load_profiles[is.na(avg_kw),avg_kw:=0]
            #Add day_of_week information
            weekday_ref <- evi_fleet[!duplicated(unique_vid),day_of_week,by=unique_vid]
            setkey(weekday_ref,unique_vid)
            setkey(fleet_load_profiles,unique_vid)
            fleet_load_profiles <- merge(weekday_ref,fleet_load_profiles,by="unique_vid",all.y=TRUE)
            #### REPEAT FOR DELAYED LOAD SHAPE 
            setkey(truck_evi_delayed_load_profiles,unique_vid,session_id)
            delayed_fleet_load_profiles <- truck_evi_delayed_load_profiles[unique_vid %in% evi_fleet[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
            delayed_fleet_load_profiles <- id_key[delayed_fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id
            setkey(delayed_fleet_load_profiles,unique_vid)
            delayed_fleet_load_profiles <- merge(weekday_ref,delayed_fleet_load_profiles,by="unique_vid",all.y=TRUE)
            # Finally dependingon fracSmartCharge, replace some of the delayed with their original load
            n.veh <- length(u(fleet_load_profiles$unique_vid))
            unmanaged.ids <- sample(u(fleet_load_profiles$unique_vid),n.veh*(1-truckfractionSmartCharging))
            delayed_fleet_load_profiles <- rbindlist(list(delayed_fleet_load_profiles[!unique_vid %in% unmanaged.ids],fleet_load_profiles[unique_vid%in%unmanaged.ids]))
            #remove temp variables
            remove(id_key,weekday_ref)
            # Turn into energy and power constraints
            fleet_load_profiles[,t:=time_of_day]
            delayed_fleet_load_profiles[,t:=time_of_day]
            energy.constraints.unnormalized <- join.on(join.on(data.table(expand.grid(list(t=u(c(fleet_load_profiles$t,delayed_fleet_load_profiles$t)),day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
            setkey(energy.constraints.unnormalized,day_of_week,t)
            energy.constraints.unnormalized[is.na(min.energy),min.energy:=0]
            energy.constraints.unnormalized[is.na(max.energy),max.energy:=0]
            energy.constraints.unnormalized[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
            max.diff <- energy.constraints.unnormalized[,list(diff=max(max.energy-min.energy,na.rm=T)),by='day_of_week']
      
            fleet_load_profiles[,t:=time_of_day%%24]
            delayed_fleet_load_profiles[,t:=time_of_day%%24]
            energy.constraints <- join.on(join.on(data.table(expand.grid(list(t=0:23,day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
            setkey(energy.constraints,day_of_week,t)
            energy.constraints[is.na(min.energy),min.energy:=0]
            energy.constraints[is.na(max.energy),max.energy:=0]
            energy.constraints[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
            energy.constraints <- join.on(energy.constraints,max.diff,'day_of_week','day_of_week')
            energy.constraints[,delta:= diff - max(max.energy-min.energy),by='day_of_week']
            min.energy.initial.offsets <- energy.constraints[,.(min.offset=min(min.energy)),by='day_of_week']
            energy.constraints[,min.energy:=min.energy - delta]
            energy.constraints[,':='(delta=NULL,diff=NULL)]
            #cat(pp("First max energy UB: ",energy.constraints$max.energy[1],"\n"))
            power.constraints <- fleet_load_profiles[,.(max.power=sum(plugged_kw),min.power=0),by='t']
            setkey(power.constraints)
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['energy']] <- energy.constraints
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['energy.min.offsets']] <- min.energy.initial.offsets
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['power']] <- power.constraints
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unscaled.charger.reqs']] <- unscaled.ch.requirements
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['private.fleet.share']] <- private.fleet
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unmanaged.load.by.ch']] <- unmanaged.load.by.ch
            energy.and.power.constraints.cache <- copy(energy.and.power.constraints)
            save(energy.and.power.constraints.cache,file=cache.file)
          }
        }
      }
  
      fleets <- list()
      unmanaged.loads <- list()
      unmanaged.loads.human <- list()
      chargers <- list()
      cumul.energy.constraints <- list()
      cumul.energy.constraints.human <- list()
      for(i in 1:length(days)){
        the.day <- days[i]
        the.season <- toupper(dates$seasons[i])
        the.day.type <- dates$day.types[i]
        if(the.day.type == 'SA/SU'){
          the.evipro.day.type <- "weekend"
        }else{
          the.evipro.day.type <- "weekday"
        }
        the.transit.type <- ifelse(includeTransitDemand,'with_transit','no_transit')
        scaling.factor <- fleet_size / 25000
        energy.constraints <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['energy']])
        energy.constraints[,':='(max.energy=max.energy*scaling.factor,min.energy=min.energy*scaling.factor)]
        if(any(is.na(energy.constraints$min.energy)))stop('here')
        min.energy.initial.offsets <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['energy.min.offsets']])
        min.energy.initial.offsets[,':='(min.offset=min.offset*scaling.factor)]
        power.constraints <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['power']])
        power.constraints[,':='(max.power=max.power*scaling.factor,min.power=min.power*scaling.factor)]
        df <- energy.constraints[day_of_week==the.evipro.day.type]
        setkey(df,t)
        df[,t:=t+(i-1)*24]
        if(i>1){
          maxes <- cumul.energy.constraints[[length(cumul.energy.constraints)]][,list(max.max=max(max.energy),max.min=max(min.energy))]
          mins <- df[,list(the.min=min(min.energy))]
          df[,max.energy:=max.energy+maxes$max.max]
          df[,min.energy:=min.energy+maxes$max.min - mins$the.min + min.energy.initial.offsets[day_of_week==the.evipro.day.type]$min.offset]
        }
        #cat(pp("First max energy for day ",the.day,": ",df$max.energy[1],"\n"))
        df[,max.power:=power.constraints$max.power]
        df[,min.power:=power.constraints$min.power]
        #ggplot(df,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
        
        #for human driven factors
        df1 <- df
        df1[,max.energy:=max.energy*truckhumanchargingfactor]
        df1[,min.energy:=min.energy*truckhumanchargingfactor]
        df1[,max.power:=max.power*truckhumanchargingfactor]
        df1[,min.power:=min.power*truckhumanchargingfactor]
        
        
        #ggplot(df,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
        
        cumul.energy.constraints[[length(cumul.energy.constraints)+1]] <- df
        cumul.energy.constraints.human[[length(cumul.energy.constraints)+1]] <- df1
        chargers[[length(chargers)+1]] <- energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unscaled.charger.reqs']][,.(dest_type,dest_chg_level,n.ch=req*scaling.factor,day=i)]
        fleets[[length(fleets)+1]] <- energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['private.fleet.share']][,.(name,n.veh=share*fleet_size,day=i)]
        unmanaged.loads[[length(unmanaged.loads)+1]] <- energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unmanaged.load.by.ch']][,.(power,hr,l,day=i)]
        loads.human <-energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unmanaged.load.by.ch']][,.(power,hr,l,day=i)]
        loads.human[,power:= power * rep(truckhumanchargingfactor,times=3)]
        unmanaged.loads.human[[length(unmanaged.loads.human)+1]] <- loads.human
      }
      cumul.energy.constraints <- rbindlist(cumul.energy.constraints)
      cumul.energy.constraints.human <- rbindlist(cumul.energy.constraints.human)
      chargers <- rbindlist(chargers)[,.(n.ch=max(n.ch)),by=c('dest_type','dest_chg_level')]
      fleets <- rbindlist(fleets)[,.(n.veh=mean(n.veh)),by=c('name')]
      unmanaged.loads <- rbindlist(unmanaged.loads)
      unmanaged.loads.human <- rbindlist(unmanaged.loads.human)
      
      #p <- ggplot(cumul.energy.constraints,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
      #ggsave(pp(substr(cache.file,1,nchar(cache.file)-6),'.pdf'),p,width=10,height=8,units='in')
      
      cumul.energy.constraints[,t:=pp('t',sprintf('%04d',t+1))]
      cumul.energy.constraints[,rmob:=the.region]
      cumul.energy.constraints.human[,t:=pp('t',sprintf('%04d',t+1))]
      cumul.energy.constraints.human[,rmob:=the.region]
      chargers[,rmob:=the.region]
      fleets[,rmob:=the.region]
      all.chargers[[length(all.chargers)+1]] <- chargers
      all.unmanaged.loads[[length(all.unmanaged.loads)+1]] <- unmanaged.loads
      all.unmanaged.loads.human[[length(all.unmanaged.loads.human)+1]] <- unmanaged.loads.human
      all.fleets[[length(all.fleets)+1]] <- fleets
      all.all.energy.constraints[[length(all.all.energy.constraints)+1]] <- copy(cumul.energy.constraints)
      all.all.energy.constraints.human[[length(all.all.energy.constraints.human)+1]] <- copy(cumul.energy.constraints.human)
    }
    all.all.energy.constraints <- rbindlist(all.all.energy.constraints)
    all.all.energy.constraints.human <- rbindlist(all.all.energy.constraints.human)
    all.chargers <- rbindlist(all.chargers)
    all.fleets <- rbindlist(all.fleets)
    all.unmanaged.loads <- rbindlist(all.unmanaged.loads)
    all.unmanaged.loads.human <- rbindlist(all.unmanaged.loads.human)
    inputs <- list()
    inputs$sets <- list()
    inputs$parameters <- list()
    inputs$parameters$truckpersonalEVChargeEnergyUB <- all.all.energy.constraints[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*ifelse(max.energy>=min.energy,max.energy,min.energy))]
    inputs$parameters$truckpersonalEVChargeEnergyLB <- all.all.energy.constraints[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*min.energy)]
    inputs$parameters$truckpersonalEVChargePowerUB <- all.all.energy.constraints[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*max.power)]
    inputs$parameters$truckpersonalEVChargePowerLB <- all.all.energy.constraints[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*min.power)]
    inputs$parameters$truckpersonalEVFleetSize <- all.fleets[,.(rmob,value=n.veh,type=name)]
    inputs$parameters$truckpersonalEVUnmanagedLoads <- all.unmanaged.loads[,.(l,t=hr,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*power)]
    inputs$parameters$truckpersonalEVChargers <- all.chargers[,.(rmob,value=n.ch,type=dest_type,level=dest_chg_level)]
    inputs$parameters$truckhumanEVChargeEnergyUB <- all.all.energy.constraints.human[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*ifelse(max.energy>=min.energy,max.energy,min.energy))]
    inputs$parameters$truckhumanEVChargeEnergyLB <- all.all.energy.constraints.human[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*min.energy)]
    inputs$parameters$truckhumanEVChargePowerUB <- all.all.energy.constraints.human[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*max.power)]
    inputs$parameters$truckhumanEVChargePowerLB <- all.all.energy.constraints.human[,list(t,rmob,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*min.power)]
    inputs$parameters$truckhumanEVFleetSize <- all.fleets[,.(rmob,value=n.veh,type=name)]
    inputs$parameters$truckhumanEVUnmanagedLoads <- all.unmanaged.loads.human[,.(l,t=hr,value=truckconversionEfficiencyScalingFactor*truckvmtReboundFactor*power)]
    inputs$parameters$truckhumanEVChargers <- all.chargers[,.(rmob,value=n.ch,type=dest_type,level=dest_chg_level)]
    # what is average utilization rate for the private charging infrastructure
    kwh.per.day <- inputs$parameters$truckpersonalEVChargeEnergyLB[,.(kwh.per.day=sum(diff(value))/(length(u(t))/24)),by='rmob']
    charger.utilization <- join.on(all.chargers[,.(n=sum(n.ch)),by=c('dest_chg_level','rmob')],kwh.per.day,'rmob','rmob')
    charger.utilization[,kw:=ifelse(dest_chg_level=='L1',5,ifelse(dest_chg_level=='L2',20,100))]
    charger.utilization <- charger.utilization[,.(rate=kwh.per.day[1]/sum(n*kw*24)),by='rmob']
    inputs$parameters$truckchargerUtilization <- charger.utilization[,.(rmob,value=rate)]
  }else{
    zero <- data.table(expand.grid(t=common.inputs$sets$t,rmob=common.inputs$sets$rmob,value=0)) 
    inputs$parameters$truckpersonalEVChargeEnergyUB <- zero
    inputs$parameters$truckpersonalEVChargeEnergyLB <- zero
    inputs$parameters$truckpersonalEVChargePowerUB <- zero
    inputs$parameters$truckpersonalEVChargePowerLB <- zero
    inputs$parameters$truckpersonalEVFleetSize <- data.table(expand.grid(rmob=common.inputs$sets$rmob,value=0,type=c('PHEV20','PHEV50','BEV600','BEV800'))) 
    inputs$parameters$truckpersonalEVChargers <- data.table(expand.grid(t=common.inputs$sets$t,rmob=common.inputs$sets$rmob,value=0,type=c(type=c('Public','Home','Work')),level=c('L1','L2','L3')))
    inputs$parameters$truckchargerUtilization <- data.table(expand.grid(rmob=common.inputs$sets$rmob,value=0)) 
#    inputs$parameters$truckpersonalEVUnmanagedLoads<-data.table(expand.grid(l=c('L1','L2','L3'),t=common.inputs$sets$t,value=0))
    inputs$parameters$truckhumanEVChargeEnergyUB <- zero
    inputs$parameters$truckhumanEVChargeEnergyLB <- zero
    inputs$parameters$truckhumanEVChargePowerUB <- zero
    inputs$parameters$truckhumanEVChargePowerLB <- zero
    inputs$parameters$truckpersonalEVUnmanagedLoads <- zero
    inputs$parameters$truckhumanEVUnmanagedLoads <- zero
    inputs$parameters$truckhumanEVFleetSize <- data.table(expand.grid(rmob=common.inputs$sets$rmob,value=0,type=c('PHEV20','PHEV50','BEV600','BEV800'))) 
    inputs$parameters$truckhumanEVChargers <- data.table(expand.grid(t=common.inputs$sets$t,rmob=common.inputs$sets$rmob,value=0,type=c(type=c('Public','Home','Work')),level=c('L1','L2','L3')))
    
  }
  inputs
}
