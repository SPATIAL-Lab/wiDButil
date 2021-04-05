library(openxlsx)
library(readxl)
library(sp)
library(rgdal)
library(raster)

server = function(input, output){
  options(stringsAsFactors = FALSE)
  
  #list of sheets for download
  sheets_list <- reactive({
    s <- finishedSheets()
  })
  
  #removes duplicates and matches siteID to samples 
  conSites = function(sites, samples, siteField = "Site_ID", latField = "Latitude", lonField = "Longitude"){
    sites = sites[order(sites[,4]),]
    sites = sites[order(sites[,3]),]
    for(i in 1:nrow(sites)){
      if(i == 1){
        cid = sites[i, 1]
      } else if(sites[i, 3] != sites[i-1, 3] | sites[i, 4] != sites[i-1, 4]){
        cid = sites[i, 1]
      } else{
        oid = sites[i, 1]
        sites[i, 1] = cid
        ind = samples[,3] == oid
        samples[ind, 3] = cid
      }
    }
    sites = sites[!duplicated(sites[,1]),]
    samples = samples[!duplicated(samples[,3]),]
    return(list(sites, samples))
  }
  
  #gets rows of site sheet that need the country resolved
  ct <- reactive({
    sites <- sitesSheet()
    cty <- readOGR("TM_WORLD_BORDERS-0.3.shp")
    
    sites.sp <- SpatialPointsDataFrame(data.frame(sites$Longitude, sites$Latitude),
                                       data = sites, proj4string = crs(cty))
    cty <- readOGR("TM_WORLD_BORDERS-0.3.shp")
    mcs = over(sites.sp, cty)
    #Use this to track whether the site has already been resolved, allowing
    #stop/restart for developers
    mcs$RES = rep(FALSE)
    #Count the number of missing or mis-matched values
    ct = 0
    for(i in seq_along(sites.sp)){
      if(is.na(sites.sp$Country[i]) | is.na(mcs$ISO2[i])){
        ct = ct + 1
      }
      else if(sites.sp$Country[i] != mcs$ISO2[i]){
        ct = ct + 1
      }
    }
    ct <- ct
  })
  
  uniquect <- reactive({
    sites <- sitesSheet()
    us <- unique(sites$Country)
  })
  
  mod <- eventReactive(input$done3, {
    sites <- sitesSheet()
    cty <- readOGR("TM_WORLD_BORDERS-0.3.shp")
    uniquect <- uniquect()
    if(input$startcc == "Site") { #length(uniquect) > 10
      sites.sp <- SpatialPointsDataFrame(data.frame(sites$Longitude, sites$Latitude),
                                          data = sites, proj4string = crs(cty))
      mcs = over(sites.sp, cty)
      
      for(i in seq_along(sites.sp)){
        map.code = mcs$ISO2[i]
        if(map.code == "MISSING"){
          mcs$Recommendation = "Delete or type new code"
        }
        #If there is a map code for the location
        else{
          mcs$Recommendation = map.code
        }
      }
      mod <- data.frame(Site_ID = sites$Site_ID, Latitude = sites$Latitude, Longitude = sites$Longitude, Original_Country = sites$Country, Recommendation = mcs$Recommendation, Choice = mcs$Recommendation)
    }
    else {
      sites <- sites[!duplicated(sites$Country),]
      sites.sp <- SpatialPointsDataFrame(data.frame(sites$Longitude, sites$Latitude),
                                         data = sites, proj4string = crs(cty))
      mcs = over(sites.sp, cty)
      
      for(i in seq_along(sites.sp)){
        map.code = mcs$ISO2[i]
        if(map.code == "MISSING"){
          mcs$Recommendation = "Delete or type new code"
        }
        #If there is a map code for the location
        else{
          mcs$Recommendation = map.code
        }
      }
      mod <- data.frame(Original_Country = sites$Country, Recommendation = mcs$Recommendation, Choice = mcs$Recommendation)
    }
  })
  
  output$mod_table <- DT::renderDataTable({
      mod <- mod()
      DT::datatable(mod, editable = TRUE)
  })
  
  proxy = DT::dataTableProxy('mod_table')
  
  observeEvent(input$mod_table_cell_edit, {
    mod = mod()
    info = input$mod_table_cell_edit
    str(info)
    mod <<- DT::editData(mod, info)
    sites <- sitesSheet()
    i = info$row
    ncc = info$value
    if (input$startcc == "Country") {
      cc <- mod[i, which ( colnames(mod) == "Original_Country")]
      sites$Country[sites$Country == cc] <- ncc
    }
    else {
      sites$Country[i] <- ncc
      sites <- sites
    }
  })
  
  finishedSheets <- reactive({
    ss <- sitesSheet()
    sms <- fixedTypeSheet()
    s <- conSites(ss, sms)
  })
  
  output$ss <- renderTable({
    sheets <- finishedSheets()
    s <- sheets[1]
  })
  
  output$sss <- renderTable({
    sheets <- finishedSheets()
    s <- sheets[2]
  })
  
  #downloads sheets
  output$d1 <- downloadHandler(
    filename = function() {"new1.xlsx"},
    content = function(file) { 
      writexl::write_xlsx(sheets_list(), file)}
  )
  
  #takes in user input of table
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    file.rename(file1$datapath,
                paste(file1$datapath, ".xlsx", sep=""))
    if(input$one == TRUE && input$header == TRUE){
      read_excel(paste(file1$datapath, ".xlsx", sep=""), 1, col_names = input$header, )
    }
    else{
      shinyalert::shinyalert("Error", "Please reformat file so that all data is stored on one sheet and header is in the first row.", type= "error")
    }
    
  })
  
  #siteid column chosen by user. if "N/A" then we will assign siteids. 
  siteid <- reactive({
    sid <- input$siteID
    df <- data()
    pn <- input$pname
    if(sid == "N/A"){
      sn = seq_along(1:nrow(df))
      sn = formatC(sn, width = 4, format = "d", flag = "0")
      SITEID <- data.frame(paste0(pn, "_site_", sn)) 
    }
    else{
      SITEID <- df[, which(colnames(df) == sid)]
    }
  })
  
  #longitude input
  longitude <- reactive({
    df <- data()
    LONG <- df[, which(colnames(df) == input$long)]
  })
  
  #latitude input
  latitude <- reactive({
    df <- data()
    LAT <- df[, which(colnames(df) == input$lat)]
  })
  
  #renders table 
  output$table <- renderTable({
    if(is.null(data()))
    {return ()}
    data()
  })
  
  #function that checks that latitude is in correct range
  checkLat = function(dataframe, id){
    for(i in 1:length(dataframe)){
      if(dataframe[i,which(colnames(dataframe) == id)] >= 90 || 
         dataframe[i,which(colnames(dataframe) == id)] <= -90){
        return (FALSE); 
      }
    }
    return (TRUE);
  }
  
  #function that checks that longitude is in correct range
  checkLong = function(dataframe, id){
    for(i in 1:length(dataframe)){
      if((dataframe[i,which(colnames(dataframe) == id)] >= 180) || 
         (dataframe[i,which(colnames(dataframe) == id)] <= -180)){
        return (FALSE);
      }
    }
    return (TRUE);
  }
  
  #function that checks if lat/long field are numeric
  checkNumeric = function(dataframe, id){
    L <- unlist(dataframe[,which(colnames(dataframe) == id)])
    L <- na.omit(L)
    for(i in 1:length(L)){
      L[i] <- as.numeric(L[i])
    }
    return(L);
  }
  
  #function that checks if data uses n/s/e/w convention
  nsewMatch = function(dataframe, id) {
    L <- unlist(dataframe[!is.numeric(dataframe), which(colnames(dataframe) == id)])
    L <- na.omit(L) 
    
    # Matches strings that use N,S,E,W instead of + or -
    nsewRegex <- "[nNsSeEwW]"
    #^[nNsSeEwW]\\D+.?\\D*|^\\D+.?\\D*[nNsSeEwW]"
    return (grep(nsewRegex, L,))
  }
  
  #function that checks if data uses degrees/min/sec instead of decimal degrees
  dmsMatch = function(dataframe, id) {
    L <- unlist(dataframe[!is.numeric(dataframe), which(colnames(dataframe) == id)])
    L <- na.omit(L) 
    
    # Matches strings that use degrees min sec instead of dd
    dmsRegex <- "([+-])?([0-8]?[0-9]|90)\u00B0\\s*([0-5]?[0-9])'\\s*([0-5]?[0-9].?[0-9]*\")"
    return (grep(dmsRegex, L,))
  }
  
  
  #site sheet
  observeEvent(input$done, {
    df <- data()
    
    #checks if lat/long are numeric
    LAT <- checkNumeric(df, input$lat)
    LONG <- checkNumeric(df, input$long)
    tryCatch(
      expr = {
        showModal(modalDialog(
          title = "Warning", 
          "Any rows without longitude or latitude values will be deleted.", 
          footer = actionButton("dismiss", label = "OK")
        ))
      },
      error = function(e){ 
        # Do this if an error is caught...
        shinyalert::shinyalert(title = "Error", 
                               "The column you selected for latitude/longitude has values that are not numeric.", 
                               type = "error", inputId = "nonnum", showConfirmButton = TRUE)
      },
      warning = function(w){
        # Do this if an warning is caught...
        shinyalert::shinyalert(title = "Warning", 
                               paste0(w), 
                               type = "warning", inputId = "w", showConfirmButton = TRUE)
      }
    )
    
  })
  
  sitesSheet <- eventReactive(input$dismiss, {
    removeModal()    
    if(!is.null(input$file)){
      df <- data()
      SID <- siteid()
      
      
      #matches user input to column in inputted dataframe
      SITENAME <- df[, which(colnames(df) == input$sn)]
      LAT <- df[, which(colnames(df) == input$lat)]
      LONG <- df[,which(colnames(df) == input$long)]
      ELEV <- df[, which(colnames(df) == input$elv)]
      ADDY <- df[, which(colnames(df) == input$addy)]
      CITY <- df[, which(colnames(df) == input$cityy)]
      STATE <- df[, which(colnames(df) == input$stateProvince)]
      COUNTRY <- df[, which(colnames(df) == input$cc)]
      COMMENT <- df[, which(colnames(df) == input$sc)]
      
      #alerts user that any rows without lat/long values will be deleted.
      warning("Any rows without longitude or latitude values will be deleted.")
      
      #checks if lat/long are formatted with N/S/E/W instead of (+) and (-)
      nsewMatchLAT <- nsewMatch(df, input$lat)
      nsewMatchLONG <- nsewMatch(df, input$long)
      
      if (length(nsewMatchLAT) > 0 || length(nsewMatchLONG) > 0) {
        
        warning("Your data is formatted using N/S/E/W instead of +/-. Any rows with this formatting will be changed to +/- formatting")
        
        if(length(nsewMatchLAT) > 0) {
          for (i in 1:length(nsewMatchLAT)) {
            lat <- df[nsewMatchLAT[i], which(colnames(df) == input$lat)]
            nd <- stringr::str_detect(lat, "[nN]",)
            sd <- stringr::str_detect(lat, "[sS]",)
            if (!is.na(nd) && nd == TRUE) {
              df[nsewMatchLAT[i], which(colnames(df) == input$lat)] <- paste0('+', gsub("[nN]", "", lat))
            }
            if (!is.na(sd) && sd == TRUE) {
              df[nsewMatchLAT[i], which(colnames(df) == input$lat)] <- paste0('-', gsub("[sS]", "", lat))
            }
          }
        }
        if (length(nsewMatchLONG) > 0) {
          for (j in 1:length(nsewMatchLONG)) {
            long <- df[nsewMatchLONG[j], which(colnames(df) == input$long)]
            ed <- stringr::str_detect(long, "[eE]",)
            wd <- stringr::str_detect(long, "[wW]",)
            if (!is.na(ed) && ed == TRUE) {
              df[nsewMatchLONG[j], which(colnames(df) == input$long)] <- paste0('+', gsub("[eE]", "", long))
            }
            if (!is.na(wd) && wd == TRUE) {
              df[nsewMatchLONG[j], which(colnames(df) == input$long)] <- paste0('-', gsub("[wW]", "",long))
            }
          }
        }
      }
      
      #checks if lat/long are formatted as degrees/min/sec instead of decimal degrees
      dmsMatchLAT <- dmsMatch(df, input$lat)
      dmsMatchLONG <- dmsMatch(df, input$long)
      
      if (length(dmsMatchLAT) > 0 || length(dmsMatchLONG) > 0) {
        
        warning("Your data is formatted using DMS formatting instead of DD. Any rows with this formatting will be changed to DD.")
      
        lat <- df[, which(colnames(df) == input$lat)]
        long <- df[, which(colnames(df) == input$long)]
        
        for (i in 1:nrow(lat)) {
          #in order to parse into dd, must remove delimiters and make sure there are no extra spaces
          l <- gsub("[\u00B0,',\"]", "", lat[i,])
          lo <- gsub("[\u00B0,',\"]", "", long[i,])
          l <- gsub("\\s+"," ", l)
          lo <- gsub("\\s+"," ", lo)
          
          
          df[i, which(colnames(df) == input$lat)] <- measurements::conv_unit(l, from = 'deg_min_sec', to = 'dec_deg')
          df[i, which(colnames(df) == input$long)] <- measurements::conv_unit(lo, from = 'deg_min_sec', to = 'dec_deg')
        }
      }
      
      #checks if lat/long are numeric
      LAT <- checkNumeric(df, input$lat)
      LONG <- checkNumeric(df, input$long)
      
      #if lat/long vectors still contain non-numeric values at this point, then we throw an error
      if (is.na(LAT) == TRUE || is.na(LONG) == TRUE) {
        stop("The column you selected for latitude/longitude has values that are not numeric. Please edit your file and retry.")
      }
      
      #checks if lat/long values are within ranges. currently only throws an error.
      lat <- df[, which(colnames(df) == input$lat)]
      long <- df[, which(colnames(df) == input$long)]
      
      latOver90 <- which(lat > 90 || lat < -90)
      longOver180 <- which(long > 180 || long < -180)
      
      if (length(latOver90) > 0 || length(longOver180) > 0) {
        stop("You have latitudes/longitudes outside of expected range ([-90,90] and [-180,180])")
      }
      
      if(input$sn == "N/A"){
        SITENAME <- rep(NA)
      }
      if(input$elv == "N/A"){
        ELEV <- rep(NA)
      }
      if(input$addy == "N/A"){
        ADDY <- rep(NA)
      }
      if(input$cityy == "N/A"){
        CITY <- rep(NA)
      }
      if(input$stateProvince == "N/A"){
        STATE <- rep(NA)
      }
      if(input$cc == "N/A"){
        COUNTRY <- rep(NA)
      }
      if(input$sc == "N/A"){
        COMMENT <- rep(NA)
      }
      
      #creates sites sheet
      siteHeader <- c("Site_ID", "Site_Name", "Latitude", "Longitude", "Elevation_mabsl", 
                      "Address", "City", "State_or_Province", "Country", "Site_Comments")
      dff <- data.frame(SID, SITENAME, LAT, LONG, ELEV,
                        ADDY, CITY,  STATE, COUNTRY,  COMMENT)
      names(dff) <- siteHeader
      
      #removes rows without long/lat. needs to display a pop up dialogue.
      dfff <- subset(dff, !is.na(Longitude) | !is.na(Latitude), )
    }
  })
  
  #renders site table
  output$siteTable <- renderTable(sitesSheet(), rownames = TRUE)
  
   
  #samples sheet
  observeEvent(input$done1, {
    showModal(modalDialog(
      title = "Warning", "Any rows without a Type value will be deleted.", footer = actionButton("dismiss1", label = "OK")
    ))
  })
  
  #replaces user's type params w ours
  fixedTypeSheet <- eventReactive(input$done2, {
    samples <- sampleSheet()
    types <- types()
    lengthN <- numtype()
    
    for(i in 1:lengthN) {
       samples$Type <- gsub(types[i], input[[paste0('types',i)]], samples$Type)
    }
    
    samples <- samples
  })
  
  sampleSheet <- eventReactive(input$dismiss1, {
    removeModal()
    if(!is.null(input$file)){
      dframe <- data()
      sid <- siteid()
      
      #matches user input with column in inputted dataframe
      SAMPLEID <- dframe[, which(colnames(dframe) == input$ssid)]
      TYPE <- dframe[, which(colnames(dframe) == input$tt)]
      STARTDATE <- dframe[, which(colnames(dframe) == input$sd)]
      STARTTZ <- dframe[, which(colnames(dframe) == input$stz)]
      COLD <- dframe[, which(colnames(dframe) == input$cd)]
      COLTZ <- dframe[, which(colnames(dframe) == input$ctz)]
      SAMPLEVOL <- dframe[, which(colnames(dframe) == input$sml)]
      COLTYPE <- dframe[, which(colnames(dframe) == input$ct)]
      PHASE <- dframe[, which(colnames(dframe) == input$ph)]
      DEPTH <- dframe[, which(colnames(dframe) == input$dm)]
      SAMPLESOURCE <- dframe[, which(colnames(dframe) == input$sampS)]
      SAMPLEIGNORE <- dframe[, which(colnames(dframe) == input$sampI)]
      SAMPLECOM <- dframe[, which(colnames(dframe) == input$sampC)]
      PROJECTID <- dframe[, which(colnames(dframe) == input$pID)]
      
      if(input$ssid == "N/A"){
        SAMPLEID <- rep(NA)
      }
      if(input$tt == "N/A"){
        TYPE <- rep(NA)
      }
      if(input$sd == "N/A"){
        STARTDATE <- rep(NA)
      }
      if(input$stz == "N/A"){
        STARTTZ <- rep(NA)
      }
      if(input$cd == "N/A"){
        COLD <- rep(NA)
      }
      if(input$ctz == "N/A"){
        COLTZ <- rep(NA)
      }
      if(input$sml == "N/A"){
        SAMPLEVOL <- rep(NA)
      }
      if(input$ct == "N/A"){
        COLTYPE <- rep(NA)
      }
      if(input$ph == "N/A"){
        PHASE <- rep(NA)
      }
      if(input$dm == "N/A"){
        DEPTH <- rep(NA)
      }
      if(input$sampS == "N/A"){
        SAMPLESOURCE <- rep(NA)
      }
      if(input$sampI == "N/A"){
        SAMPLEIGNORE <- rep(NA)
      }
      if(input$sampC == "N/A"){
        SAMPLECOM <- rep(NA)
      }
      if(input$pID == "N/A"){
        PROJECTID <- rep(NA)
      }
      
      #throwing error "object cannot be coerced to type double"
      #STARTDATE <- convertToDate(STARTDATE)
      #COLD <- convertToDate(COLD)
      
      #create samples sheet
      sampleHeader <- c("SampleID", "Sample_ID_2", "Site_ID", "Type", "Start_Date", "Start_Time_Zone", 
                        "Collection_Date", "Collection_Time_Zone", "Sample_Volume_ml", "Collector_type", 
                        "Phase", "Depth_meters", "Sample_Source", "Sample_Ignore", "Sample_Comment", "Project_ID")
      d <- data.frame(SAMPLEID, SAMPLEID, sid, TYPE, STARTDATE, STARTTZ,
                      COLD, COLTZ, SAMPLEVOL, COLTYPE, PHASE, DEPTH, SAMPLESOURCE,
                      SAMPLEIGNORE, SAMPLECOM, PROJECTID)
      names(d) <- sampleHeader
      
      #removes rows without a type
      dd <- subset(d, !is.na(Type), )
    }
    
  })
  
  #renders sample table
  output$sampleTable <- renderTable(sampleSheet(), rownames = TRUE)
  
  #renders sample table with fixed type 
  output$sampleTable2 <- renderTable(fixedTypeSheet(), rownames = TRUE)
  
  #gets unique type list
  types  <- eventReactive(input$done1, {  
    samples <- sampleSheet()
    t <- samples[,which(colnames(samples) == 'Type')]
    type <- unique(t)
  })
  
  numtype <- reactive({
    types <- types()
    nt <- length(types)
  })
  
  #dynamically creates drop-down lists for each unique water type, 
  #so that user can match their names to ours
  output$watertype <- renderUI({
    types <- types()
    lengthN <- numtype()
    lapply(1:lengthN, function(i) {
      selectInput(paste0('types',i), types[i], choices = c("Bottled","Canal","Ground","Lake","Leaf","Mine","Ocean",
                "Precipitation","River_or_stream","Snow_pit","Soil",
                "Spring","Stem","Sprinkler", "Tap", "Vapor"))
    })
  })
  
  #function that rearranges names vector if column that might be field is found 
  fieldMatch = function(d, list){
    if(!is.null(d)){
      cn <- names(d)
      for(i in 1:length(cn)){
        if(tolower(cn[i]) %in% list){
          l = cn[i]
          cn <- cn[cn != cn[i]]
          cn <- append(cn, l, after = 0)
        }
      }
      return( cn <- as.list(cn))
    }
  }
  
  #function that rearranges names vector if column that might be field is found
  fieldMatchNA = function(d, list){
    if(!is.null(d)){
      cn <- names(d)
      for(i in 1:length(cn)){
        if(tolower(cn[i]) %in% list){
          l = cn[i]
          cn <- cn[cn != cn[i]]
          cn <- append(cn, l, after = 0)
          return( cn <- as.list(cn))
        }
      } 
      cn <- c("N/A", cn)
      return( cn <- as.list(cn))
    }
  }
  
  
  #field matching
  #site fields
  siteidd <- reactive({
    sn = c("site_id", "site id", "id")
    fieldMatchNA(data(), sn)
  })
  #rearranges names vector if column that might be sitename is found 
  siteName <- reactive({
    sn = c("site_name", "name", "sitename")
    fieldMatchNA(data(), sn)
  })
  
  #rearranges names vector if column that might be latitude is found 
  lat <- reactive({
    latitude = c("latitude", "lat")
    fieldMatch(data(), latitude)
  })
  
  #rearranges names vector if column that might be longitude is found 
  long <- reactive({
    longitude = c("longitude", "long", "lon")
    fieldMatch(data(), longitude)
  })
  
  #rearranges names vector if column that might be country is found
  country <- reactive({
    Country = c("country")
    fieldMatchNA(data(), Country)
  })
  
  #rearranges names vector if column that might be elevation is found 
  elev <- reactive({
    elevation = c("elevation", "elev", "elevation_mabsl")
    fieldMatchNA(data(), elevation)
  })
  
  #rearranges names vector if column that might be state/province is found 
  state <- reactive({
    states = c("state","province", "state_or_province")
    fieldMatchNA(data(), states)
  })
  
  #rearranges names vector if column that might be city is found 
  city <- reactive({
    City = c("city", "municipality")
    fieldMatchNA(data(), City)
  })
  
  #rearranges names vector if column that might be address is found 
  address <- reactive({
    a = c("address", "location")
    fieldMatchNA(data(), a)
  })
  
  #rearranges names vector if column that might be site comment is found 
  sitecomment <- reactive({
    d = c("site_comment", "sitecomment", "site comment", "comment", "comments", "site_comments", "site comments")
    fieldMatchNA(data(), d)
  })
  
  
  #sample fields
  #rearranges names vector if column that might be type is found 
  typee <- reactive({
    type = c("type", "source", "water source", "water_source", "water_type", "water type", "sample type", "sample_type")
    fieldMatch(data(), type)
  })
  
  #rearranges names vector if column that might be phase is found 
  phase <- reactive({
    Phase = c("phase", "water phase")
    fieldMatchNA(data(), Phase)
  })
  
  #rearranges names vector if column that might be collection date is found 
  collectionDate <- reactive({
    cd = c("collection date", "date collected", "date", "collection_date", "date_collected")
    fieldMatchNA(data(), cd)
  })
  
  #rearranges names vector if column that might be start date is found 
  startDate <- reactive({
    sd = c("start date", "date", "start", "start_date")
    fieldMatchNA(data(), sd)
  })
  
  #rearranges names vector if column that might be depth is found 
  depth <- reactive({
    d = c("depth", "depth_meters", "depth meters")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be start time zone is found 
  startTimezone <- reactive({
    d = c("timezone", "start_time_zone", "start", "start_timezone", "start timezone")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be collection time zone is found 
  collectionTimezone <- reactive({
    d = c("timezone", "collection_time_zone", "collection", "collection_timezone", "collection timezone")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be sample_Vol is found 
  samplevolume <- reactive({
    d = c("samplevolume", "sample_volume", "sample volume", "sample_volume_ml", "volume", "ml")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be collector type is found 
  collectortype <- reactive({
    d = c("collector", "collector_type", "collector type")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be sample source is found 
  samplesource <- reactive({
    d = c("sample_source", "sample source", "source")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be sample ignore is found 
  sampleignore <- reactive({
    d = c("sample ignore", "ignore", "sample_ignore")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be sample comment is found 
  samplecomment <- reactive({
    d = c("comment", "sample comment", "sample_comment", "comments", "sample comments", "sample_comments")
    fieldMatchNA(data(), d)
  })
  
  #rearranges names vector if column that might be project ID is found 
  projectid <- reactive({
    d = c("project_id", "project", "id", "project id")
    fieldMatchNA(data(), d)
  })
  
  
  #Sites drop down lists  
  
  output$projectname <- renderUI({
    textInput('pname', 'ID Prefix',)
  })
  output$id <- renderUI({
    selectInput('siteID', 'Site_ID', choices = c(siteidd()))
  })
  output$sitename <- renderUI({
    selectInput('sn', 'Site_Name', choices = c(siteName()))
  })
  output$latitude <- renderUI({
    selectInput('lat', 'Latitude', choices = c(lat()))
  })
  output$longitude <- renderUI({
    selectInput('long', 'Longitude', choices = c(long()))
  })
  output$elevation <- renderUI({
    selectInput('elv', 'Elevation_mabsl', choices = c(elev()))
  })
  output$address <- renderUI({
    selectInput('addy', 'Address', choices = c(address()))
  })
  output$city <- renderUI({
    selectInput('cityy', 'City', choices = c(city()))
  })
  output$state <- renderUI({
    selectInput('stateProvince', 'State_or_Province', choices = c(state()))
  })
  output$country <- renderUI({
    selectInput('cc', 'Country', choices = c(country()))
  })
  output$scomment <- renderUI({
    selectInput('sc', 'Site_Comment', choices = c(sitecomment()))
  })
  
  
  #Samples drop down lists
  
  output$sid <- renderUI({
    selectInput('ssid', 'Sample_ID', choices = c(names(data())))
  })
  output$type <- renderUI({
    selectInput('tt', 'Type', choices = c(typee()))
  })
  output$startD <- renderUI({
    selectInput('sd', 'Start_Date', choices = c(startDate()))
  })
  output$startTZ <- renderUI({
    selectInput('stz', 'Start_Time_Zone', choices = c(startTimezone()))
  })
  output$collectionD <- renderUI({
    selectInput('cd', 'Collection_Date', choices = c(collectionDate()))
  })
  output$collectionTZ <- renderUI({
    selectInput('ctz', 'Collection_Time_Zone', choices = c(collectionTimezone()))
  })
  output$sampleVol <- renderUI({
    selectInput('sml', 'Sample_Volume_ml', choices = c(samplevolume()))
  })
  output$collectT <- renderUI({
    selectInput('ct', 'Collector_type', choices = c(collectortype()))
  })
  output$phase <- renderUI({
    selectInput('ph', 'Phase', choices = c(phase()))
  })
  output$depth <- renderUI({
    selectInput('dm', 'Depth_meters', choices = c(depth()))
  })
  output$sampleSource <- renderUI({
    selectInput('sampS', 'Sample_Source', choices = c(samplesource()))
  })
  output$sampIgnore <- renderUI({
    selectInput('sampI', 'Sample_Ignore', choices = c(sampleignore()))
  })
  output$sampCom <- renderUI({
    selectInput('sampC', 'Sample_Comments', choices = c(samplecomment()))
  })
  output$projectID <- renderUI({
    selectInput('pID', 'Project_ID', choices = c(projectid()))
  })
  
  
  #Samples type drop down lists
  
  output$bottle <- renderUI({
    selectInput('bot', 'Bottle', choices = c('NA', types()))
  })
  output$canal <- renderUI({
    selectInput('can', 'Canal', choices = c(types(), 'NA'))
  })
  output$ground <- renderUI({
    selectInput('gr', 'Ground', choices = c(types(), 'NA'))
  })
  output$lake <- renderUI({
    selectInput('lak', 'Lake', choices = c(types(), 'NA'))
  })
  output$leaf <- renderUI({
    selectInput('lef', 'Leaf', choices = c(types(), 'NA'))
  })
  output$mine <- renderUI({
    selectInput('mi', 'Mine', choices = c(types(), 'NA'))
  })
  output$ocean <- renderUI({
    selectInput('oc', 'Ocean', choices = c(types(), 'NA'))
  })
  output$precip <- renderUI({
    selectInput('pre', 'Precipitation', choices = c(types(), 'NA'))
  })
  output$river <- renderUI({
    selectInput('riv', 'River_or_stream', choices = c(types(), 'NA'))
  })
  output$snow <- renderUI({
    selectInput('sno', 'Snow_pit', choices = c(types(), 'NA'))
  })
  output$soil <- renderUI({
    selectInput('soi', 'Soil', choices = c(types(), 'NA'))
  })
  output$spring <- renderUI({
    selectInput('spri', 'Spring', choices = c(types(), 'NA'))
  })
  output$stem <- renderUI({
    selectInput('ste', 'Stem', choices = c(types(), 'NA'))
  })
  output$sprinkler <- renderUI({
    selectInput('sprin', 'Sprinkler', choices = c(types(), 'NA'))
  })
  output$tap <- renderUI({
    selectInput('tapp', 'Tap', choices = c(types(), 'NA'))
  })
  output$vapor <- renderUI({
    selectInput('vap', 'Vapor', choices = c(types(), 'NA'))
  })
  
  #countries
  output$startCC <- renderUI({
    ct<- ct()
    uniquect <- uniquect()
    selectInput('startcc', paste(ct, "sites to review from ", length(uniquect), " countries. Review by country or site? "), choices = c('Country', 'Site'))
  })
}
