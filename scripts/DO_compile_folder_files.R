#from https://rdrr.io/github/mnsentinellakes/mnsentinellakes/src/R/compile_do_folder.R MN DNR Sentinel Lakes Program
#currently need to change serial name in file path (bottom when you run function) 
#final file name will save as name of the folder the data comes from. For me, that's the serial number of each DO logger

compile_do_folder=function(folderpath,savetofolder){
  
  if (!is.null(savetofolder)){
    
    read_do_files=function(filename){
      dofile=utils::read.delim(filename,stringsAsFactors = FALSE)
      dodata=as.data.frame(do.call(rbind,strsplit(dofile[,1],split = ",")))
      names(dodata)=as.character(unlist(dodata[2,]))
      dodata=dodata[-c(1,2),]
      return(dodata)
    }
    
    dofiles=list.files(folderpath,full.names = TRUE,pattern = ".txt")
    
    docombined=NULL
    for (i in dofiles){
      readdo=read_do_files(i)
      
      datetime=as.numeric(as.character(readdo$`Time (sec)`))
      datetimeutc=as.POSIXct(datetime,origin = "1970-01-01",tz="UTC")
      batteryformat=as.numeric(as.character(readdo$`  BV (Volts)`))
      tempformat=as.numeric(as.character(readdo$`  T (deg C)`))
      doformat=as.numeric(as.character(readdo$`  DO (mg/l)`))
      qformat=as.numeric(as.character(readdo$`  Q ()`))
      
      
      
      doreformat=data.frame("Unix.Timestamp"=as.character(datetime),"UTC_Date_._Time"=as.character(datetimeutc),
                            "Central.Standard.Time"=as.character(lubridate::with_tz(datetimeutc,'US/Central')),"Battery"=as.character(batteryformat),
                            "Temperature"=as.character(tempformat),"Dissolved.Oxygen"=as.character(doformat),"Dissolved.Oxygen.Saturation"=NA,
                            "Q"=as.character(qformat),stringsAsFactors = FALSE)
      
      docombined=rbind(docombined,doreformat)
    }
    
    dorowadd=data.frame("Unix.Timestamp"=as.character("(Second)"),"UTC_Date_._Time"=as.character("(none)"),"Central.Standard.Time"=as.character("(none)"),
                        "Battery"=as.character("(Volt)"),"Temperature"=as.character("(deg C)"),"Dissolved.Oxygen"=as.character("(mg/l)"),
                        "Dissolved.Oxygen.Saturation"=as.character("(%)"),"Q"=as.character("(none)"),stringsAsFactors = FALSE)
    dofinal=rbind(dorowadd,docombined)
    
    folder=unlist(strsplit(folderpath,split = "/"))[length(unlist(strsplit(folderpath,split = "/")))]
    
    if (substr(savetofolder,(nchar(savetofolder)),nchar(savetofolder))!="/"){
      savetofolder=paste0(savetofolder,"/")
    }
    
    pathout=paste0(savetofolder,folder,".csv")
    
    utils::write.csv(dofinal,pathout,row.names = FALSE)
  }else{
    warning("Missing saveto")
  }
}

#Actually run the function
compile_do_folder(folderpath = "~/Documents/MiniDOT/425218",savetofolder = "~/Documents/GitHub/2022-thermal-zoop-experiments/raw-tank-data")
