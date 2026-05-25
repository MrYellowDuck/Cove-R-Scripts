#This is the scheduling loop run on each robot

setwd("G:/.shortcut-targets-by-id/0B7LWykRP_SIGRUlaTTAtaFJRaEE/Cove/Actuarial/Competitor Quotes/Robot Factory")

source("Robot Queue.r")

job_name <- "Tower Auckland Premiums"

while(TRUE) {
  data <- claim_job(Sys.info()[["nodename"]], job=job_name)
  
  if(nrow(data)==0) {
    #No jobs left.
    print(paste("Sleeping", lubridate::now()))
    Sys.sleep(60)
  } else {
    df <- do.call(rbind,
                  lapply(data$payload, function(z) {
                    x <- jsonlite::fromJSON(z, simplifyVector = FALSE)
                    
                    x <- lapply(x, function(v) {
                      if (length(v) == 0) {
                        NA
                      } else {
                        v
                      }
                    })
                    
                    data.frame(x, stringsAsFactors = FALSE)
                  }))
    
    tryCatch(source(paste0("./scripts/", df$script)))
    
    quote_data <- jsonlite::toJSON(quote_data)
    print(quote_data)
    print(complete_job(data$id, data$worker, quote_data))
  }
}  
  
  
  