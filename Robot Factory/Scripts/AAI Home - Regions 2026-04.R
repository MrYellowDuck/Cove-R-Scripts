source("C:/Users/robot/Dropbox/Fingerprinter2/R Code/Fingerprinter Commands v4.2.R")

type_words <- function(x, scale=1) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  
  for(i in 1:nchar(x)) {
    y = substr(x,i,i)
    keybd.type_string(y)
    if(i < 6) {
      Sys.sleep(runif(1,0.1*scale,0.3*scale))
    } else {
      Sys.sleep(runif(1,0.1,0.3))
    }
  }
}

find_in_list <- function(anchor, zone, text) {
  if(anchor != "") click.element(anchor)
  
  tries <- 10
  while(tries > 0) {
    if(click.element(zone, OCRClickWord = text)[1]=="OKAY") {
      break
    }
    
    keybd.press("pagedown")
    
    tries <- tries - 1
  }
  if(tries==0) Error <<- paste0("ERROR: Could not find '", text, "'")
}

copy_clipboard <- function() {
  Sys.sleep(2)
  keybd.press("ctrl", hold = TRUE)
  keybd.press("a")
  keybd.release("ctrl")
  Sys.sleep(0.5)
  keybd.press("ctrl", hold = TRUE)
  keybd.press("c")
  keybd.release("ctrl")
  Sys.sleep(1)
}

click <- function(element, error_message="", dontclick = FALSE, xoffset = 0, yoffset = 0, OCRClickWord="", timeout=30, xwindow=0, ywindow=0) {
  if(click.element(element, dontclick = dontclick, xoffset = xoffset, yoffset = yoffset, OCRClickWord = OCRClickWord, timeout = timeout, xwindow = xwindow, ywindow = ywindow)[1] != "OKAY") {
    Error <<- paste0("ERROR: Couldn't click on '", error_message,"'.")
    stop(Error)
  }
}

quote_data <- list(
  scheme = df$scheme,
  street = paste(df$street_number, df$street_name),
  suburb = df$suburb,
  post_code = df$post_code,
  date_of_birth1 = df$date_of_birth,
  gender1 = "Male",
  name1 =strsplit(df$insured_name, " ")[[1]][1],
  surname1 = strsplit(df$insured_name, " ")[[1]][2],
  sum_insured = df$sum_insured,
  year_built = df$year_built,
  sqm = df$sq_metres_dwelling
)

tryCatch(
  { 
    Error <- ""
    
    Flag <- FALSE
    while(!Flag) {          
      tryCatch({
        set.scheme("AAI House 2026-04", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
        clear.region()
        Sys.sleep(1)
        click("Sydney")
        Sys.sleep(5)
        Flag <- TRUE
      }, error = function(e) {print("Can't change ip")})
    }        
    
    clear.region()
    clear.region()
    clear.region()
    clear.region()
    click("Open Firefox")
    Sys.sleep(5)
    click("New Tab", yoffset=150)
    
    set.region("Top Left", "Bottom Right")
    
    click("Enter URL")
    Sys.sleep(1)
    
    keybd.type_string("www.aainsurance.co.nz")
    keybd.press("enter")
    Sys.sleep(5)
    human.pause()
    
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("House", xwindow = 10, ywindow = 10)
    
    click("get a quote")
    
    click("Next")
    
    click("Owner Occupied")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("I live here")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Freestanding")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Not body corporate")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Home only")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("DOB")
    human.pause(minsec=1, maxsec=1.5)
    
    type_words(substr(quote_data[["date_of_birth1"]], 4, 5))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["date_of_birth1"]], 1, 2))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["date_of_birth1"]], 7, 10))
    human.pause(minsec=2, maxsec = 3)
    
    click("Next")
    
    click("Not AA")
    human.pause(minsec=1, maxsec = 1.6)
    
    click("Next")
    
    click("Address", xwindow=10, ywindow=10)
    Sys.sleep(1)
    human.pause()
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(3)
    click("Address2", xwindow=100, ywindow=10, yoffset=75)
    Sys.sleep(1)
    
    human.pause(minsec=1, maxsec=2)
    click("Address2", xwindow=100, ywindow=10, yoffset=20)
    Sys.sleep(1)    
    copy_clipboard()
    cb <- readClipboard()
    quote_data[["quoted_address"]] <- cb
    
    click("Next")
    
    if(click.element("Unit", timeout = 5)[1] == "OKAY") {
      keybd.press("pagedown")
      Sys.sleep(3)
      click("Next")
    }
    
    click("Walls")
    human.pause(minsec=1, maxsec=2)
    click.element("Wall Type", dontclick = TRUE, Hover = TRUE, yoffset=200)
    scroll.to.find("Wood", FALSE)
    human.pause(minsec=1, maxsec=1.5)
    click("Wood")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    
    click("Roof")
    human.pause(minsec=1, maxsec=2)
    click.element("Roof Type", dontclick = TRUE, Hover = TRUE, yoffset=200)
    scroll.to.find("Metal", FALSE)
    human.pause(minsec=1, maxsec=1.5)
    click("Metal")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Know Year")
    
    click("Year")
    human.pause(minsec=1, maxsec=1.5)
    type_words(quote_data[["year_built"]])        
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("One Level")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("No Carports")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Outbuildings", dontclick = TRUE)
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("SQM")
    human.pause(minsec=1, maxsec=1.5)
    type_words(quote_data[["sqm"]])        
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    
    click("Sum Insured")
    human.pause(minsec=1, maxsec=1.5)
    keybd.press("ctrl", hold = TRUE)
    keybd.press("a")
    keybd.release("ctrl")
    Sys.sleep(0.5)
    type_words(quote_data[["sum_insured"]])        
    Sys.sleep(0.5)
    
    click("Amount")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    Searching <- TRUE
    Finished <- FALSE
    Tries <- 0
    while(Searching & (Tries < 10)) {
      if(click.element("No Mortgage", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
        Searching <- FALSE
      } else if(click.element("Sum Insured High", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
        Searching <- FALSE
        Finished <- TRUE
        quote_data[["Error"]] <- "Sum insured too high"
      }
      Tries <- Tries + 1
    }
    
    if(!Finished) {
      click("No Mortgage")
      human.pause(minsec=1, maxsec=1.5)
      
      click("Next")
      
      click("No Hazards")
      human.pause(minsec=1, maxsec=1.5)
      
      click("Next")
      
      click("No Claims")
      human.pause(minsec=1, maxsec=1.5)
      
      click("Next")
      
      click("Annual")
      
      Sys.sleep(2)
      
      click("Your Quote")
      Sys.sleep(1)
      
      copy_clipboard()
      cb <- readClipboard()
      
      quote_data[["headline_premium_annual"]] <- as.numeric(gsub("\\$|\\,", "", cb[grep("Fortnightly", cb)-1]))
    }
    
    quote_data[["prodcut_quoted"]] <- "House"
  
    click("Close Firefox")
    clear.region()  
    

  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })
