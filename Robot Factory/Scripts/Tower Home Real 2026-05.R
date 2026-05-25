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
  street = df$street,
  suburb = df$suburb,
  post_code = df$post_code,
  date_of_birth1 = df$date_of_birth1,
  gender1 = "Male",
  name1 = df$name1,
  surname1 = df$surname1
)

tryCatch(
  { 
    Error <- ""
    
    Flag <- FALSE
    while(!Flag) {          
      tryCatch({
        set.scheme("Tower House 2026-05", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
        clear.region()
        Sys.sleep(1)
        click("Sydney")
        Sys.sleep(5)
        Flag <- TRUE
      }, error = function(e) {print("Can't change ip")})
    }        
    
    click("Open Firefox")
    Sys.sleep(5)
    click("New Tab", yoffset=150)
    
    set.region("Top Left", "Bottom Right")
    
    click("Enter URL")
    Sys.sleep(1)
    
    keybd.type_string("www.tower.co.nz")
    keybd.press("enter")
    Sys.sleep(5)
    human.pause()
    
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("House", xwindow = 10, ywindow = 10)
    
    
    click("House 2", xwindow = 10, ywindow = 10)
    human.pause(minsec=1, maxsec=1.5)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Tower No", xwindow = 10, ywindow = 10)
    
    click("Begin Quote", xwindow = 10, ywindow = 10)
    
    
    click("Address", dontclick = TRUE)
    human.pause(minsec=1, maxsec=1.5)
    click("Address")
    human.pause(minsec=1, maxsec=1.5)
    
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(5)
    click("Address2", yoffset = 50)
    human.pause(minsec=1, maxsec=2)
    
    click("Address2", yoffset = 10)
    human.pause(minsec=1, maxsec=2)
    
    copy_clipboard()
    cb <- readClipboard()
    quote_data[["quoted_address"]] <- cb
    
    click("House Details")

    Finished <- FALSE
    if(click.element("LifeStyle", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
      quote_data[["Error"]] <- "Lifestyle block"
      Finished <- TRUE
    }

    if(click.element("Unable", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
      quote_data[["Error"]] <- "Unable to offer cover"
      Finished <- TRUE
    }
    
    if(click.element("More Info", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
      quote_data[["Error"]] <- "More information required"
      Finished <- TRUE
    }
    
    if(click.element("Body Corporate", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
      quote_data[["Error"]] <- "Body corporate"
      Finished <- TRUE
    }
    

    if(!Finished) {
      click.element("Freestanding House", timeout=1)
      click.element("Freestanding2", timeout=1)
      human.pause(minsec=1, maxsec=2)
      
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec=2)
      
      
      click("Year Built", yoffset = 25)
      human.pause(minsec=1, maxsec=2)
      copy_clipboard()
      cb <- readClipboard()
      quote_data[["year_built"]] <- cb
      

      scroll.to.find("Construction Quality", FALSE)
      
      # human.pause(minsec=1, maxsec = 2)
      # click("Levels")
      # 
      # human.pause(minsec=0.5, maxsec = 0.7)
      # click("One Level")
      
      human.pause(minsec=1, maxsec = 2)
      click("Area House", yoffset = 25)
      human.pause(minsec=1, maxsec=2)
      copy_clipboard()
      cb <- readClipboard()
      quote_data[["sqm"]] <- cb
      
      
      
      # keybd.press("ctrl", hold = TRUE)
      # keybd.press("a")
      # keybd.release("ctrl")
      # human.pause(minsec=0.5, maxsec = 0.7)
      # type_words(quote_data[["sqm"]])
      # human.pause(minsec=0.5, maxsec = 0.7)
      # 
      # click("Area Out", yoffset = 25)
      # human.pause(minsec=1, maxsec=2)
      # keybd.press("ctrl", hold = TRUE)
      # keybd.press("a")
      # keybd.release("ctrl")
      # human.pause(minsec=0.5, maxsec = 0.7)
      # type_words("0")
      # human.pause(minsec=0.5, maxsec = 0.7)
      # 
      # click("Walls", yoffset = 25)
      # human.pause(minsec=0.5, maxsec = 0.7)
      # keybd.press("w")
      # human.pause(minsec=0.5, maxsec = 0.7)
      # keybd.press("enter")
      # 
      # click("Roof", yoffset = 25)
      # human.pause(minsec=0.5, maxsec = 0.7)
      # if(click.element("Metal", timeout=1)[1]!="OKAY") {
      #   keybd.press("pagedown")
      #   human.pause(minsec=1, maxsec = 1.5)
      #   click("metal")        
      # }
      # 
      # human.pause(minsec=1, maxsec = 2)
      # 
      # click("Slope", yoffset = 25)
      # human.pause(minsec=0.5, maxsec = 0.7)
      # click("Flat")
      # human.pause(minsec=1, maxsec = 2)
      
      click("Construction", yoffset = -50)
      human.pause(minsec=0.5, maxsec = 0.7)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec = 1.5)
      
      # click("Construction", yoffset = 25)
      # human.pause(minsec=0.5, maxsec = 0.7)
      # click("High")
      # human.pause(minsec=1, maxsec = 2)
      
      click("Confirm")
      
      human.pause(minsec=1, maxsec = 2)

#      Finished <- FALSE
#      Tries <- 0
#      while(!Finished & Tries < 3) {
        click("Sum Insured Label")
        human.pause(minsec=1, maxsec = 2)
        keybd.press("pagedown")
        human.pause(minsec=1, maxsec = 1.5)
        
        click.element("Sum Insured", yoffset=25, timeout=1)
        click.element("Sum Insured2", yoffset=25, timeout=1)
        human.pause(minsec=1, maxsec = 1.5)
        copy_clipboard()
        cb <- readClipboard()
        quote_data[["sum_insured"]] <- cb

        click("Confirm")
        
      #   Finished <- TRUE
      #   if(click.element("Low Sum Insured", timeout=3)[1]=="OKAY") {
      #     scroll.to.find("Construction",TRUE)
      #     click("Construction", yoffset = 25)
      #     human.pause(minsec=0.5, maxsec = 0.7)
      #     click("Standard")
      #     click("Confirm")
      #     Finished <- FALSE
      #   }
      #   Tries <- Tries + 1
      # }
      
      click.element("Rewired", timeout = 2)

      scroll.to.find("Hazard No", FALSE)
      human.pause(minsec=1, maxsec = 1.5)
      click("Hazard No")
      
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec = 1.5)
      
      click("Extra Units")
      human.pause(minsec=1, maxsec = 1.5)
      
      click("Owner Occupied")
      human.pause(minsec=1, maxsec = 1.5)
      
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec = 1.5)
      
      click("Business No")
      human.pause(minsec=1, maxsec = 1.5)
      
      click("DOB")
      human.pause(minsec=1, maxsec = 1.5)
      
      type_words(substr(quote_data[["date_of_birth1"]], 9, 10))
      human.pause(minsec=1, maxsec = 1.6)
      type_words(substr(quote_data[["date_of_birth1"]], 6, 7))
      human.pause(minsec=1, maxsec = 0.6)
      type_words(substr(quote_data[["date_of_birth1"]], 1, 4))
      human.pause(minsec=2, maxsec = 3)
      
      click("DOB Label")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec = 1.5)
      
      click("Claims No")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      
      click("Customise")    
      
      click("Policy Benefits")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      
      click("Summary")
      
      Flag <- FALSE
      count <- 0
      while(!Flag & count < 10) {
        if(click.element("Details", timeout=1)[1]=="OKAY") Flag <- TRUE
        if(click.element("Details2", timeout=1)[1]=="OKAY") Flag <- TRUE
        count <- count + 1
      }

      human.pause(minsec=0.5, maxsec = 0.9)
      keybd.press("pagedown")
      human.pause(minsec=0.5, maxsec = 0.9)
      
      click("Yearly", xoffset = -50)
      human.pause(minsec=1, maxsec = 2)
      
      copy_clipboard()
      cb <- readClipboard()
      
      quote_data[["headline_premium_annual"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("Total Premium", cb) + 1]))
    }
    
    quote_data[["prodcut_quoted"]] <- "House"

    click("Close Firefox")
    clear.region()  
    
  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })
