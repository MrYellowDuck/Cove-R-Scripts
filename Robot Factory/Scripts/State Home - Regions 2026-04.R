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
        set.scheme("State House 2026-04", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
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
    
    keybd.type_string("www.state.co.nz")
    keybd.press("enter")
    Sys.sleep(5)
    human.pause()

    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("House", xwindow = 10, ywindow = 10)
    
    click("Select and Continue", dontclick = TRUE)    
    human.pause(minsec=1, maxsec=1.5)
    click("Select and Continue")    
    
    click("Freestanding", dontclick = TRUE)    
    human.pause(minsec=1, maxsec=1.5)
    click("Freestanding")    
    
    human.pause(minsec=1, maxsec=1.5)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Own Home")
    
    human.pause(minsec=0.5, maxsec = 1.2)
    
    click("DOB")
    human.pause(minsec=0.5, maxsec = 1.2)
    
    type_words(substr(quote_data[["date_of_birth1"]], 1, 2))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["date_of_birth1"]], 4, 5))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["date_of_birth1"]], 7, 10))
    human.pause(minsec=2, maxsec = 3)
    
    click("Address Label")
    human.pause(minsec=0.3, maxsec = 0.6)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    
    
    click("Address", xwindow=100, ywindow=10)
    Sys.sleep(1)
    human.pause()
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(3)
    click("Address Label", xwindow=10, ywindow=10, yoffset=170)
    Sys.sleep(1)
    
    human.pause(minsec=1, maxsec=2)
    
    Sys.sleep(5)    
    copy_clipboard()
    cb <- readClipboard()
    quote_data[["quoted_address"]] <- cb
    
    click("Start Cover Label")
    human.pause(minsec=0.3, maxsec = 0.6)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    
    click("House Symbol", dontclick = TRUE)
    human.pause(minsec=0.3, maxsec = 0.6)
    
    if(click.element("No Year", timeout = 1, dontclick = TRUE)[1] == "OKAY") {
      click("Year Built", yoffset=25, xoffset=-50)
      human.pause(minsec=1, maxsec = 1.5)
      type_words(quote_data[["year_built"]])

      click("Floor Area", yoffset=25, xoffset=-150)
      human.pause(minsec=1, maxsec = 1.5)
      type_words(quote_data[["sqm"]])
      
      click("Garage", yoffset=25, xoffset=-150)      
      human.pause(minsec=1, maxsec = 1.5)
      type_words("0")
      
      click("One")
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec=1.5)

      click("Flat")      
      
      click("Roof Choices")
      human.pause(minsec=1, maxsec = 2)
      click.element("Metal", timeout = 1)
      
      click("Wall Choices")
      human.pause(minsec=1, maxsec = 2)  
      click.element("Wood", timeout = 1)
      
      click("Quality Label")
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec=1.5)
      
      click("Quality")

    } else {
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      
      
      click("Year Built")
      human.pause(minsec=1, maxsec = 1.5)
      type_words(quote_data[["year_built"]])
      
      click("Floor Area")
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("ctrl", hold = TRUE)
      keybd.press("a")
      keybd.release("ctrl")
      human.pause(minsec=0.5, maxsec = 0.7)
      type_words(quote_data[["sqm"]])
      
      click("Garage")
      human.pause(minsec=1, maxsec = 1.5)
      keybd.press("ctrl", hold = TRUE)
      keybd.press("a")
      keybd.release("ctrl")
      human.pause(minsec=0.5, maxsec = 0.7)
      type_words("0")
      
      click("Garage", yoffset=-50)
      human.pause(minsec=0.3, maxsec = 0.6)
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec=1.5)
      
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      click("Pencil")
      human.pause(minsec=0.3, maxsec = 0.6)
      click.element("Pencil", timeout=1)
      human.pause(minsec=0.3, maxsec = 0.6)
      click.element("Pencil", timeout=1)
      human.pause(minsec=0.3, maxsec = 0.6)
      
      click.element("One", timeout=1)
      click.element("Flat", timeout=1)
      
      click("Roof Choices")
      human.pause(minsec=1, maxsec = 2)
      click.element("Metal", timeout = 1)
      
      click("Wall Choices")
      human.pause(minsec=1, maxsec = 2)  
      click.element("Wood", timeout = 1)
      
      scroll.to.find("Quality Label",Up=FALSE)
      human.pause(minsec=1, maxsec = 2)  
      
      click("Quality Label")
      human.pause(minsec=1, maxsec = 2)  
      keybd.press("pagedown")
      human.pause(minsec=1, maxsec=1.5)
      
      click.element("Pencil", timeout=1)
      
      click.element("Quality", timeout=1)
    }
    
    
    click("One Unit")
    
    human.pause(minsec=0.5, maxsec = 0.7)  
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")    
    
    click("Sum Insured Label")
    copy_clipboard()
    cb <- readClipboard()
    mouse.click()
    
    human.pause(minsec=0.5, maxsec = 0.7)  
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)

    x <- grep("The maximum you can enter is", cb)
    if(length(x) != 0) {
      cb <- cb[x]
      cb <- as.numeric(gsub(",", "", sub(".*?\\$?([0-9,]+).*", "\\1", cb)))
    } else {
      cb <- cb[grep("Confirm an amount between", cb)]
      
      m <- gregexpr("\\$?[0-9][0-9,]*", cb)
      nums <- regmatches(cb, m)[[1]]
      cb <- as.numeric(gsub("[^0-9]", "", nums[2]))
    }
      
    if(cb < 1000000) {
        click("Back")
        click("House Symbol")
        human.pause(minsec=0.5, maxsec = 0.7)  
        keybd.press("pagedown")
        human.pause(minsec=0.5, maxsec = 0.7)  
        click("Pencil")
        human.pause(minsec=0.5, maxsec = 0.7)  
        click("Pencil")
        human.pause(minsec=0.5, maxsec = 0.7)  
        click("Pencil")
        human.pause(minsec=0.5, maxsec = 0.7)  
        click("Pencil")
        human.pause(minsec=0.5, maxsec = 0.7)  
        
        click("Prestige")
        human.pause(minsec=0.5, maxsec = 0.7)  
        keybd.press("pagedown")
        
        click("Next")
        
        click("Sum Insured Label")
        
        human.pause(minsec=0.5, maxsec = 0.7)  
        keybd.press("pagedown")
        human.pause(minsec=1, maxsec=1.5)
    }
    
    click("Sum Insured")
    human.pause(minsec=0.5, maxsec = 0.7)  
    
    keybd.press("ctrl", hold = TRUE)
    keybd.press("a")
    keybd.release("ctrl")
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(quote_data[["sum_insured"]])
    
    click("Confirm")
    
    click("Get Quote")

    Waiting <- TRUE
    Finished <- FALSE
    Attempts <- 1
    while(Waiting) {
      if(click.element("Continue on Phone", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
        Waiting <- FALSE
        quote_data[["Error"]] <- "State: Continue on phone"
        Finished <- TRUE
      }
      
      if(click.element("Your Quote", dontclick = TRUE, timeout = 1)[1]=="OKAY") {
        Waiting <- FALSE
      }

      Attempts <- Attempts + 1
      if(Attempts > 20) {
        Waiting <- FALSE
        quote_data[["Error"]] <- "Error: Infinite loop"
        Finished <- TRUE
      }
    }
    
    if(!Finished) {
      click("Your Quote")
      copy_clipboard()
      cb <- readClipboard()
      
      quote_data[["headline_premium_annual"]] <- as.numeric(gsub("\\$|\\,", "", cb[grep("Quote number:", cb) + 3]))
    }
    
    quote_data[["prodcut_quoted"]] <- "House"

    click("Close Firefox")
    clear.region()  
    
  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })
