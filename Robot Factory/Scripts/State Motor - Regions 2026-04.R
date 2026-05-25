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
  scheme = "State Motor 2026-04",
  street = df$street,
  suburb = df$district,
  city = NA,
  post_code = df$post_code,
  date_of_birth1 = df$date_of_birth1,
  gender1 = "Male",
  name1 =df$name1,
  surname1 = df$surname1,
  date_of_birth2 = NA,
  gender2 = NA,
  name2 = NA,
  plateno = df$plateno,
  sum_insured = NA,
  glass = "No",
  rental = "No",
  roadside = "No",
  excess = 500
)

tryCatch(
  { 
    Error <- ""
    
    Flag <- FALSE
    while(!Flag) {          
      tryCatch({
        set.scheme("State Motor 2026-04", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
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
    
    click("Grab A Quote", xwindow = 50, ywindow = 10)
    human.pause()
    
    click("Plate No", dontclick = TRUE)
    human.pause()
    
    click("Plate No", xwindow = 50, ywindow = 10)
    human.pause()
    type_words(quote_data[["plateno"]])
    
    human.pause(maxsec = 3)
    click("Search", xwindow = 50, ywindow = 10)
    human.pause()
    
    click("Next", xoffset=100)
    Sys.sleep(1)
    copy_clipboard()
    cb <- readClipboard()
    mouse.click()
    
    quote_data[["vehicle_quoted"]] <- cb[grep("We found your car", cb)+2]
    
    click("Next")
    
    click("Address", xwindow=100, ywindow=10)
    Sys.sleep(1)
    human.pause()
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(3)
    click("Address2", xwindow=100, ywindow=10, yoffset=75)
    Sys.sleep(1)
    
    human.pause(minsec=1, maxsec=2)
    
    Sys.sleep(5)    
    copy_clipboard()
    cb <- readClipboard()
    quote_data[["quoted_address"]] <- cb
    
    click("Next")
    
    
    click("Garage", dontclick = TRUE)
    human.pause()
    
    click("Garage")
    
    Sys.sleep(1)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Warrant")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Modifications")
    
    Sys.sleep(1)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Activities")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    human.pause()
    
    click("Drive Car")
    
    human.pause()
    click("Date Of Birth", xwindow = 5, ywindow = 5)
    human.pause(minsec=0.5, maxsec = 1.2)
    
    type_words(substr(quote_data[["date_of_birth1"]], 9, 11))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["date_of_birth1"]], 6, 7))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["date_of_birth1"]], 1, 4))
    human.pause()
    
    human.pause()
    click("Date Of Birth2", yoffset=-40)
    keybd.press("pagedown")
    
    human.pause()
    click("Male")
    
    click("License")
    human.pause()
    keybd.press("pagedown")
    human.pause()
    
    click("License Age")
    human.pause()
    type_words("18")
    human.pause()
    
    click("Offences")
    human.pause()
    
    click("Claims")
    human.pause()
    
    click("Next")
    
    human.pause()
    click("Next")
    
    human.pause()
    click("Finance")
    
    human.pause()
    click("Get Quote")
    
    human.pause()
    click("State")
    human.pause()
    
    copy_clipboard()
    cb <- readClipboard()
    
    quote_data[["headline_premium_fortnight"]] <- gsub("\\$|\\,|\\*", "", cb[grep("All sorted", cb)+1])
    
    quote_data[["prodcut_quoted"]] <- "Car"
    quote_data[["level_quoted"]] <- "Comprehensive"
    
    click("Close Firefox")
    clear.region()  
    
  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })





