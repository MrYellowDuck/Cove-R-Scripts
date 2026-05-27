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
  suburb = df$district,
  post_code = df$post_code,
  date_of_birth = df$date_of_birth,
  gender = df$gender,
  firstname = df$name,
  surname = df$surname,
  plate = df$plate,
  glass = df$glass,
  roadside = df$roadside,
  rental = df$rental,
  cove_gep = df$cove_premium
)

tryCatch(
  { 
    Error <- ""
    
    Flag <- FALSE
    while(!Flag) {          
      tryCatch({
        set.scheme("Tower Motor - Single Driver", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
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
    
    click("Car", xwindow = 10, ywindow = 10)
    
    
    click("Car 2", xwindow = 10, ywindow = 10)
    human.pause(minsec=1, maxsec=1.5)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Tower No", xwindow = 10, ywindow = 10)
    
    click("Begin Quote", xwindow = 10, ywindow = 10)
    
    click("No Business Use")
    
    human.pause(minsec=1, maxsec=1.5)
    click("Rego")
    
    type_words(quote_data[["plate"]])
    
    human.pause(minsec=1, maxsec=1.5)
    click("Search")
    
    human.pause(minsec=3, maxsec=4)
    scroll.to.find("Address1", FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("Address1")
    human.pause(minsec=1, maxsec=1.5)
    
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(5)
    click("Address2", yoffset=120)
    human.pause(minsec=1, maxsec=2)
    
    click("Address2", yoffset = 80)
    human.pause(minsec=1, maxsec=2)
    
    copy_clipboard()
    cb <- readClipboard()
    quote_data[["quoted_address"]] <- cb
    click("Address2")
    
    scroll.to.find("Surname", Up=FALSE)
    human.pause(minsec=1, maxsec=2)

    click("Firstname")
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["firstname"]]))
    human.pause(minsec=1, maxsec=2)
    
    click("Surname")
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["surname"]]))
    
    human.pause(minsec=1, maxsec=2)
    scroll.to.find("ddmmyyyy", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("DOB")
    human.pause(minsec=1, maxsec=2)
    type_words(sprintf("%02d", lubridate::day(quote_data[["date_of_birth"]])))
    human.pause(minsec=1, maxsec=2)
    type_words(sprintf("%02d", lubridate::month(quote_data[["date_of_birth"]])))
    human.pause(minsec=1, maxsec=2)
    type_words(lubridate::year(quote_data[["date_of_birth"]]))
    human.pause(minsec=1, maxsec=2)
    
    scroll.to.find("Gender Buttons", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    
    if(quote_data[["gender"]]=="Male") {
      click("Male")
    } else {
      click("Female")
    }
    human.pause(minsec=1, maxsec=2)
    
    scroll.to.find("Losses", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("No Claims")
    human.pause(minsec=1, maxsec=2)
    
    scroll.to.find("Under 25", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    click("Under 25")

    click("Under 25 Yes")
    human.pause(minsec=1, maxsec=2)
    
    scroll.to.find("I Understand", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("I Understand")
    human.pause(minsec=1, maxsec=2)
    
    scroll.to.find("Customise", Up=FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("Customise")
    
    click("Summary", dontclick = TRUE)
    human.pause(minsec=1, maxsec=2)
    
    click("Summary", yoffset=-50)
    human.pause(minsec=1, maxsec=2)
    copy_clipboard()
    cb <- readClipboard()
    human.pause(minsec=1, maxsec=2)
    mouse.click()
    
    quote_data[["headline_premium_per_fortnight"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("per fortnight", cb)[1]-1]))
    
    quote_data[["glass_premium_per_fortnight"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("Excess free windscreen", cb)[1]+1]))
    quote_data[["roadside_premium_per_fortnight"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("RoadWise", cb)[1]+1]))
    quote_data[["rental_premium_per_fortnight"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("Rental vehicle", cb)[1]+1]))
    
    quote_data[["minimum_sum_insured"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("Minimum", cb)[1]+1]))    
    quote_data[["maximum_sum_insured"]] <- as.numeric(gsub("\\$|\\,|\\*", "", cb[grep("Maximum", cb)[1]+1]))    
    
    
    scroll.to.find("Additional", Up = FALSE)
    human.pause(minsec=1, maxsec=2)
    
    click("Quoted Sum Insured")
    human.pause(minsec=1, maxsec=2)
    copy_clipboard()
    cb <- readClipboard()
    
    quote_data[["quoted_sum_insured"]] <- as.numeric(gsub("\\,", "", cb))

    premium <- quote_data[["headline_premium_per_fortnight"]]
    
    if(quote_data[["glass"]]=="Yes") premium <- premium + quote_data[["glass_premium_per_fortnight"]]
    if(quote_data[["roadside"]]=="Yes") premium <- premium + quote_data[["roadside_premium_per_fortnight"]]
    if(quote_data[["rental"]]=="Yes") premium <- premium + quote_data[["rental_premium_per_fortnight"]]
    
    quote_data[["tower_premium"]] <- premium
    
    quote_data[["prodcut_quoted"]] <- "Car"

    click("Close Firefox")
    clear.region()  
    
  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })
