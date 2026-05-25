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
        set.scheme("ANZ House 2026-05", "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images2.db")
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
    
    keybd.type_string("https://vanz.vero.co.nz/new-quote?type=home")
    keybd.press("enter")
    Sys.sleep(5)
    human.pause()
    
    click("Start")
    
    click("First name")
    human.pause(minsec = 0.5, maxsec = 1.5)
    type_words(quote_data[["name1"]])
    human.pause(minsec = 0.5, maxsec = 1.5)
    
    click("Surname")
    human.pause(minsec = 0.5, maxsec = 1.5)
    type_words(quote_data[["surname1"]])
    human.pause(minsec = 0.5, maxsec = 1.5)
    
    click("DOB")
    human.pause(minsec=1, maxsec = 1.5)
    
    type_words(substr(quote_data[["date_of_birth1"]], 1, 2))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["date_of_birth1"]], 4, 5))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["date_of_birth1"]], 7, 10))
    human.pause(minsec=1, maxsec = 1.5)
    
    click("DOB Label", xoffset=100)
    human.pause(minsec=1, maxsec = 1.5)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec = 1.5)
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Next")
    
    
    click("Address Label", dontclick = TRUE)
    human.pause(minsec=1, maxsec = 1.5)
    scroll.to.find("Back", FALSE)
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Address", yoffset=50)
    human.pause(minsec=1, maxsec = 1.5)
    
    type_words(quote_data[["street"]], scale=3)
    human.pause(minsec=1, maxsec=2)
    type_words(paste0(", ", quote_data[["suburb"]]))
    
    Sys.sleep(5)
    click("Address", yoffset=75)
    human.pause(minsec=2, maxsec=2.5)
    
    click("Start Date")
    human.pause(minsec=1, maxsec = 1.5)
    
    quote_data[["quote_date"]] <- format(Sys.Date() + 1, "%d/%m/%Y")
    type_words(substr(quote_data[["quote_date"]], 1, 2))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["quote_date"]], 4, 5))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["quote_date"]], 7, 10))
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Owner Yes")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Permission Yes")
    human.pause(minsec=1, maxsec = 1.5)
    
    scroll.to.find("Mortgage No", FALSE)
    
    click("Body corporate no")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Year Built")
    human.pause(minsec=1, maxsec = 1.5)
    type_words(quote_data[["year_built"]])
    human.pause(minsec=1, maxsec = 1.5)
    
    click("No Claims")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Mortgage no")
    human.pause(minsec=1, maxsec = 1.5)
    
    keybd.press("pagedown")
    human.pause(minsec=1, maxsec=1.5)

    click("One Dwelling")
    human.pause(minsec=1, maxsec=1.5)
    
    click("lifestyle no")
    human.pause(minsec=1, maxsec=1.5)
    
    scroll.to.find("Next", FALSE)
    human.pause(minsec=1, maxsec=1.5)
    
    click("Natural hazards no")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Alarm no")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("Freestanding")
    
    scroll.to.find("Owner Occupied Desc", FALSE)
    human.pause(minsec=1, maxsec=1.5)
    
    click("Owner Occupied")
    
    scroll.to.find("Next", FALSE)
    human.pause(minsec=1, maxsec=1.5)
    
    click("Business no")
    human.pause(minsec=1, maxsec=1.5)
    
    click("Next")
    
    click("SQM")
    human.pause(minsec=1, maxsec=1.5)
    type_words(quote_data[["sqm"]])
    human.pause(minsec=1, maxsec = 1.5)

    scroll.to.find("SumExtra", FALSE)
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Sum Insured")
    human.pause(minsec=1, maxsec=1.5)
    type_words(quote_data[["sum_insured"]])
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Submit")
    human.pause(minsec=1, maxsec = 1.5)
    
    scroll.to.find("Next", FALSE)
    human.pause(minsec=1, maxsec = 1.5)
    click("Next")
        
    click("Page Ready")
    
    scroll.to.find("Landlords", FALSE)
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Choose Excess")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("$750")
    human.pause(minsec=1, maxsec = 1.5)
    
    scroll.to.find("Next", FALSE)
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Nil Excess no")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Glass Excess no")
    human.pause(minsec=1, maxsec = 1.5)
    
    click("Next")

    click("Quote Summary")
    human.pause(minsec=1, maxsec = 1.5)
    copy_clipboard()
    cb <- readClipboard()
    
    quote_data[["headline_premium_annual"]] <- as.numeric(gsub("\\$|\\,","", cb[grep("/ year", cb)-1]))
    
    quote_data[["prodcut_quoted"]] <- "House"
    
    click("Close Firefox")
    clear.region()  

  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
  })
