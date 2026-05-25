
void <- suppressWarnings(tryCatch(source("Z:/Dropbox/Fingerprinter2/R Code/Fingerprinter Commands v4.1.R"), error=function(e) NULL))
void <- suppressWarnings(tryCatch(source("C:/Users/robot/Dropbox/Fingerprinter2/R Code/Fingerprinter Commands v4.2.R"), error=function(e) NULL))

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
  gender1 = df$gender1,
  name1 = df$name1,
  surname1 = df$surname1,
  plateno = df$plateno
)

print(quote_data[["scheme"]])

tryCatch(
  { 
    Error <- ""
    
    Flag <- FALSE
    while(!Flag) {          
      tryCatch({
        ImagePath <- "C:\\Users\\robot\\Dropbox\\Fingerprinter2\\Image Databases\\images3.db"
        if(!file.exists(ImagePath)) {
          ImagePath <- "Z:\\Dropbox\\Fingerprinter2\\Image Databases\\images3.db"
        }
        
        set.scheme(quote_data[["scheme"]], ImagePath)
        clear.region()
        Sys.sleep(1)
        click("Sydney")
        Sys.sleep(5)
        Flag <- TRUE
      }, error = function(e) {print("Can't change ip. Retrying.")})
    }        
    
    click("Open Firefox")
    Sys.sleep(5)

    click("New Tab", yoffset=150)
    
    set.region("Top Left", "Bottom Right")
    
    click("Enter URL")
    Sys.sleep(1)
    
    keybd.type_string("https://go.aainsurance.co.nz/motor/quote.html#policystartdate")
    keybd.press("enter")
    Sys.sleep(5)
    human.pause()
    
    click("Next", xwindow = 50, ywindow = 10)
    human.pause()
    
    click("Cover Options")
    human.pause(minsec=1, maxsec = 2)
    
    click("Comprehensive")
    human.pause(minsec=1, maxsec = 2)
    
    click("Next", xwindow = 50, ywindow = 10)
        
    click("Plate No", dontclick = TRUE)
    human.pause()
    
    click("Plate No", xwindow = 50, ywindow = 10)
    human.pause()
    type_words(quote_data[["plateno"]])
    
    click("Next", xwindow = 50, ywindow = 10)
    
    click("Found", timeout=20, dontclick = TRUE)
    
    void <- click.element("Select Car", timeout = 1)
    Sys.sleep(1)
    
    keybd.press("pagedown")
    Sys.sleep(1)
    
    click("Next", xwindow = 50, ywindow = 10)
    
    click("After Market")
    Sys.sleep(3)
    
    click("Next")
    
    click("Finance")
    Sys.sleep(3)
    
    click("Next")
    
    click("Private")
    human.pause(minsec=2, maxsec = 3)
    
    click("Next")
    
    click("Address", dontclick = TRUE)
    human.pause(minsec=2, maxsec = 3)
    
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

    click("DOB", dontclick = TRUE)
    human.pause(minsec=1, maxsec=2)
    
    click("DOB")
    human.pause(minsec=0.5, maxsec = 1.2)
    
    type_words(substr(quote_data[["date_of_birth1"]], 6, 7))
    human.pause(minsec=1, maxsec = 1.6)
    type_words(substr(quote_data[["date_of_birth1"]], 9, 11))
    human.pause(minsec=0.3, maxsec = 0.6)
    type_words(substr(quote_data[["date_of_birth1"]], 1, 4))
    human.pause(minsec=0.5, maxsec = 1.2)
    
    click("Gender")
    human.pause(minsec=2, maxsec = 2.5)
    
    click("Male")
    human.pause(minsec=0.5, maxsec = 1.2)
    
    click("Next")
    
    click("No", dontclick = TRUE)
    human.pause(minsec=0.5, maxsec = 1.2)
    click("No")
    
    human.pause(minsec=0.5, maxsec = 1.2)
    
    click("Next")
    
    click("Additional Drivers", dontclick = TRUE)
    human.pause(minsec=2, maxsec = 3)
    
    click("Next")
    
    click("AA No", dontclick = TRUE)
    human.pause(minsec=0.5, maxsec = 1.2)
    click("AA No")
    human.pause(minsec=2, maxsec = 3)
    
    click("Next")
    
    click("Your Quote", timeout = 60, dontclick = TRUE)
    human.pause(minsec=2, maxsec = 3)
    click("Your Quote")
    
    copy_clipboard()
    cb <- readClipboard()
    
    quote_data[["headline_premium_annual"]] <- gsub("\\$|\\,|\\*", "", cb[grep("for the year", cb)])
    quote_data[["headline_premium_annual"]] <- gsub(" for the year", "", quote_data[["headline_premium_annual"]])
    
    quote_data[["prodcut_quoted"]] <- "Car"
    quote_data[["level_quoted"]] <- "Comprehensive"
    
    click("Close Firefox")
    clear.region()  
  }, error = function(e) {
    click.element("Close Firefox", timeout = 5)
    clear.region()  
    
    quote_data[["script_error"]] <- e
    print(e)

    Error <- e
  })
    