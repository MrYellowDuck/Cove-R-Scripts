# This script supports parallel execution of the rating workbook by creating
# multiple worker copies, each identified by a NODE stamp in the filename.
#
# Sections are deliberately wrapped in while(FALSE) so they can be run manually
# one at a time.
#
# Create workers:
#   Copies the base workbook once per node and writes run_nodes.bat. The batch
#   file opens each worker workbook in a separate Excel instance using /x, with
#   a short delay between launches. Workbook_Open() in ThisWorkbook then decides
#   whether the opened workbook is a worker node and runs run_all().
#
# Gather results:
#   Opens each completed worker workbook, reads the named output ranges for
#   new business, renewals, and day 0, converts them to numeric matrices, and
#   adds the matrices together across all nodes. The combined outputs are then
#   written to projection_results.xlsx, one output matrix per sheet.
#
# Delete workers:
#   Removes the generated worker workbooks after results have been gathered.

setwd("G:/Actuarial/Motor/Pricing model/WIP Updates")

spreadsheet <- "Motor_Rating_May_2026_v01 (renewal &NB testing).xlsm"

excel_path <- "C:/Program Files/Microsoft Office/root/Office16/excel.exe"

nodes <- 8 #This should generally be set close to the number of P-cores on the host CPU. Increasing node count further to utilise E-cores does not materially improve runtime for this workload.



###############
#Create workers
###############
while(FALSE) {
  for(i in 1:nodes) {
    newname <- gsub("\\.xlsm", paste0(" NODE", sprintf("%02d", nodes), sprintf("%02d", i), ".xlsm"), spreadsheet)
    file.copy(spreadsheet, newname, overwrite = TRUE)
  }
  
  batfile <- file.path(getwd(), "run_nodes.bat")
  
  cmds <- c("@echo off", sapply(1:nodes, function(i) c(
    paste0('start "" "', excel_path, '" /x "', getwd(), '\\', gsub("\\.xlsm", paste0(" NODE", sprintf("%02d%02d", nodes, i), ".xlsm"), spreadsheet), '"'),
    "timeout /t 3 /nobreak > nul"
  )))
  
  writeLines(cmds, batfile)
}


###############
#Gather results
###############
while(FALSE) {
  get_range <- function(wb, range_name) {
    nr <- wb$get_named_regions()
    
    range <- nr$value[which(nr$name == range_name)]
    
    sheet <- gsub("'", "", sub("!.*$", "", range))
    dims <- sub("^.*!", "", range)
    
    df <- openxlsx2::wb_to_df(wb, sheet= sheet, dims = dims, col_names = FALSE)
    data.matrix(df)
  }
  
  
  for (node in 1:nodes) {
    file <- gsub("\\.xlsm$", paste0(" NODE", sprintf("%02d", nodes), sprintf("%02d", node), ".xlsm"), spreadsheet)
    print(file)
    
    wb <- openxlsx2::wb_load(file)
    
    if(node==1) {
      NB <- get_range(wb, "Cases_NB_Outputs")
      RN <- get_range(wb, "Cases_Renewal_Outputs")
      D0 <- get_range(wb, "Cases_Day_0_outputs")
    } else {
      NB <- NB + get_range(wb, "Cases_NB_Outputs")
      RN <- RN + get_range(wb, "Cases_Renewal_Outputs")
      D0 <- D0 + get_range(wb, "Cases_Day_0_outputs")
    }
  }
  
  out_file <- "projection_results.xlsx"
  
  wb_out <- openxlsx2::wb_workbook()
  
  wb_out$add_worksheet("NB")
  wb_out$add_data("NB", NB, col_names = FALSE)
  
  wb_out$add_worksheet("RN")
  wb_out$add_data("RN", RN, col_names = FALSE)
  
  wb_out$add_worksheet("D0")
  wb_out$add_data("D0", D0, col_names = FALSE)
  
  wb_out$save(out_file, overwrite = TRUE)
}


###############
#Delete workers
###############
while(FALSE) {
  for(i in 1:nodes) {
    file <- gsub("\\.xlsm", paste0(" NODE", sprintf("%02d", nodes), sprintf("%02d", i), ".xlsm"), spreadsheet)
    file.remove(file)
  }
}



