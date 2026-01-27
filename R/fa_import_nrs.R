
#' Import NRS data
#'
#' Importing Non-resident Student data. This data contains information on student transfers into and out of each district. 
#'
#'
#' @export
fa_import_nrs <- function(){
  
  
  nrs_raw <- 
    tt_import_nrs_trans_in_out() %>% 
    mutate(operating.dcode = as.numeric(operating.dcode),
           resident.dcode = as.numeric(resident.dcode))
  
  
  
  stu_transfer_in <- 
    nrs_raw %>% 
    group_by(FY, operating.dcode) %>% 
    summarise(nrs.stu.transfer.in = sum(student.fte.count, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(dnum = operating.dcode)
  
  
  stu_transfer_out <- 
    nrs_raw %>% 
    group_by(FY, resident.dcode) %>% 
    summarise(nrs.stu.transfer.out = sum(student.fte.count, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(dnum = resident.dcode)
  
  
  nrs_clean <- 
    stu_transfer_in %>% 
    left_join(stu_transfer_out, by = join_by(FY == FY, dnum == dnum)) %>% 
    mutate(nrs.stu.transfer.in = ifelse(is.na(nrs.stu.transfer.in), 0, nrs.stu.transfer.in),
           nrs.stu.transfer.out = ifelse(is.na(nrs.stu.transfer.out), 0, nrs.stu.transfer.out) * - 1,
           nrs.stu.transfer.net = nrs.stu.transfer.in + nrs.stu.transfer.out)
  
  return(nrs_clean)
  
  
}






