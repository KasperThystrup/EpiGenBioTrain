# Iterate through df under the conditions of time, patient ID and center
# Append rows to new df, if more than 1 patient visit center at the same day(s)

#' @param dt Data table.
#' @param loc Column of either unit, hospital or ward.

OverlapCalc <- function(dt, loc) {
  
  dt$rownum <- 1:nrow(dt)
  
  location_dt <- dt %>%
    inner_join(dt, by = loc, suffix=c(".1",".2")) %>%
    filter(InDate.1 <= OutDate.2 &
             OutDate.1 >= InDate.2 &
             patient.1 != patient.2 &
             rownum.1 < rownum.2) %>%
    rowwise %>%
    mutate(Start = max(InDate.1, InDate.2),
           End = min(OutDate.1, OutDate.2),
           Duration_days = as.integer(End-Start+1),
           Patient.1 = patient.1,
           Patient.2 = patient.2,
           Department = Department.1) %>%
    distinct(all_of(loc), Patient.1, Patient.2, Start, End, .keep_all = T) %>%
    select(Patient.1, Patient.2, all_of(loc), Department, Start, End, Duration_days) %>%
    arrange(desc(Start), desc(Patient.1)) %>%
    mutate(Start = as.character(Start), End = as.character(End))
  
  return(location_dt)
}

