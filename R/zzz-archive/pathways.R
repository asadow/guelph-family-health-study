## Pathways

OS <- .Platform$OS.type
if (OS == "unix"){
  sl <- "/"
  opath <- "/Users/Adam/OneDrive - University of Guelph/"
  ogpath <- "/Users/Adam/OneDrive - University of Guelph (1)/GFHS Team OneDrive/"
  #fpath <- "/Volumes/cbsshared/HHNS/David_Ma/study1/"
  fpath <- "/Users/Adam/study1/"

} else if (OS == "windows"){
  wuser <- Sys.getenv("USERNAME")
  sl <- "\\"
  opath <- paste0("C:\\Users\\", wuser, "\\OneDrive - University of Guelph\\")
  ogpath <- paste0("C:\\Users\\", wuser, "\\OneDrive - University of Guelph (1)\\", "GFHS Team OneDrive")
  fpath <- "V:\\HHNS\\David_Ma\\study1\\"

} else {
  print("ERROR: OS could not be identified")
}

mpath <- paste0(fpath, "GFHS Master List.xlsx")
tpath <- paste0(fpath, "GFHS Study Visit Tracker.xlsx")
log_p <- paste0(fpath, "ASA24 Tracking", sl, "Login Info", sl)
dat_p <- paste0(fpath, "Data", sl)

cle_p <- paste0(dat_p, "Clean Data", sl)
raw_p <- paste0(dat_p, "Raw Data", sl)
ref_p <- paste0(dat_p, "Reference Files", sl)
ss_p <- paste0(raw_p, "SS", sl)

raw_p_asa <- paste0(raw_p, "ASA", sl)
raw_p_esha <- paste0(raw_p, "ESHA", sl)
sv_p <- paste0(raw_p, "SV", sl, "gfhs-123_survey_deidentified_qualtrics", sl)
ha_p <- paste0(raw_p, "HA", sl)
ahha_p <- paste0(raw_p, "AHHA", sl)
ll_p <- paste0(raw_p, "LL", sl)

sub_p <- paste0(cle_p, "Subsets for Requests", sl)

out_p <- paste0(ogpath, sl, "Data Cleaning Reference Files", sl, "ASA", sl)
ana_p <- paste0(opath, "Analysis", sl)

if (dir.exists(fpath)) {

} else {

  stop("V drive path does not exist.")
}


# options(java.parameters = "- Xmx1024m")
