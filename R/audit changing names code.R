Data_Guys<-Data_Guys%>%mutate (audit=case_when(
  audit=="NBOCA"~ "Bowel Cancer",
  audit=="NKCA" ~ "Kidney Cancer",
  audit=="NLCA" ~ "Lung Cancer",
  audit== "NAoMe"~"Metastatic Breast Cancer",
  audit=="NNHLA"~"Non-Hodgkin Lymphoma",
  audit=="NOGCA"~"Oesophago-Gastric Cancer",
  audit=="NOCA"~"Ovarian Cancer",
  audit=="NPaCA"~"Pancreatic Cancer",
  audit=="NAoPri"~"Primary Breast Cancer"
))
