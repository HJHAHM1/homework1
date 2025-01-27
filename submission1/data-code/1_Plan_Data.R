#########################################################################
## Read in enrollment data for january of each year
#########################################################################

    ## Enrollments per plan
  library(readr)
  library(dplyr)
  library(tidyr)
  
  ## Basic contract/plan information
  ma.path=paste0("data/input/CPSC_Contract_Info_2015_01.csv")
  contract.info=read_csv(ma.path,
                         skip=1,
                         col_names = c("contractid","planid","org_type","plan_type",
                                       "partd","snp","eghp","org_name","org_marketing_name",
                                       "plan_name","parent_org","contract_date"),
                         col_types = cols(
                           contractid = col_character(),
                           planid = col_double(),
                           org_type = col_character(),
                           plan_type = col_character(),
                           partd = col_character(),
                           snp = col_character(),
                           eghp = col_character(),
                           org_name = col_character(),
                           org_marketing_name = col_character(),
                           plan_name = col_character(),
                           parent_org = col_character(),
                           contract_date = col_character()
                         ))

  contract.info = contract.info %>%
    group_by(contractid, planid) %>%
    mutate(id_count=row_number())
    
  contract.info = contract.info %>%
    filter(id_count==1) %>%
    select(-id_count)
    
    ## Enrollments per plan
  ma.path=paste0("data/input/CPSC_Enrollment_Info_2015_01.csv")
  enroll.info=read_csv(ma.path,
                       skip=1,
                       col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
                       col_types = cols(
                       contractid = col_character(),
                       planid = col_double(),
                       ssa = col_double(),
                       fips = col_double(),
                       state = col_character(),
                       county = col_character(),
                       enrollment = col_double()
                       ),na="*")
    

  ## Merge contract info with enrollment info
  plan.data = contract.info %>%
    left_join(enroll.info, by=c("contractid", "planid")) %>%
    mutate(year=2015)
    
  ## Fill in missing fips codes (by state and county)
  plan.data = plan.data %>%
    group_by(state, county) %>%
    fill(fips)

  ## Fill in missing plan characteristics by contract and plan id
  plan.data = plan.data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan.data = plan.data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)
    
  ## Collapse from monthly data to yearly
  plan.year = plan.data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) %>%
    rename(avg_enrollment=enrollment)
  
  write_rds(plan.year,paste0("data/output/ma_data_2015.rds"))

## Load in SA
    ma.path=paste0("data/input/MA_Cnty_SA_2015_01.csv")
    service.area=read_csv(ma.path,skip=1,
                          col_names=c("contractid","org_name","org_type","plan_type","partial","eghp",
                                      "ssa","fips","county","state","notes"),
                          col_types = cols(
                            contractid = col_character(),
                            org_name = col_character(),
                            org_type = col_character(),
                            plan_type = col_character(),
                            partial = col_logical(),
                            eghp = col_character(),
                            ssa = col_double(),
                            fips = col_double(),
                            county = col_character(),
                            notes = col_character()
                          ), na='*')
      service.area.clean = service.area %>% select("contractid","org_name","org_type","plan_type","ssa","fips","county","state","notes")

write_rds(service.area.clean,paste0("data/output/sa_data_2015.rds"))

# Question 1.
# Inner merge
merged_data <- inner_join(service.area.clean, plan.year, by = c("contractid", "org_type", "org_name", "ssa", "fips", "county", "state", "plan_type"))
# Count number of occurrences of each plan type
plan_summary <- merged_data %>%
  group_by(plan_type) %>%
  summarise(count = n())

# Print the summary table
print(plan_summary)

# Question 2. 
filtered_data <- merged_data %>%
  filter(snp != "Yes",   # Exclude SNP plans
         eghp != "Yes",  # Exclude eghp plans
         !grepl("^800", planid))  # Exclude "800-series" plans
# Count number of occurrences of each plan type
plan_summary_filtered <- filtered_data %>%
  group_by(plan_type) %>%
  summarise(count = n())

# Print the summary table
print(plan_summary_filtered)

#Question 3
average_enrollment = merged_data %>% select("plan_type","avg_enrollment").reset_index(drop=True)

# Display the resulting table
print(average_enrollment)