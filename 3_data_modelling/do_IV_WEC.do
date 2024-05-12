import delimited "C:\Users\wojci\OneDrive\WSZ\WEC2024\gminy.csv", clear 
replace type_20_50k = "0" if type_20_50k == "NA"
destring type_20_50k, replace

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) education_share_secondary education_share_vocational education_share_primary , robust

reg percent_vaccinated glosy_sld index_ineq education_share_secondary education_share_vocational education_share_primary , vce(robust)

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc , robust

reg percent_vaccinated glosy_sld index_ineq pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc, vce(robust)

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) v223 lat i.type_0_20k i.type_20_50k i.type_50_100k i.type_100_500k i.type_500k  , robust

reg percent_vaccinated glosy_sld index_ineq v223 lat i.type_0_20k i.type_20_50k i.type_50_100k i.type_100_500k i.type_500k  , vce(robust)

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat i.type_0_20k i.type_20_50k i.type_50_100k i.type_100_500k i.type_500k, robust

reg percent_vaccinated glosy_sld index_ineq education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat i.type_0_20k i.type_20_50k i.type_50_100k i.type_100_500k i.type_500k, vce(robust)

net_migrations_per_1000_persons 

preserve

keep if type_0_20k == 1 

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat, robust

restore 

preserve

keep if type_20_50k == 1 

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_mean index_ineq_mean) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat, robust

restore 

preserve
keep if type_50_100k == 1 

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_iv index_iv ) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat, robust

restore 

preserve
keep if type_100_500k == 1 

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_iv index_iv ) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat, robust

restore 

preserve
keep if type_500k == 1 

ivreg2 percent_vaccinated ( glosy_sld index_ineq = sld_iv index_iv ) education_share_secondary education_share_vocational education_share_primary  pop_f_all_perc pop_t_0_19_perc pop_t_20_29_perc pop_t_30_39_perc pop_t_40_49_perc pop_t_50_59_perc pop_t_60_84_perc v223 lat, robust

restore 

i.type_20_50k i.type_50_100k i.type_100_500k i.type_500k