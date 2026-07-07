
** Lecture One Exercise Six


* Install if needed
ssc install rdecompose

* Create data
clear
input str12 LAA long n_people long n_women15to44 int n_births
"Glasgow City" 650300 161105 6150
"Stirling" 94210 17844 659
end

* Compute factors
gen women_prop = n_women15to44 / n_people
gen births_prop = n_births / n_women15to44 * 1000

* Decomposition
rdecompose women_prop births_prop, group(LAA)

** Lecture One Exercise Seven

* Create data
clear
input str12 LAA long n_people long n_women long n_women15to44 int n_births
"Glasgow City" 650300 332054 161105 6150
"Stirling"     94210  48731  17844  659
end

* Compute factors

gen propw = n_women/n_people
gen  propw1544 = n_women15to44/n_women
gen  bpw = n_births/n_women15to44*1000

* Decomposition
rdecompose propw propw1544 bpw, group(LAA)


**

clear

input str10 country n_people n_women n_women1544 n_births
"Scotland"  5546900   2851740   1064441    45763
"England"   58620101  29895762  11583779   567708
end

* Crude birth rate (per 1,000)
gen cr = n_births / n_people * 1000

list country cr

* Factors
gen propw      = n_women / n_people
gen propw1544  = n_women1544 / n_women
gen bpw        = n_births / n_women1544

* Das Gupta decomposition
rdecompose propw propw1544 bpw, group(country)


** Gender earnings gap

clear
input str7 pop long working_age long labour_force long employees float avg_hours_pp double total_weekly_earnings
"males"   23718900 18442860 4823169 37.9 4240916038
"females" 24613110 5767405  280557  31.7 178762504
end


* create factors

* Create decomposition factors
gen size          = working_age
gen participation = labour_force / working_age
gen employment    = employees / labour_force
gen hours         = avg_hours_pp
gen earnings      = total_weekly_earnings / (hours * employees)

* Keep only the required variables
keep pop size participation employment hours earnings

rdecompose participation employment hours earnings, group(pop)


** Recycling

clear
import delimited "https://josiahpjking.github.io/sgsss_std_decomp/data/recycling_2011_2022.csv", varnames(1) clear


gen rpp = recycled/pop*1e3
gen wpp = totalwaste/pop*1e3
gen recp = recycled/totalwaste

rdecompose wpp recp, group(year)



** DG 2.1

clear

input str6 pop avg_earnings earner_prop
"Black" 10930 0.717892
"White" 16591 0.825974
end

gen crude_rate = avg_earnings * earner_prop

rdecompose avg_earnings earner_prop, group(pop)



*** Practical two

clear

input ///
    year str10 age convicted_population offenders reconvicted
2004 "21 to 25" 49351 10591 3861
2004 "26 to 30" 49351  7522 2595
2004 "31 to 40" 49351 12071 3487
2004 "over 40"  49351  7528 1447
2004 "under 21" 49351 11639 4587
2016 "21 to 25" 40606  6901 2044
2016 "26 to 30" 40606  6918 2024
2016 "31 to 40" 40606 11222 3332
2016 "over 40"  40606 11225 2237
2016 "under 21" 40606  4340 1398
end


gen rate = reconvicted/offenders
gen weight = offenders/convicted_population

tab weight

rdecompose weight rate, group(year) sum(age)


* age and sex

clear
import delimited "https://raw.githubusercontent.com/josiahpjking/sgsss_std_decomp/refs/heads/main/data/reconv.csv", varnames(1) clear

gen agesex = age + "." + sex

gen rate = reconvicted/offenders
gen weight = offenders/convicted_population

rdecompose weight rate, group(year) sum(age) multi detail



** homelessness deaths

import delimited "https://josiahpjking.github.io/sgsss_std_decomp/data/hdeaths.csv", varnames(1) clear

gen hdr = deaths/homeless
gen hr = homeless/scotpop

summarize hr

* descriptives

table scotpop

bysort year: egen total_scotpop = total(scotpop)
gen popshare = scotpop / total_scotpop

keep if year == 2017 | year == 2024

rdecompose hr hdr popshare, group(year) sum(age_group) func(sum(popshare*hr*hdr*1e5))



** recyling

import delimited "https://josiahpjking.github.io/sgsss_std_decomp/data/recycling_timeseries.csv", varnames(1) clear


* create the variables

gen wpp = totalwaste/pop
gen recp = recycled/totalwaste
gen lfp = landfilled/totalwaste
gen incp = diverted/totalwaste

rdecompose wpp recp, group(year) multi

rdecompose pop wpp recp, group(year) multi