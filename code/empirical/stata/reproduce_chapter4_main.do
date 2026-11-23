* ============================================================================
* Chapter 4 main analysis: one-file Stata reproduction
* ============================================================================
* This do-file is deliberately self-contained. It reads the raw WBES survey,
* the ITC creative-industry code list, World Bank controls, and the final
* six-channel semantic-score CSV. It then creates the analysis sample and
* exports the descriptive statistics and regression results used in Chapter 4.
*
* The semantic scores are the only substantive analysis data produced outside
* Stata. The Excel workbook is used only for transparent lookup mappings. No R
* regression result, RDS file, or previously created Stata result is used.
* ============================================================================

version 17.0
clear all
set more off
set linesize 255

* -----------------------------------------------------------------------------
* STEP 1. Find the project folder and define the six required inputs.
*
* The script can be run either from the project root or from this Stata folder.
* The path check below makes both ways of running the file work. Results are
* written as simple CSV files in output/tables/.
* -----------------------------------------------------------------------------

local root = c(pwd)
capture confirm file "`root'/data/raw/wbes/New_Comprehensive_April_29_2026.dta"
if _rc {
    capture confirm file "`root'/../../../data/raw/wbes/New_Comprehensive_April_29_2026.dta"
    if !_rc local root "`root'/../../.."
}
local output_dir "`root'/output/tables"
local raw_file "`root'/data/raw/wbes/New_Comprehensive_April_29_2026.dta"
local code_file "`root'/data/mappings/itc_creative_isic_codes_rev5_rev4_rev31_only.csv"
local mapping_file "`root'/data/mappings/chapter4_crosswalks.xlsx"
local country_mapping_file "`root'/data/external/analysis_inputs/creative_only_wb_region_country_mapping.csv"
local wdi_file "`root'/data/external/WB_WDI.dta"
local semantic_scores "`root'/data/semantic_scores/smeco2027_six_channels_transformer/unesco_smeco2027_six_channel_scores_country_year.csv"
global CH4_MAX_SEMANTIC_AGE 4

foreach file in "`raw_file'" "`code_file'" "`mapping_file'" "`country_mapping_file'" "`wdi_file'" "`semantic_scores'" {
    capture confirm file "`file'"
    if _rc {
        di as error "Required input not found: `file'"
        exit 601
    }
}

* -----------------------------------------------------------------------------
* STEP 2. Import the crosswalks used to connect the source files.
*
* These mappings are stored in data/mappings/chapter4_crosswalks.xlsx so that
* country-code and country-name changes can be reviewed or updated without
* editing the analysis code. Each lookup is saved as a temporary Stata file.
* -----------------------------------------------------------------------------
tempfile iso_map
import excel using "`mapping_file'", sheet("iso2_iso3") firstrow clear
isid iso2
save `iso_map', replace

* The main Word report used the 72-country WBES mapping in this CSV. Keeping
* the eligible-country list as a data file makes the sample restriction visible
* without hard-coding country names in the do-file.
tempfile eligible_wbes_countries
import delimited using "`country_mapping_file'", varnames(1) clear
rename wbescountry wbes_country_map
rename iso2 iso2_wbes
keep wbes_country_map iso2_wbes
gen str100 unesco_key = lower(wbes_country_map)
replace unesco_key = ustrnormalize(unesco_key, "nfd")
replace unesco_key = ustrregexra(unesco_key, "\p{M}", "")
replace unesco_key = ustrregexra(unesco_key, "[^A-Za-z0-9]", "")
keep unesco_key iso2_wbes
duplicates drop
isid unesco_key
save `eligible_wbes_countries', replace

tempfile country_aliases_wbes wb_regions channel_map
import excel using "`mapping_file'", sheet("country_aliases") firstrow clear
keep if source_system == "wbes"
rename source_key unesco_key
rename canonical_key canonical_unesco_key
recast str100 unesco_key canonical_unesco_key
keep unesco_key canonical_unesco_key
isid unesco_key
save `country_aliases_wbes', replace

import excel using "`mapping_file'", sheet("wb_regions") firstrow clear
rename region_code wb_region_code
isid wb_region_code
save `wb_regions', replace

import excel using "`mapping_file'", sheet("semantic_channels") firstrow clear
isid channel
save `channel_map', replace

* -----------------------------------------------------------------------------
* STEP 3. Prepare the World Development Indicators (WDI) controls.
*
* We keep GDP per capita, population, and electricity access. GDP and population
* enter the regressions in logarithms. Electricity access is kept in levels;
* its missing values are replaced by the sample median, while a missingness
* indicator is retained so that the imputation itself is observable in the model.
* `isid iso3` checks that the WDI merge will not duplicate firms.
* -----------------------------------------------------------------------------
tempfile wdi_controls
use ISO_code WDI_GDPpc2022_current WDI_population WDI_accesstoelectricity using "`wdi_file'", clear
rename ISO_code iso3
rename WDI_GDPpc2022_current wdi_gdp_pc_2022
rename WDI_population wdi_population_2022
rename WDI_accesstoelectricity wdi_electricity_access_2022
gen double log_wdi_gdp_pc_2022 = ln(wdi_gdp_pc_2022) if wdi_gdp_pc_2022 > 0
gen double log_wdi_population_2022 = ln(wdi_population_2022) if wdi_population_2022 > 0
gen byte wdi_elec_missing = missing(wdi_electricity_access_2022)
quietly summarize wdi_electricity_access_2022 if !missing(wdi_electricity_access_2022), detail
local wdi_elec_median = r(p50)
replace wdi_electricity_access_2022 = `wdi_elec_median' if missing(wdi_electricity_access_2022)
keep iso3 log_wdi_gdp_pc_2022 log_wdi_population_2022 wdi_electricity_access_2022 wdi_elec_missing
isid iso3
save `wdi_controls', replace

* -----------------------------------------------------------------------------
* STEP 4. Prepare the ITC creative-industry classification.
*
* The mapping file contains semicolon-separated Rev.4 and Rev.3.1 code lists.
* We split both lists into one code per row. The main-report sample includes
* direct Rev.4 creative firms and Rev.3.1-mapped creative firms.
* -----------------------------------------------------------------------------
tempfile creative_codes_rev4 creative_codes_rev31
import delimited using "`code_file'", varnames(1) clear
preserve
    split isic_rev4_codes, parse(";") gen(code)
    reshape long code, i(isic_rev5_code) j(code_part)
    destring code, replace force
    rename code isic_rev4_code
    keep isic_rev4_code
    drop if missing(isic_rev4_code)
    duplicates drop
    save `creative_codes_rev4', replace
restore
keep isic_rev31_codes
gen long maprow = _n
split isic_rev31_codes, parse(";") gen(code)
reshape long code, i(maprow) j(code_part)
destring code, replace force
rename code isic_rev31_code
keep isic_rev31_code
drop if missing(isic_rev31_code)
duplicates drop
save `creative_codes_rev31', replace

* -----------------------------------------------------------------------------
* STEP 5. Read and clean the raw World Bank Enterprise Survey (WBES) data.
*
* Only variables used by the main specification are loaded. This makes the
* source of every outcome, control, and identifier visible in one place.
*
* The raw file is stored in a synced Google Drive folder. Google Drive can
* reject Stata's random-access read of a large .dta file, so the script copies
* it to Stata's local temporary directory before importing it when needed.
* -----------------------------------------------------------------------------
local raw_file_to_use "`raw_file'"
capture confirm file "/private/tmp/ch4_wbes_for_count.dta"
if !_rc {
    di as text "Using the existing local WBES copy in /private/tmp."
    local raw_file_to_use "/private/tmp/ch4_wbes_for_count.dta"
}
else if strpos("`raw_file'", "/CloudStorage/") > 0 {
    di as text "Copying the WBES file to a local temporary directory before import."
    tempfile local_raw_file
    copy "`raw_file'" "`local_raw_file'", replace
    local raw_file_to_use "`local_raw_file'"
}
use idstd country region wt d1a2 d1a2_v4 d2 n3 l1 l2 l6 n2a d3b d3c b1 b2a b3 b4 b5 ///
    h2 h5 r8 e33 f1 d40a d14a k35 l11a k30 k21 k4 c43 ge8d using "`raw_file_to_use'", clear
gen int survey_year = real(substr(country, -4, 4))
gen str80 wbes_country_name = substr(country, 1, length(country) - 4)

* Use the ISO2 code in the external country mapping as the common country key.
* This avoids hard-coded country-name conversions in the empirical code.
gen str100 unesco_key = lower(wbes_country_name)
replace unesco_key = ustrnormalize(unesco_key, "nfd")
replace unesco_key = ustrregexra(unesco_key, "\p{M}", "")
replace unesco_key = ustrregexra(unesco_key, "[^A-Za-z0-9]", "")
merge m:1 unesco_key using `country_aliases_wbes', keep(master match) nogen
replace unesco_key = canonical_unesco_key if !missing(canonical_unesco_key)
drop canonical_unesco_key
merge m:1 unesco_key using `eligible_wbes_countries', keep(3) nogen
rename iso2_wbes iso2

foreach v in wt d2 n3 l1 l2 l6 n2a d3b d3c b1 b2a b3 b4 b5 ///
    h2 h5 r8 e33 f1 d40a d14a k35 l11a k30 k21 k4 c43 ge8d {
    replace `v' = . if `v' < 0
}
foreach v in h2 h5 r8 e33 k21 k4 c43 ge8d {
    * These variables are binary in the analysis. WBES code 2 means "No";
    * other non-binary values are treated as missing.
    replace `v' = 0 if `v' == 2
    replace `v' = . if !inlist(`v', 0, 1)
}

* Match direct Rev.4 codes and Rev.3.1 codes separately. A firm is classified
* using the direct Rev.4 code when that code exists; otherwise the Rev.3.1
* code is used. This reproduces the two sample components in the main report.
gen int isic_rev4_code = round(d1a2_v4) if d1a2_v4 > 0 & d1a2_v4 <= 9999
gen int isic_rev31_code = round(d1a2) if missing(isic_rev4_code) & d1a2 > 0 & d1a2 <= 99999
merge m:1 isic_rev4_code using `creative_codes_rev4'
gen byte creative_rev4 = (_merge == 3) if !missing(isic_rev4_code)
drop _merge
merge m:1 isic_rev31_code using `creative_codes_rev31'
gen byte creative_rev31 = (_merge == 3) if !missing(isic_rev31_code)
drop _merge
gen byte creative = creative_rev4 if !missing(isic_rev4_code)
replace creative = creative_rev31 if missing(isic_rev4_code)

* Construct the raw firm-level controls. Their sample-specific transformations
* are applied after the Chapter 4 population is selected below.
gen double log_employment_raw = ln(l1) if l1 > 0
gen double log_employment = log_employment_raw

* Age, ownership, and female ownership are imputed at their medians. For each
* variable, the corresponding missingness indicator remains in the regression.
gen double firm_age_years = survey_year - b5 if !missing(survey_year, b5)
replace firm_age_years = . if firm_age_years < 0 | firm_age_years > 250
gen double log_firm_age_raw = ln(1 + firm_age_years)
gen double female_owner_raw = 1 if b4 == 1
replace female_owner_raw = 0 if b4 == 2
gen byte log_firm_age_missing = missing(log_firm_age_raw)
gen byte female_owner_missing = missing(female_owner_raw)
gen double log_firm_age = log_firm_age_raw
gen double female_owner = female_owner_raw
quietly egen double ch4_age_median = median(log_firm_age_raw)
quietly summarize ch4_age_median in 1, meanonly
local med_age = r(mean)
drop ch4_age_median
if missing(`med_age') local med_age = 0

* Convert categorical variables to strings before encoding them as fixed-effect
* indicators later. The industry fixed effect uses the first two Rev.4 digits.
gen str8 legal_status = "missing"
replace legal_status = string(b1, "%1.0f") if inrange(b1, 1, 6)
gen str8 industry_division = "missing"
gen int industry_code = isic_rev4_code
replace industry_code = isic_rev31_code if missing(industry_code)
gen str4 isic_code_string = string(industry_code, "%04.0f")
replace industry_division = substr(isic_code_string, 1, 2) if !missing(industry_code)

* Translate the WBES numeric region code using the workbook mapping. The -99
* row in that sheet provides a readable label for missing region codes.
gen int wb_region_code = region
replace wb_region_code = -99 if missing(wb_region_code)
merge m:1 wb_region_code using `wb_regions', keep(master match) nogen
rename region_label wb_region
drop wb_region_code

* Create the two derived outcomes used in the main tables:
* any_exports = whether domestic and foreign export shares sum to more than 0;
* finance_obstacle_any = whether finance is reported as an obstacle at all.
gen double total_export_share = d3b + d3c if !missing(d3b, d3c)
gen byte any_exports = (total_export_share > 0) if !missing(total_export_share)
gen byte finance_obstacle_any = (k30 >= 1) if !missing(k30)

* Restrict the raw survey to the Chapter 4 population: creative firms surveyed
* from 2021 onward with a valid positive survey weight. Then retain only the
* variables needed by the score-matching and estimation stages.
keep if survey_year >= 2021 & creative == 1 & wt > 0 & !missing(wt)

* Winsorize and impute using the selected Chapter 4 population. This is the
* same order used by the report-producing analysis and keeps the regression
* controls independent of unrelated survey years and industries.
quietly summarize log_employment_raw if !missing(log_employment_raw), detail
local emp_p1 = r(p1)
local emp_p99 = r(p99)
replace log_employment = `emp_p1' if log_employment < `emp_p1' & !missing(log_employment)
replace log_employment = `emp_p99' if log_employment > `emp_p99' & !missing(log_employment)
replace log_firm_age = `med_age' if missing(log_firm_age)
quietly summarize female_owner_raw if !missing(female_owner_raw), detail
local med_female = r(p50)
if missing(`med_female') local med_female = 0
replace female_owner = `med_female' if missing(female_owner)

keep idstd country survey_year wbes_country_name unesco_key iso2 wt creative isic_rev4_code isic_rev31_code ///
    h2 h5 r8 e33 f1 d40a d14a k35 l11a k30 k21 k4 c43 ge8d any_exports ///
    finance_obstacle_any log_firm_age log_firm_age_missing female_owner ///
    female_owner_missing log_employment legal_status industry_division wb_region

tempfile raw_before_scores
save `raw_before_scores', replace
global CH4_RAW "`raw_before_scores'"
global CH4_ISO_MAP "`iso_map'"
global CH4_CHANNELS "`channel_map'"
global CH4_WDI "`wdi_controls'"

* -----------------------------------------------------------------------------
* STEP 6. Build the final firm-level analysis sample.
*
* This program is written as a small reusable block so the matching logic is
* easy to see. It takes the raw creative-firm sample and the one semantic-score
* CSV, then returns one row per WBES firm with six channel scores and controls.
* -----------------------------------------------------------------------------
capture program drop build_chapter4_sample
program define build_chapter4_sample
    syntax, SCOREFILE(string) SAVING(string)
    use $CH4_RAW, clear
    tempfile scores score_meta score_wide
    preserve
        * Read only the fields needed from the semantic file. `policy_count` is
        * the overall policy exposure; `bounded_score_mean` is the channel score.
        import delimited using "`scorefile'", varnames(1) clear
        keep country iso2 year channel policy_count bounded_score_mean
        rename country policy_country
        keep if policy_country != "CE / no country title"
        * Keep exactly the six channels used in Chapter 4 by merging the channel
        * list from the workbook. Any other channel is excluded automatically.
        merge m:1 channel using $CH4_CHANNELS, keep(3) nogen
        duplicates drop iso2 year channel, force
        save `scores', replace
    restore

    * For each firm, use the most recent completed report before the survey.
    * The report must be strictly earlier than the survey year and no more than
    * four years old. This is the previous-cycle timing rule used in the Word
    * report: year < survey_year and year >= survey_year - 4.
    joinby iso2 using `scores'
    keep if year < survey_year & year >= survey_year - $CH4_MAX_SEMANTIC_AGE
    gsort idstd channel -year
    bysort idstd channel: keep if _n == 1
    bysort idstd: egen n_channels = count(channel)
    * Require all six channels. Firms with incomplete channel coverage are not
    * assigned a partial semantic treatment and therefore leave the sample.
    keep if n_channels == 6

    preserve
        keep idstd policy_country iso2 year policy_count
        bysort idstd: keep if _n == 1
        rename year policy_report_year
        save `score_meta', replace
    restore

    * The channel workbook merge above supplies the short names. Reshape the
    * six long channel rows into one column per channel, then merge those
    * columns back to the firm-level WBES data.
    keep idstd channel_short bounded_score_mean
    reshape wide bounded_score_mean, i(idstd) j(channel_short) string
    rename bounded_score_meanip ip_focus
    rename bounded_score_meanbrand brand_focus
    rename bounded_score_meanmarket market_focus
    rename bounded_score_meandigital digital_focus
    rename bounded_score_meanfinance finance_focus
    rename bounded_score_meansustain sustain_focus
    save `score_wide', replace

    use $CH4_RAW, clear
    merge 1:1 idstd using `score_wide', keep(3) nogen
    merge 1:1 idstd using `score_meta', keep(3) nogen
    * These identifiers are used for fixed effects and for counting the number
    * of countries and country-wave observations in the regression sample.
    gen str100 cluster_country = policy_country
    replace cluster_country = wbes_country_name if missing(cluster_country)
    gen str120 country_year_fe = cluster_country + "_" + string(survey_year)
    gen str12 survey_year_fe = string(survey_year)

    merge m:1 iso2 using $CH4_ISO_MAP, keep(master match) nogen
    merge m:1 iso3 using $CH4_WDI, keep(master match) nogen

    * Normalize the WBES survey weights within each semantic country. This keeps
    * the relative firm weights but gives each country the same total weight.
    bysort cluster_country: egen double country_weight_total = total(wt)
    gen double analysis_weight = wt / country_weight_total
    keep if analysis_weight > 0 & !missing(analysis_weight, policy_count)
    drop country_weight_total

    sort country_year_fe
    by country_year_fe: gen byte first_wave = (_n == 1)
    * Convert each continuous exposure into the high/low indicator used in the
    * main specification. The cutoff is the median across country-wave groups,
    * so the same exposure is assigned consistently within a country-wave.
    foreach x in policy_count ip_focus brand_focus market_focus digital_focus ///
        finance_focus sustain_focus {
        quietly summarize `x' if first_wave & !missing(`x'), detail
        local median = r(p50)
        gen byte `x'_high = (`x' > `median') if !missing(`x')
        label variable `x'_high "Above country-wave median: `x'"
    }
    drop first_wave

    * Encode categorical identifiers so Stata can include them as fixed effects.
    encode survey_year_fe, gen(fe_year)
    encode industry_division, gen(fe_industry)
    encode legal_status, gen(fe_legal)
    encode wb_region, gen(fe_region)
    encode cluster_country, gen(fe_cluster)
    save "`saving'", replace
end

tempfile main_sample
build_chapter4_sample, scorefile("`semantic_scores'") saving("`main_sample'")

* Report the size of the final firm-level sample before outcome-specific
* missing values are applied. This is useful when comparing specifications.
quietly count
local ch4_sample_firms = r(N)
quietly egen byte ch4_country_tag = tag(cluster_country)
quietly count if ch4_country_tag
local ch4_sample_countries = r(N)
drop ch4_country_tag
quietly egen byte ch4_wave_tag = tag(country_year_fe)
quietly count if ch4_wave_tag
local ch4_sample_waves = r(N)
drop ch4_wave_tag
di as result "Final Chapter 4 sample: `ch4_sample_firms' firms, `ch4_sample_countries' countries, `ch4_sample_waves' country-waves"

* -----------------------------------------------------------------------------
* STEP 7. Calculate the descriptive high-versus-low comparisons.
*
* Each call below specifies one outcome and the corresponding semantic exposure.
* The helper program posts weighted means and unweighted firm counts to a small
* temporary results file, which is exported at the end of this section.
* -----------------------------------------------------------------------------
tempname desc_handle
tempfile desc_file
postfile `desc_handle' str50 family str24 outcome str8 exposure_group ///
    double weighted_mean long firms using `desc_file', replace
global CH4_DESC_HANDLE `desc_handle'

capture program drop post_chapter4_descriptive
program define post_chapter4_descriptive
    syntax, FAMILY(string) OUTCOME(name) EXPOSURE(name)
    quietly summarize `outcome' [aw=analysis_weight] if `exposure' == 1 & !missing(`outcome', analysis_weight), meanonly
    local high_mean = r(mean)
    quietly count if `exposure' == 1 & !missing(`outcome', analysis_weight)
    local high_n = r(N)
    quietly summarize `outcome' [aw=analysis_weight] if `exposure' == 0 & !missing(`outcome', analysis_weight), meanonly
    local low_mean = r(mean)
    quietly count if `exposure' == 0 & !missing(`outcome', analysis_weight)
    local low_n = r(N)
    post $CH4_DESC_HANDLE ("`family'") ("`outcome'") ("High") (`high_mean') (`high_n')
    post $CH4_DESC_HANDLE ("`family'") ("`outcome'") ("Low") (`low_mean') (`low_n')
end

use `main_sample', clear
post_chapter4_descriptive, family("Overall policy count") outcome(h2) exposure(policy_count_high)
post_chapter4_descriptive, family("Overall policy count") outcome(any_exports) exposure(policy_count_high)
post_chapter4_descriptive, family("Intellectual property") outcome(h5) exposure(ip_focus_high)
post_chapter4_descriptive, family("Intellectual property") outcome(r8) exposure(ip_focus_high)
post_chapter4_descriptive, family("Brand development and positioning") outcome(e33) exposure(brand_focus_high)
post_chapter4_descriptive, family("Brand development and positioning") outcome(r8) exposure(brand_focus_high)
post_chapter4_descriptive, family("Brand development and positioning") outcome(f1) exposure(brand_focus_high)
post_chapter4_descriptive, family("Market leverage and access") outcome(d40a) exposure(market_focus_high)
post_chapter4_descriptive, family("Market leverage and access") outcome(d14a) exposure(market_focus_high)
post_chapter4_descriptive, family("Digital capabilities and commerce") outcome(k35) exposure(digital_focus_high)
post_chapter4_descriptive, family("Digital capabilities and commerce") outcome(l11a) exposure(digital_focus_high)
post_chapter4_descriptive, family("Finance and financial capability") outcome(finance_obstacle_any) exposure(finance_focus_high)
post_chapter4_descriptive, family("Finance and financial capability") outcome(k4) exposure(finance_focus_high)
post_chapter4_descriptive, family("Finance and financial capability") outcome(k21) exposure(finance_focus_high)
use `main_sample', clear
post_chapter4_descriptive, family("Sustainability, standards and certification") outcome(c43) exposure(sustain_focus_high)
post_chapter4_descriptive, family("Sustainability, standards and certification") outcome(ge8d) exposure(sustain_focus_high)
post_chapter4_descriptive, family("Sustainability, standards and certification") outcome(h5) exposure(sustain_focus_high)
postclose `desc_handle'
use `desc_file', clear
sort family outcome exposure_group
format weighted_mean %21.17g
export delimited using "`output_dir'/chapter4_stata_descriptives.csv", replace

* -----------------------------------------------------------------------------
* STEP 8. Run the main weighted regressions.
*
* For every outcome, regress it on the matching high/low semantic exposure and
* the same firm and country controls. Year, industry, legal-status, and World
* Bank-region indicators are included as fixed effects. Standard errors are
* clustered by semantic country, the level at which the exposure is assigned.
* -----------------------------------------------------------------------------
tempname reg_handle
tempfile reg_file
postfile `reg_handle' str50 family str24 outcome ///
    double coefficient std_error p_value long n countries country_waves using `reg_file', replace
global CH4_REG_HANDLE `reg_handle'
global CH4_CONTROLS log_firm_age log_firm_age_missing female_owner ///
    female_owner_missing log_employment log_wdi_gdp_pc_2022 ///
    log_wdi_population_2022 wdi_electricity_access_2022 wdi_elec_missing

* This helper runs one regression, extracts the coefficient and diagnostics, and
* stores them in the results file used by the report-building step.
capture program drop run_chapter4_regression
program define run_chapter4_regression
    syntax, FAMILY(string) OUTCOME(name) EXPOSURE(name)
    quietly regress `outcome' `exposure' $CH4_CONTROLS ///
        i.fe_year i.fe_industry i.fe_legal i.fe_region ///
        [aw=analysis_weight] if analysis_weight > 0, vce(cluster fe_cluster)
    local b = _b[`exposure']
    local se = _se[`exposure']
    local n = e(N)
    local p = 2 * ttail(e(df_r), abs(`b' / `se'))

    * Count unique countries and country-wave groups in the exact estimation
    * sample (after Stata removes missing outcome/control observations).
    gen byte ch4_in_model = e(sample)
    preserve
        keep if ch4_in_model
        sort cluster_country
        by cluster_country: gen byte ch4_country_tag = (_n == 1)
        count if ch4_country_tag
        local countries = r(N)
        sort country_year_fe
        by country_year_fe: gen byte ch4_wave_tag = (_n == 1)
        count if ch4_wave_tag
        local waves = r(N)
    restore
    drop ch4_in_model
    post $CH4_REG_HANDLE ("`family'") ("`outcome'") ///
        (`b') (`se') (`p') (`n') (`countries') (`waves')
end

use `main_sample', clear
run_chapter4_regression, family("Overall policy count") outcome(h2) exposure(policy_count_high)
run_chapter4_regression, family("Overall policy count") outcome(any_exports) exposure(policy_count_high)
run_chapter4_regression, family("Intellectual property") outcome(h5) exposure(ip_focus_high)
run_chapter4_regression, family("Intellectual property") outcome(r8) exposure(ip_focus_high)
run_chapter4_regression, family("Brand development and positioning") outcome(e33) exposure(brand_focus_high)
run_chapter4_regression, family("Brand development and positioning") outcome(r8) exposure(brand_focus_high)
run_chapter4_regression, family("Brand development and positioning") outcome(f1) exposure(brand_focus_high)
run_chapter4_regression, family("Market leverage and access") outcome(d40a) exposure(market_focus_high)
run_chapter4_regression, family("Market leverage and access") outcome(d14a) exposure(market_focus_high)
run_chapter4_regression, family("Digital capabilities and commerce") outcome(k35) exposure(digital_focus_high)
run_chapter4_regression, family("Digital capabilities and commerce") outcome(l11a) exposure(digital_focus_high)
run_chapter4_regression, family("Finance and financial capability") outcome(finance_obstacle_any) exposure(finance_focus_high)
run_chapter4_regression, family("Finance and financial capability") outcome(k4) exposure(finance_focus_high)
run_chapter4_regression, family("Finance and financial capability") outcome(k21) exposure(finance_focus_high)
use `main_sample', clear
run_chapter4_regression, family("Sustainability, standards and certification") outcome(c43) exposure(sustain_focus_high)
run_chapter4_regression, family("Sustainability, standards and certification") outcome(ge8d) exposure(sustain_focus_high)
run_chapter4_regression, family("Sustainability, standards and certification") outcome(h5) exposure(sustain_focus_high)
postclose `reg_handle'
use `reg_file', clear
sort family outcome
format coefficient std_error p_value %21.17g
export delimited using "`output_dir'/chapter4_stata_regressions.csv", replace

* -----------------------------------------------------------------------------
* STEP 9. Confirm the two files created for the Chapter 4 report.
* -----------------------------------------------------------------------------
di as result "Chapter 4 Stata reproduction complete"
di as text "  Created: `output_dir'/chapter4_stata_descriptives.csv"
di as text "  Created: `output_dir'/chapter4_stata_regressions.csv"
