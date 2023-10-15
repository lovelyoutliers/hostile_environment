//////////////////////////////////////////////////////////////////////////////
// Difference-in-differences analysis 										//
//																			//	
// Updated Sept 2023														//
// J Dyxhoorn																//
//////////////////////////////////////////////////////////////////////////////


log using "...\cc_analysis_aug3.smcl", replace

use "...\didnanalysis_v2.dta", clear



//////////////////////////////////////////////////////////////////////////////
* COMPLETE CASE ANALYSIS 
drop if agenew==.
drop if edu==.
drop if citizen==.
drop if urban==.
drop if marital==.
drop if ethnicity==.
drop if ghq2_==.


*Descriptive statistics
tab sex  if wave==1, m
	sum agenew if wave==1
tab edu if wave==1, m
tab citizen  if wave==1, m
tab urban  if wave==1, m
tab marital  if wave==1, m
*tab jobnew  if wave==1, m
	sum ghq2_ if wave==1 

*by ethnicity - supplemental table 1
bysort ethnicity: tab sex  if wave==1, m
	bysort ethnicity: sum agenew if wave==1
bysort ethnicity: tab edu if wave==1, m
bysort ethnicity: tab citizen  if wave==1, m
bysort ethnicity: tab urban  if wave==1, m
bysort ethnicity: tab marital  if wave==1, m
bysort ethnicity: tab jobnew  if wave==1, m
	bysort ethnicity: sum ghq2_ if wave==1 


*Interaction
regress ghq2_ era ib6.ethnicity
	estimates store m1

regress ghq2_ era##ib6.ethnicity
	est store m2
lrtest m1 m2

*DID in complete case (un-imputed)
set showbaselevels on
 
mixed  ghq2_ era##ib6.ethnicity [pweight=analysisweight10] || psu: 
	margins era#ethnicity	
	marginsplot
mixed, coeflegend

	margins era#ethnicity	
	marginsplot

test 1.ethnicity = 6.ethnicity
test 2.ethnicity = 6.ethnicity
test 3.ethnicity = 6.ethnicity
test 4.ethnicity = 6.ethnicity
test 5.ethnicity = 6.ethnicity

		 
*Adjusted (complete case, uniimputed)
mixed  ghq2_ era##ib6.ethnicity sex agenew marital edu citizen urban [pweight=analysisweight10] || psu: 
 margins era#ethnicity	

*Citizenship status
 mixed  ghq2_ era##citizen [pweight=analysisweight10] || psu: 
  margins era#citizen
	marginsplot
	
 mixed  ghq2_ era##citizen sex agenew marital edu urban ethnicity [pweight=analysisweight10] || psu: 
  margins era#citizen
	marginsplot

log close
/////////////////////////////////////////////////////////////////////////////////
* IMPUTED ANALYSIS 
log using "...\mi_analysis_aug3.smcl", replace 
use "...\didnanalysis_v2.dta", clear
 
*Descriptives
bysort ethnicity: tab sex  if wave==1, m
	bysort ethnicity: sum agenew if wave==1
bysort ethnicity: tab edu if wave==1, m
bysort ethnicity: tab citizen  if wave==1, m
bysort ethnicity: tab urban  if wave==1, m
bysort ethnicity: tab marital  if wave==1, m
bysort ethnicity: tab jobnew  if wave==1, m
	bysort ethnicity: sum ghq2_ if wave==1 

//Imputation
mi set mlong
mi xtset, clear

mi register imputed ghq2_1 ghq2_2 ghq2_3 ghq2_4 ghq2_5 ghq2_6 ghq2_7 ghq2_8 ghq2_9 ghq2_10 citizen1 citizen2 citizen3 citizen4 citizen5 citizen6 citizen7 citizen8 citizen9 citizen10 sf12p1 sf12p2 sf12p3 sf12p4 sf12p5 sf12p6 sf12p7 sf12p8 sf12p9 sf12p10 fimnnet_dv2 fimnnet_dv3 fimnnet_dv4 fimnnet_dv5 fimnnet_dv6 fimnnet_dv7 fimnnet_dv8 fimnnet_dv9 fimnnet_dv10 

mi impute chained (regress) ghq2_1 ghq2_2 ghq2_3 ghq2_4 ghq2_5 ghq2_6 ghq2_7 ghq2_8 ghq2_9 ghq2_10 sf12p2 sf12p3 sf12p4 sf12p5 sf12p6 sf12p7 sf12p8 sf12p9 sf12p10 fimnnet_dv2 fimnnet_dv3 fimnnet_dv4 fimnnet_dv5 fimnnet_dv6 fimnnet_dv7 fimnnet_dv8 fimnnet_dv9 fimnnet_dv10 = urban1 agenew1 fimnnet_dv1 country1 transind1 transind2 transind3 transind4 transind5 transind6 transind7 transind8 transind9 transind10 postind1 postind2 postind3 postind4 postind5 postind6 postind7 postind8 postind9 postind10 transpak1 transpak2 transpak3 transpak4 transpak5 transpak6 transpak7 transpak8 transpak9 transpak10 postpak1 postpak2 postpak3 postpak4 postpak5 postpak6 postpak7 postpak8 postpak9 postpak10 transbang1 transbang2 transbang3 transbang4 transbang5 transbang6 transbang7 transbang8 transbang9 transbang10 postbang1 postbang2 postbang3 postbang4 postbang5 postbang6 postbang7 postbang8 postbang9 postbang10 transcar1 transcar2 transcar3 transcar4 transcar5 transcar6 transcar7 transcar8 transcar9 transcar10 postcar1 postcar2 postcar3 postcar4 postcar5 postcar6 postcar7 postcar8 postcar9 postcar10 transaf1 transaf2 transaf3 transaf4 transaf5 transaf6 transaf7 transaf8 transaf9 transaf10 postaf1 postaf2 postaf3 postaf4 postaf5 postaf6 postaf7 postaf8 postaf9 postaf10 transera1 transera2 transera3 transera4 transera5 transera6 transera7 transera8 transera9 transera10 postera1 postera2 postera3 postera4 postera5 postera6 postera7 postera8 postera9 postera10 indian bangladeshi pakistani caribbean african [pweight=analysisweight10], add(20) rseed(3732) noisily


//DID analysis
mi reshape long agenew fimnnet_dv country ghq2_ jobnew edu citizen sf12p marital urban era ioutcome, i(pidp) j(wave)
mi xtset pidp wave

*Unadjusted (imputed)
mi estimate: mixed  ghq2_ era##ib6.ethnicity [pweight=analysisweight10] || psu: 

*Adjusted (imputed) 
mi estimate: mixed  ghq2_ era##ib6.ethnicity sex agenew marital edu citizen urban   [pweight=analysisweight10] || psu: 

*Citizenship status 
mi estimate: mixed  ghq2_ era##i.citizen [pweight=analysisweight10] || psu: 
mi estimate: mixed  ghq2_ era##i.citizen sex agenew marital edu urban   [pweight=analysisweight10] || psu: 
mi estimate: mixed  ghq2_ era##i.citizen sex agenew marital edu urban ethnicity  [pweight=analysisweight10] || psu: 
log close
/////////////////////////////////////////////////////////////////////////////////



