# EFP_shift_share_proj
 Shift-share analysis, .r project

Data pulled from Quarterly Census of Employment & Wages - QCEW: State and County Employment and Wages
https://www.bls.gov/data/
Note: Private Sector Jobs

1) Create 4 folders: "Local", "State", "National", "Output"

2) Download employment data: https://www.bls.gov/data/   (Takes the longest time) //
	Quarterly Census of Employment & Wages - QCEW: State and County Employment and Wages
	one screen data search
Download national and statewide employment numbers:
	Select 	US + US total + total, all industries **
		California + statewide + total, all industries **
		California + [county] + total, all industries **
	save as US_tot, CA_tot, loc_tot into "National", "State", "Local" folders respectively.
Download national, statewide and county employment numbers per industry
	(industry codes to download are listed in the R script). 
	save as US_#, CA_#, loc_# into "National", "State", "Local" folders respectively.
**All downloads should include only private ownership, all establishment sizes, and all employees.

run "shift_share_proj.R". Creates "Shift_share.xlsx". 
	Use that to plug into the two LaTeX documents (manual entry for now, automate this later). 
	Also creates graphs.
