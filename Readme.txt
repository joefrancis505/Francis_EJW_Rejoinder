This repository contains the following elements:

all_authors_with_citations_and_indicators.csv
Necessary elements from Magness and Makovi's database for use in the Structural_breaks.R and SCM_maker.R scripts.

JSTOR_1907-1916.odsThe calculations for Table 1 in the rejoinder.

JSTOR_sociology_correction.odsThe calculations showing how Magness and Makovi subtracted Annals from the numerator but not the denominator.SCM_donor_pool_analysis.ods
The donor pools used in the SCM_maker.R script.
SCM_maker_v2.RThe replication of some of Magness and Makovi's (2020; 2023) synthetic Marxes.SCM_p_values.csv
The p-values produced by the SCM_maker_v2.R script.

SCM_results
The folder containing the detailed output of SCM_maker_v2.R, including plots for all the donors in the various pools tested. The subfolders are listed below.
Structural_breaks.pdf
Figure 3 in the rejoinder, which is produced by the Structural_breaks.R script.Structural_breaks.RApplies the Bai-Perron structural break test to Marx's n-gram share from 1867 to 2000.


The SCM_results subfolders contain my replications of the following Synthetic Marxes and their donors, as created by the SCM_maker_v2.R script:

WP - The 97 donors used in the headline Synthetic Marx in Magness and Makovi (2020).
WP_Germans - The above plus 45 German-language authors added in Magness and Makovi (2023).
WP_Germans_Brainstorming - The above plus 3 authors added from brainstorming in Magness and Makovi (2023).
WP_Germans_Brainstorming_Pericles – The above plus Pericles added from political philosophy compilations in Magness and Makovi (2023).
JPE – The above plus 49 donors added from Harvard Universal Classics in Magness and Makovi (2023).
Germans_only – The 55 donors used in the headline Synthetic Marx and coded as cite_German = 1 by Magness and Makovi (2023).
Germans_only_Freud_Weber – The above with Sigmund Freud and Max Weber added.
JPE_German_language – 222 donors using cite_German, as in Magness and Makovi (2023).
Germans_only_German_language – As above, but restricted to 87 German-language donors, as in Magness and Makovi (2023).


