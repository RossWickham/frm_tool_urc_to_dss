# frm_tool_urc_to_dss
Converts a URC from the Excel FRM tool and saves as DSS time series 

# Running the Tool

The easiest way to run this tool is by running the 'excel_urc_to_dss.bat' file.

This tool runs using R software bundle.  A pre-packaged version of all
  necessary sofware to run the tool is provided in the resources folder:
  
  R 4.0.3 with dssrip package for DSS read/write
  RStudio
  Java
  
The tool may also be ran by loading the main.R script into Rstudio (using
  the provided R v4.0.3 with dssrip package), and sourcing the script.
  
The default behavior of the tool is to process all tabs corresponding to
  months prior to now.  This behavior can be overriden by setting the
  flag 'processAllMonths' flag to False.

# URC Excel Format

Anticipated raw URCs in Excel (as text all on one column):
          units   Oct 31  Nov 30  Dec 31  Jan 31  Feb 28  Mar 31  Apr 15  Apr 30  May 31  Jun 30  Jul 31  Aug 31  Sep 30
 MCDB       kaf      200     200     390    1662    2810    4080    4080    4080    2448       0       0       0       0
 ARDB        ft   1442.0  1442.0  1438.5  1430.5  1422.9  1414.1  1414.1  1414.1  1423.9  1443.2  1444.0  1444.0  1444.0
 LIB         ft   2459.0  2448.0  2411.0  2388.9  2370.5  2363.2  2363.2  2363.2  2422.1  2459.0  2459.0  2459.0  2459.0
 DCDB        ft   1892.0  1892.0  1868.0  1839.3  1812.5  1807.7  1807.7  1807.7  1834.5  1877.3  1892.0  1892.0  1892.0
 HGH         ft   3555.8  3555.8  3549.0  3544.1  3539.4  3534.1  3531.4  3528.8  3552.6  3560.0  3560.0  3560.0  3560.0
 GCL         ft   1290.0  1290.0  1290.0  1290.0  1290.0  1281.8  1269.9  1257.9  1274.6  1289.7  1290.0  1290.0  1290.0
 BRN         ft   2077.0  2077.0  2077.0  2077.0  2056.1  2057.8  2061.9  2062.8  2074.9  2077.0  2077.0  2077.0  2077.0
 DWR         ft   1581.5  1568.8  1558.0  1527.6  1500.1  1459.9  1450.0  1490.4  1564.3  1599.0  1600.0  1600.0  1587.5
 SKQ         ft      n/a     n/a     n/a     n/a     n/a     n/a  2883.0     n/a  2890.0  2893.0  2893.0  2893.0  2893.0
 ALF         ft   2062.5  2062.5  2062.5  2060.0  2060.0  2056.0     n/a  2056.0  2062.5  2062.5  2062.5  2062.5  2062.5
 
 The script reads URCs from the 'urc_tables' folder, with Excel files stored
   named after the water year (e.g., '2022.xlsx').  Tabs within the Excel
   are named after the forecast month (e.g., "JAN").

# Configuration
 
 The main.R script allows you to configure which water year to read
   for the URCs.  By default, the current water year will be used.
 
 