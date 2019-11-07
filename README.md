# Shiny visualization tools for clinical trials

For Phase I visualization, the input data should be prepared as follows:

Input must be a csv file.

| ID | Evaluable  | Therapy_Start_Date | Dose_Level | Last_Assessment_Date | DLT |
| :-----: | :-: | :-: |:-: |:-: |:-: |
| 1 | 1 | 3/11/18 | 2 | 11/3/19 |0 |
| 2 | 1 | 11/24/14 | 2 | 1/20/16 |0 |
| 3 | 0 | 10/22/13 | 2 | 10/21/13 |1 |

**ID**: Patient ID. Each patient should have a unique ID.  
**Evaluable**: whether the data point is evaluable (1 = evaluable, 0 = non-evaluable).  
**Therapy_Start_Date**: trial start date (mm/dd/yy).  
**Dose_Level**: dose level, numeric value.  
**Last_Assessment_Date**: last follow up date (mm/dd/yy).  
**DLT**: whether there is a dose limiting toxicity (1 = yes, 0 = no).  

**Need to makes sure the column names are exactly the same as above, otherwise there would be an error.**
