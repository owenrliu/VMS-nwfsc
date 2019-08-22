## Vessel Monitoring System Data Analysis

This repository contains all files used to analyze vessel monitoring system (VMS) data. Data and RMarkdown output are not included for confidentiality.

### Project Overview

#### Part I. Process VMS Data: 
Create a pipeline to (1) pre-process VMS data, (2) match PacFIN fish ticket data to VMS data, and clean up VMS data from identified fishing trips, and (3) regularize VMS tracks

#### Part II. Home Ranges: 
Use utilization distributions to identify "home range" areas for groups.



<br>
<br>

### Repository Structure

`Input_Data`: VMS data processed in ArcGIS, raw PacFIN fish ticket data. File formats either .csv or .dbf

`ProcessVMS`: files associated with **Part I.**, processing VMS data for further analysis. Includes raw data exploration / filtering, matching VMS to PacFIN fish tickets, and interpolation of missing data points.

`HomeRange`: files associated with **Part II.**, calculating utilization distributions for use in home range analysis

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

*Additional documentation and most input / output from R is saved on Google Drive and NOAA server.*

