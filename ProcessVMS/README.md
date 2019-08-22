## Process Vessel Monitoring System (VMS) Data


There are three subprojects in processing VMS data: pre-processing PacFIN fish tickets and VMS data, matching VMS to fish tickets, and interpolation of missing VMS data.
<br>

### Current Workflow with Scripts:
![img-preprocess-workflow](https://github.com/mfisher5/VMS-repo/blob/master/ProcessVMS/methods/preprocess_scripts_workflow.png?raw=true)
![img-match-workflow](https://github.com/mfisher5/VMS-repo/blob/master/ProcessVMS/methods/match_scripts_workflow.png?raw=true)
![img-regularize-workflow](https://github.com/mfisher5/VMS-repo/blob/master/ProcessVMS/methods/regularize_scripts_workflow.png?raw=true)







### Directory Structure

`methods`: images and ppt to keep track of methods. most content pasted into readme docs.

`R_Output`: intermediate .csv, .dbf files output from R for both fish ticket and VMS data. Also includes visualizations saved as .png files.

`resources`: 

`results`: summaries of results

`scripts`: all R scripts used to process VMS data.

`verify_code`: R scripts, data, workspaces used to verify code