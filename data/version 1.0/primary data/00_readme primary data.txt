This folder contains primary data from release 1.0 of MECO-L2. 

The four sub-folders contain data of three types:

1) "Comprehension data": Accuracy data from comprehension questions presented to participants after each of the texts during the eye-tracking reading task. Note that files include data after removal of subjects who did not meet eye-tracking quality checks. Two files are provided:
- "joint_l2_acc.rda": number of correct responses for each participant (out of a total of 24 questions).
- "joint_l2_acc_breakdown.rda": number of correct responses for each participant in each text (out of 4). 
- "joint_l2_acc_full_breakdown.rda": correct (1) or incorrect (0) response for each participant in each question (i.e. trial-by-trial data). 


2) "Eye-tracking data": Includes files with eye-movement data from the reading task. Note that files include data after removal of subjects and passages that did not meet eye-tracking quality checks.  
- "joint.l2.readrate.rda": reading rate (words/minute) for each participant in each passage.
- "joint_data_l2_trimmed.rda": eye-tracking measures for each word (i.e. each interest area). 
For more information about variables and file structure see popeye github page, 
https://rdrr.io/github/sascha2schroeder/popEye/

3) "Individual-differences data": Files with data from non-eye-tracking tasks. Note that files include data after removal of subjects who did not meet quality checks. 
- "joint.ind.diff.l2.rda": provides a summary of subjects' scores in each task. For task description, please refer to the manuscript (supplementary materials). As noted in the manuscript, two scores were computed for the vocabulary task: one based on the whole task (vocab), and one based on a thousands 2-5 (vocab.t2.5). 
- the 'leap-q' folder: includes data from an abridged version of the Language Experience and Proficiency Questionnaire (LEAP-Q; Marian, Blumenfeld, & Kaushanskaya, 2007), with separate file for each site. 