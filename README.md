# AAS-minCHAT-Checker
Allows annotators to automatically check for basic minCHAT errors in their transcriptions so that they can manually fix those errors and submit them.


**This script won't catch all the errors!!**

* It only catches errors as described below
* It might even catch some "errors" that are _in reality_ perfectly fine

It is up to you humans to fix those as needed! This is just a tool to help annotators do check the basic ACLEW Annotation Scheme and minCHAT standards used in their annotation files.


## Instructions

1. Export the .eaf file you want to check as a tab-delimited text (File > Save As > Tab-delimited text)
2. Go to [https://aclew.shinyapps.io/AAS-minCHAT-Checker/]
3. Upload the file and click Submit
4. Download the spreadsheet of possible errors detected

## What does it look for? It checks to see whether...

* the tier name is either 3 or 7 characters
* the tier name pre- and post-fixes match one of the limited types (e.g., "vcm", "CHI", "FA1")
* there are missing annotations (e.g., a missing LEX annotation when VCM = C)
* there are too many annotations
* the closed-vocabulary annotation values (e.g., XDS, VCM) are valid
* there are empty transcriptions
* transcriptions have too few or too many terminal markers
* the use of square brackets follows one of the following patterns: **[: blabla]**, **\<blabla\> [=! blabla]**, or **[- lng]**
* the use of @ follows one of the following patterns: **blabla@s:eng**, **blabla@l**, or **blabla@c**

## What doesn't it look for?? Here's a non-exhaustive list: 
  
* spelling... anywhere
* &=verbs (neither the &= nor the use of present 3ps tense)
* \[=! verbs] (checks the bracket syntax, but not the use of present 3ps tense)
* xxx vs. yyy
* the use of capital letters
* the use of expressive words with special agreed-upong spellings (e.g., mm-hm)
* extra spaces
* uses of hyphens and ampersands to indicate cut-off/restarted speech (e.g., he- or he&, -in or &in)
* the use of things like [+ CHI], that were in some of the ROS files
* matching speaker names across related tiers
* inner tier structure (i.e., correct hierarchical set-up)
  