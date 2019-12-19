# AAS-minCHAT-Checker
Allows annotators to automatically check for basic minCHAT errors in their transcriptions so that they can manually fix those errors and submit them.

**This script won't catch all the errors!!**

* It only catches errors as described below
* It might even catch some "errors" that are _in reality_ perfectly fine

It is up to you humans to fix those as needed! This is just a tool to help annotators do check the basic ACLEW Annotation Scheme and minCHAT standards used in their annotation files.

Note that to properly check each file you'll have to both (a) open the file in ELAN and (b) export a text version of it for use with the minCHAT checker tool.

## Instructions

###  Start with a quick ELAN manual check
1. Locate the .eaf file you want to check; to ensure that you are using the most recent version of your files, clone (recommended) or download the directory of .eafs you want to analyze from our shared ACLEW github repo (e.g. rely\_XXX or raw\_XXX).

2. Open the file in ELAN

3. Check carefully for the following possible issues in your .eaf file:
    - **Tiers that shouldn't belong:** The file might have extra tiers or tiers with illegal names (a common extra tier is `default`). The only tiers allowed should be speaker and dependent tiers (e.g., `FA1`, `xds@FA1`, `CHI`, `vcm@CHI`, `UC2`, `xds@MA3`, etc.), and the non-speaker tiers used across ACLEW corpora: `context`, `code_num`, `code`, `on_off`, and `notes`. Any tier that has has a name different from these _exact_ options should be deleted or fixed to fit these name options.
    - **Missing/inaccurate participant names:** Go to `Tier > Change Tier Attributes...` to view a table of your tiers. Check that each tier associated with a speaker (e.g., `FA1` and `xds@FA1`) is associated with the correct participant name (e.g., `FA1`). If not, fix it. See Figure 1 for details.

_Figure 1._ How to check participant assignment and fix any errors.

<p align="center"> 
<img src="ChangePtcpInfo-screenshot.png" alt="ChangePtcpInfo-screenshot" width="800"/>
</p>

###  Export to tab-delimited text

4. Now that you have checked for these basic errors in ELAN, you should export the .eaf file as a tab-delimited text file (`File > Export As > Tab-delimited text`. When you do this, make sure that *all tiers* are selected and the right *time format* (ms) is included); see Figure 2 below.
    * ELAN will likely ask you to find the media (e.g., .wav) file. Select `Cancel` and proceed unless you want to listen to the media while checking for formatting errors. You can also add the media file back later by going to  `Edit > Linked Files...` and adding it there.
    * When you click to export the .txt file it will ask you about encoding: the default setting of `UTF-8` is the correct one for our tool.

_Figure 2._ Select all tiers and make sure your time column settings are set up as shown below. When prompted about format type for export, select UTF-8.

<p align="center"> 
<img src="EAF2TXT-screenshot.png" alt="EAF2TXT-screenshot" width="300"/>
</p>

### Run automated error detection

5. Go to [https://aclew.shinyapps.io/AAS-minCHAT-Checker/]()

6. Upload the file and click `Submit`.

7. The tool gives the number of possible errors it detected and a list of the capitalized and hyphenated words in the transcription (make sure these match the minCHAT rules!). You can download a more detailed spreadsheet of possible errors at the bottom of the report; remember, this tool finds potential errors—it is your job to determine whether there are real errors!

8. Problems? See Error Reporting below.


## What does the minCHAT checker look for?

It checks to see whether...

* the tier name is either 3 or 7 characters
* the tier name pre- and post-fixes match one of the limited types (e.g., "vcm", "CHI", "FA1")
* there are missing annotations (e.g., a missing LEX annotation when VCM = C)
* there are too many or two few annotations
* the closed-vocabulary annotation values (e.g., XDS, VCM) are valid
* there are empty transcriptions
* transcriptions have too few or too many terminal markers
* the use of square brackets follows one of the following patterns: **\<blabla\> [: blabla]**, **\<blabla\> [=! blabla]**, or **[- lng]**
* the use of @ follows one of the following patterns: **blabla@s:eng**, **blabla@l**, or **blabla@c**

## What doesn't the checker look for?

Here's a non-exhaustive list: 
  
* spelling... anywhere
* &=verbs (neither the &= nor the use of present 3ps tense)
* \[=! verbs] (checks the bracket syntax, but not the use of present 3ps tense)
* xxx vs. yyy
* the proper use of capital letters of hyphenated words; but it does return a list of these for manual review
* extra spaces
* the proper use of hyphens and ampersands to indicate cut-off/restarted speech (e.g., he- or he&, -in or &in)
* matching speaker names across related tiers
* inner tier structure (i.e., correct hierarchical set-up; requires XML)
* the use of things like [+ CHI], that were in some of the ROS files **NOTE THAT THESE WILL SHOW UP AS ERRORS; ROS can choose to ignore them for now**
  
## Error reporting

Please report any problems you encounter. You can file them under the issues tab of this tool's github repository ([https://github.com/aclew/AAS-minCHAT-Checker/issues]()). When filing an issue, please provide a link to the _exact_ input files you were using as well as a detailed explanation of the problem you encountered. Otherwise we might not be able to re-create the problem and fix it!
