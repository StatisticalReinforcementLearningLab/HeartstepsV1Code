@ECHO OFF
REM Path names are assumed local, so run this script from its directory
REM NB: File overwrites via appcfg.py throw an error with no option to avoid, via system commands are prone to errors with WebDAV

SET dir=Z:\HeartSteps\Data

ECHO You will be asked to authenticate with Google App Engine via the browser. ^
You will need to copy and paste a link provided in the console. To do this, ^
right-click anywhere in the command prompt, select "Mark", and highlight ^
the URL. Then paste this into the browser, and log in with Google.

ECHO Please wait while we search for and delete any existing access tokens.
DEL C:\Users\*.appcfg_oauth2_tokens* /s

ECHO Beginning export process...
FOR %%t IN (
            Motivational_Message Response Snoozed_FromInApp ^
            Structured_Planning_Response Unstructured_Planning_Response ^
            User_Addresses User_Calendars User_Data User_Decision_Times ^
            User_Last_Updated Valid_Jawbone_Email_Addresses ^
            Valid_User_Email_Addresses Weather_History) DO (
        ECHO Exporting %%t ...
        IF EXIST %dir%\%%t.csv (
          DEL %dir%\%%t.csv
        )
        py -2 appcfg.py download_data ^
        --url=https://com-um-heartsteps.appspot.com/remote_api ^
        --filename=%dir%\%%t.csv --kind=%%t ^
        --config_file=config_files/%%t.yaml
    )
ECHO CLEANING UP...
DEL bulkloader*
ECHO ALL DONE!
PAUSE
EXIT
