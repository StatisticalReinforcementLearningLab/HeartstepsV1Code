#!/bin/bash
# path names assumed local, so run this script from its directory
# FIXME: can we better preserve the M+Box version history?

export PATH=$PATH:/path/to/google_appengine/
export GAE_SDK_ROOT=/Data/google_appengine

dir=~/mbox/HeartSteps/Data

for table in EMA_Completed EMA_Context_Engaged EMA_Context_Notified \
    EMA_Response Heartsteps_Usage_History Momentary_Decision \
    Motivational_Message Response Snoozed_FromInApp \
    Structured_Planning_Response Unstructured_Planning_Response \
    User_Addresses User_Calendars User_Data User_Decision_Times \
    User_Last_Updated Valid_Jawbone_Email_Addresses \
    Valid_User_Email_Addresses Weather_History
do
echo "EXPORTING $table..."
if [ -f $dir/$table.csv ]
then
    if  [ -f $dir/oldfiles/$table.csv ]
    then
        rm $dir/$table.csv
    else
        mv $dir/$table.csv $dir/oldfiles/$table.csv
    fi
fi
./appcfg.py download_data \
    --url=https://com-um-heartsteps.appspot.com/remote_api \
    --filename=$dir/$table.csv --kind=$table \
    --config_file=config_files/$table.yaml
done

rm bulkloader*
