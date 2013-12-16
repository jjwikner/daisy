#!/bin/bash

# JJ Wikner

# Script to generate a combined ics file for a 
# group of people at linkoping university. In 
# addition to the ics an html page is generated 
# for the intranet.

# flags could be added to support other groups or other systems.

echo "daisy --> " $0

# The main university timeedit database is used for
# reference:

pathToCalendar="http://timeedit.liu.se/4DACTION/iCal_downloadReservations/timeedit.ics?branch=5&lang=1"

# As input the user has to specify the year.
year=`date +"%g"`

echo $year 

# We can search for the whole year, since time edit will only display the active weeks.
startWeek=${year}01
stopWeek=${year}53

# Modify web page search string with week numbers
pathToCalendar="${pathToCalendar}&from=${startWeek}&to=${stopWeek}"

# Print to verify
echo $pathToCalendar 

# This can be anything, in our case the electronics systems group
tag=es_teaching


# Remove the two old files:
# rm ${HOME}/info/$tag.html
# rm ${HOME}/info/$tag.ics

mkdir -p /tmp/${USER}/daisyCreateGroupIcs/

# Create an html for the home page
echo "<HTML><HEAD></HEAD><BODY><UL>" > /tmp/${USER}/daisyCreateGroupIcs/$tag.html

# Parse the file containing staff

while read liuId email name calId
do
    echo "${liuId} ${email} ${name} ${calId}"
    pathToCalendarX="${pathToCalendar}&id1=${calId}"
    echo "<LI><A HREF = \"$pathToCalendarX \"> ${name}</A> " >> /tmp/${USER}/daisyCreateGroupIcs/$tag.html
    wget $pathToCalendarX -O "/tmp/${USER}/daisyCreateGroupIcs/${email}_${startWeek}_${stopWeek}.ics" 
    cat /tmp/${USER}/daisyCreateGroupIcs/${email}_${startWeek}_${stopWeek}.ics >> /tmp/${USER}/daisyCreateGroupIcs/$tag.ics 
    # rm /tmp/${USER}/daisyCreateGroupIcs/${email}_${startWeek}_${stopWeek}.ics

done < ${DAISYAREA}/info/$tag.txt

echo "</UL></BODY></HTML>" >> /tmp/${USER}/daisyCreateGroupIcs//$tag.html

# End of file.

echo "daisy <-- " $0
