#!/bin/sh

# A short script to list and potentially e-mail the group members.

set ETAG="@student.liu.se"

echo `ypcat group | grep $GROUP | sed 's/,/@student.liu.se,/g' | sed 's/ //g' | sed 's/^.*://g'`@student.liu.se
