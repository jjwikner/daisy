#!/bin/csh

pdftotext -layout -eol unix $1 /tmp/$1.txt

cat /tmp/$1.txt | tr ' ' '\012' | tr '/' '\012' | tr --delete '=;:`"<>,./?#$%^&(){}[]0123456789' | tr '[:upper:]' '[:lower:]' > /tmp/$1.txt_mod



daisyWordCount.pl < /tmp/$1.txt_mod

