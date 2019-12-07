#!/bin/bash
IFS=$'\n' 
for i in `ls  .tcachedata/*`; do  echo "" && echo  $i  && cat "$i" && echo ""  ; done
