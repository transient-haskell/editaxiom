#!/bin/bash
while true
do
./dbshow.sh > tmp
sleep  5
./dbshow.sh > tmp2
diff tmp tmp2
done