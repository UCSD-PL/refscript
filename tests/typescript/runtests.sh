#!/bin/bash

ret_code=0
for f in $(ls $1*.ts)
do
	echo $f
	nanojs liquid $f
	ret_code=$(($ret_code+$?))
	echo $ret_code
done

rm $1*js*
rm $1*annot*
