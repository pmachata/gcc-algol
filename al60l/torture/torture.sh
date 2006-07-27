#!/bin/bash

compiler="../parser-test"
dumper="$compiler -d"
interpreter="false"
mask='*.a60'

rm -f core core.*

errors=0
total=0

error ()
{
    echo -e "\tunexpected: $1"
    errors=$((errors+1))
}

echo "---------- testing uncompilable ----------"

for file in fail/$mask; do
	if [ -e $file ]; then
	        total=$((total+1))
		echo $file;
		$compiler $file &>/dev/null
		result=$?

		if   [ $result -gt "128" ]; then
		        error "killed by a signal #$((result-128))"
		elif [ $result = "0" ]; then
			error "successfully compiled"
		fi
	fi;
done;

echo "---------- testing compilable ----------"

for file in compile/$mask; do
	if [ -e $file ]; then
	        total=$((total+1))
		echo $file;
		$compiler $file >/dev/null 2>tmp
		if [ $? != "0" ]; then
			error "compilation failed"
			cat tmp | sed 's/^/\t/'
		fi
		rm tmp
	fi;
done;

echo "---------- testing parsing ----------"

for file in parse/$mask; do
	if [ -e $file ]; then
	        total=$((total+1))
	    	# first char of file is space -> we need to do
	    	# preprocessing
		sfile=$file
		tfile=$file
	    	head -n 1 < $file | grep -q '^ '
		if [ $? = "0" ]; then
		    echo "$file (pp)";
		    sed -n '/^[C ]/{s/^.//; p}' < $file > sfile
		    sed -n '/^[D ]/{s/^.//; p}' < $file > tfile
		    sed -n '/^@/{s/^./\tnote: /; p}' < $file
		    sfile="sfile"
		    tfile="tfile"
		else
		    echo "$file";
		fi

		$dumper $sfile >dump 2>tmp
		if [ $? != "0" ]; then
			error "compilation failed"
			cat tmp | sed 's/^/\t/'
		else
			diff -u $tfile dump &> tmp
			if [ $? != "0" ]; then
				error "non-cannonic dump"
				cat tmp | sed 's/^/\t/'
			fi
		fi
		rm -f sfile tfile tmp dump
	fi;
done;

echo "---------- testing runnable ----------"

for file in run/$mask; do
	if [ -e $file ]; then
	        total=$((total+1))
		result="1";
		sfile=`echo $file | sed 's/\.[^/.]*$//'`".out"
		echo "$file";
		$compiler $file 2>tmp || result="0"
		if [ "$result" != "1" ]; then
			error "compilation failed"
			cat tmp | sed 's/^/\t/'
		else
			$interpreter $sfile >tmp 2>/dev/null && result="1" || result="0"
			if [ "$result" != "1" ]; then
				error "interpreter failed"
			else
				line=0;
				cat tmp | while read a; do
					line=$((line+1))
					if [ "$a" != "1" ]; then
						error "result #$line: '$a'";
					fi
				done;
			fi;
		fi;
		rm $sfile
		rm tmp
	fi;
done;

echo "---------- summary ----------"

echo "total tests:  $total"
if [ $errors -eq 0 ]; then
	echo "all test passed successfully! :)";
else
	echo "failed tests: $errors"
	exit 1;
fi;
