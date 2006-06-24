#
#!/bin/sh

all_ok=1;
compiler="../parser-test"
interpreter="false"
mask='*.a60'

echo "---------- testing uncompilable ----------"

for file in fail/$mask; do
	if [ -e $file ]; then
		echo $file;
		result="1";
		sfile=`echo $file | sed 's/\.[^/.]*$//'`".out"
		$compiler $file >/dev/null 2>/dev/null && result="0"

		if [ "$result" != "1" ]; then
			echo -e "\tunexpected: successfully compiled";
			all_ok=0;
		fi;

		rm $sfile
	fi;
done;

echo "---------- testing compilable ----------"

for file in compile/$mask; do
	if [ -e $file ]; then
		echo $file;
		result="1";
		$compiler $file >/dev/null 2>tmp || result="0"
		if [ "$result" != "1" ]; then
			echo -e "\tunexpected: compilation failed";
			cat tmp | sed 's/^/\t/'
			all_ok=0;
		fi;
		rm tmp
	fi;
done;

echo "---------- testing runnable ----------"

for file in run/$mask; do
	if [ -e $file ]; then
		result="1";
		sfile=`echo $file | sed 's/\.[^/.]*$//'`".out"
		echo "$file";
		$compiler $file 2>tmp || result="0"
		if [ "$result" != "1" ]; then
			echo -e "\tunexpected: compilation failed";
			cat tmp | sed 's/^/\t/'
			all_ok=0;
		else
			$interpreter $sfile >tmp 2>/dev/null && result="1" || result="0"
			if [ "$result" != "1" ]; then
				echo -e "\tunexpected: interpreter failed";
				all_ok=0;
			else
				line=0;
				cat tmp | while read a; do
					line=$((line+1))
					if [ "$a" != "1" ]; then
						echo -e "\tunexpected result #$line: '$a'";
						all_ok=0;
					fi
				done;
			fi;
		fi;
		rm $sfile
		rm tmp
	fi;
done;

echo "---------- summary ----------"

if [ $all_ok -eq 1 ]; then
	echo "all test passed successfully! :)";
else
	echo "something failed... :(";
	exit 1;
fi;
