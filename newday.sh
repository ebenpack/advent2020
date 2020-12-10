#! /bin/sh
touch "input/Day0$1.txt"
cp test/Days/Day01Spec.hs "test/Days/Day0$1Spec.hs"
sed -i.bak -e "s/Day01/Day0$1/g" "test/Days/Day0$1Spec.hs"
rm "test/Days/Day0$1Spec.hs.bak"