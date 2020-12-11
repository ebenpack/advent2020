#! /bin/sh
touch "input/Day$1.txt"
cp test/Days/Day01Spec.hs "test/Days/Day$1Spec.hs"
sed -i.bak -e "s/Day01/Day$1/g" "test/Days/Day$1Spec.hs"
rm "test/Days/Day$1Spec.hs.bak"