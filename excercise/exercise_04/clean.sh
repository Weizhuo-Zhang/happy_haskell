fileType=("*.hs" "*.hi" "*.o" "*.out")

for type in ${fileType[@]}
do
    if [ -f $type ]; then
        rm $type
    fi
done
