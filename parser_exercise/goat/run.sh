echo "with -p"
runghc GoatParser.hs -p asg.gt
echo "\n"

echo "without -p"
runghc GoatParser.hs asg.gt
echo "\n"

echo "error"
runghc GoatParser.hs
echo "\n"
