./debug.sh

rm -rf test_output
mkdir test_output

echo "running ctest"
touch test_output/ctest.log
ctest --verbose --test-dir cmake-build-debug &> test_output/ctest.log
echo "finished with ctest, you can find the logfiles in the test_output dir"


echo "running valgrind memcheck"
valgrind --tool=memcheck -s --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=test_output/valgrind_memcheck.log ./s-lang &> /dev/null
echo "finished with valgrind, you can find the logfiles in the test_output dir"

# TODO
# run a lot of times with different test files
