./debug.sh

rm -rf test_output
mkdir test_output

touch test_output/ctest.log
ctest --verbose --test-dir cmake-build-debug &> test_output/ctest.log


echo "running valgrind memcheck"
valgrind --tool=memcheck -s --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=test_output/valgrind_memcheck.log ./s-lang &> /dev/null
echo "finished with valgrind, you can find the logfiles in the valgrind dir"

# TODO
# run a lot of times with different test files
