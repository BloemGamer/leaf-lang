cmake --build build

rm -rf valgrind
mkdir valgrind

echo "running valgrind memcheck"
valgrind --tool=memcheck -s --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=valgrind/valgrind_memcheck.log ./s-lang &> /dev/null
echo "finished with valgrind, you can find the logfiles in the valgrind dir"

# TODO
# run a lot of times with different test files
