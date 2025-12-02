#!/usr/bin/env python3
import sys
import os
import build
import subprocess


def main():
	build.clean_all()
	results = []

	for c in build.compilers:
		for m in build.modes:
			build.setup_config(c, m)
			build.build_file(c, m)
			result = test(c, m)
			results.append((c, m, result))

	print_results(results)


def test(compiler: str, mode: str) -> dict:
	mode = mode.lower()
	result = {"compiler": compiler, "mode": mode, "ctest": False, "valgrind": False}

	try:
		test_ctest(compiler, mode)
		result["ctest"] = True
	except subprocess.CalledProcessError:
		result["ctest"] = False
		return result

	try:
		test_valgrind_all(compiler, mode)
		result["valgrind"] = True
	except Exception:
		result["valgrind"] = False

	return result


def test_ctest(compiler: str, mode: str):
	result = subprocess.run(
			["ctest", "--verbose", "--test-dir", f"build/{compiler}/{mode}"],
			capture_output=True,
			text=True
			)

	os.makedirs(f"test_output/{compiler}/{mode}", exist_ok=True)
	with open(f"test_output/{compiler}/{mode}/ctest.log", "w") as file:
		file.write(result.stderr)
		file.write(result.stdout)

	result.check_returncode()


def test_valgrind_all(compiler: str, mode: str):
	leak = False
	modules = ["s-lang", "hash_test"]
	leak_modules = []

	for mod in modules:
		result = test_valgrind(compiler, mode, mod)
		if result.returncode != 0:
			leak = True
			leak_modules.append(mod)

	if leak:
		raise Exception(
				f"Memory leak at compiler = {compiler}, mode = {mode}, modules: {leak_modules}"
				)


def test_valgrind(compiler: str, mode: str, module: str) -> subprocess.CompletedProcess:
	if sys.platform == "win32":
		module = f"{module}.exe"

	os.makedirs(f"test_output/{compiler}/{mode}/valgrind", exist_ok=True)
	result = subprocess.run(
			[
				"valgrind",
				"--tool=memcheck",
				"-s",
				"--leak-check=full",
				"--show-leak-kinds=all",
				"--track-origins=yes",
				f"--log-file=test_output/{compiler}/{mode}/valgrind/{module}.log",
				f"./{module}"
				],
			capture_output=True
			)

	return result


def print_results(results: list):
	print("\n" + "=" * 70)
	print("TEST RESULTS SUMMARY")
	print("=" * 70)

	all_passed = True
	for compiler, mode, result in results:
		status = "✓ PASSED" if result["ctest"] and result["valgrind"] else "✗ FAILED"
		if not (result["ctest"] and result["valgrind"]):
			all_passed = False

		print(f"\n{compiler} - {mode}: {status}")
		print(f"  CTest:    {'✓ Passed' if result['ctest'] else '✗ Failed'}")
		print(f"  Valgrind: {'✓ Passed' if result['valgrind'] else '✗ Failed'}")

	print("\n" + "=" * 70)
	if all_passed:
		print("ALL TESTS PASSED ✓")
	else:
		print("SOME TESTS FAILED ✗")
	print("=" * 70 + "\n")


if __name__ == "__main__":
	main()
