#!/usr/bin/env python3
import os
import sys
import shutil
import subprocess

compilers: list[str] = ["gcc", "clang"]
modes: list[str] = ["Debug", "Release"]

def main():
	# Default argument if none provided
	if len(sys.argv) == 1:
		sys.argv.append("gcc-debug")

	cmd = sys.argv[1]

	match cmd:
		case "clean":
			shutil.rmtree("build", ignore_errors=True)
			os.makedirs("build", exist_ok=True)
			setup_all()

		case "all":
			shutil.rmtree("build", ignore_errors=True)
			os.makedirs("build", exist_ok=True)
			setup_all()
			build_all()

		case "debug" | "gcc-debug" | "gcc":
			build_file("gcc", "Debug")
		case "release" | "gcc-release":
			build_file("gcc", "Release")
		case "clang-debug" | "clang":
			build_file("clang", "Debug")
		case "clang-release":
			build_file("clang", "Release")

			# Dynamic compiler-mode parsing for compilers like MSVC or modes like RelWithDebInfo
		case _ if "-" in cmd:
			compiler, mode = cmd.split("-")
			mode = mode.capitalize()
			build_file(compiler, mode)

		case _:
			print(f"\033[1;31mUnknown command: {cmd}\033[0m")
			sys.exit(1)


def setup_all():
	for c in compilers:
		for m in modes:
			setup_config(c, m)


def build_all():
	for c in compilers:
		for m in modes:
			build_file(c, m)

def clean_all():
	shutil.rmtree("build", ignore_errors=True)
	os.makedirs("build", exist_ok=True)
	setup_all()



def build_file(compiler: str, mode: str):
	build_path = os.path.join("build", compiler, mode.lower())
	print(f"\033[1;35mBuilding {compiler} {mode}...\033[0m")
	result = subprocess.run(
			["cmake", "--build", build_path, "--parallel"],
			capture_output=False
			)
	if result.returncode != 0:
		raise Exception(f"Could not compile project with compiler = {compiler} and mode = {mode}\n")


def setup_config(compiler: str, mode: str):
	build_path = os.path.join("build", compiler, mode.lower())
	print(f"\033[1;35mSetting up {compiler} {mode}...\033[0m")
	result = subprocess.run(
			[
				"cmake",
				"-B", build_path,
				"-S", ".",
				f"-DCMAKE_BUILD_TYPE={mode}",
				f"-DCMAKE_C_COMPILER={compiler}"
				],
			capture_output=False
			)
	if result.returncode != 0:
		raise Exception(f"Could not setup project for compiler = {compiler} and mode = {mode}\n")


if __name__ == "__main__":
	main()
