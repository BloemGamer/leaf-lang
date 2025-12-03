#!/usr/bin/env python3
import os
import sys
import shutil
import subprocess
import glob

exe_files = ["s-lang"]
exe_files += glob.glob("test_*")


compilers: list[str] = ["gcc", "clang"]
modes: list[str] = ["Debug", "Release"]

def main(input: list[str]):
	if len(input) == 0:
		return
	cmd = input[0]
	for file_path in exe_files:
		try:
			os.remove(file_path)
		except:
			pass
		os.makedirs("build", exist_ok=True)
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
		case "debug":
			build_file(None, "Debug")
		case "release":
			build_file(None, "Release")
		case "gcc-debug" | "gcc":
			build_file("gcc", "Debug")
		case "gcc-release":
			build_file("gcc", "Release")
		case "clang-debug" | "clang":
			build_file("clang", "Debug")
		case "clang-release":
			build_file("clang", "Release")
		case "setup":
			if len(input) == 1:
				return;
			match input[1]:
				case "debug":
					setup_config(None, "Debug")
				case "release":
					setup_config(None, "Release")
				case "gcc-debug" | "gcc":
					setup_config("gcc", "Debug")
				case "gcc-release":
					setup_config("gcc", "Release")
				case "clang-debug" | "clang":
					setup_config("clang", "Debug")
				case "clang-release":
					setup_config("clang", "Release")
				case _ if "-" in cmd:
					compiler, mode = cmd.split("-")
					mode = mode.capitalize()
					setup_config(compiler, mode)

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

def build_file(compiler: str | None, mode: str):
	if compiler is None:
		build_path = os.path.join("build", "default", mode.lower())
		print(f"\033[1;35mBuilding default compiler {mode}...\033[0m")
	else:
		build_path = os.path.join("build", compiler, mode.lower())
		print(f"\033[1;35mBuilding {compiler} {mode}...\033[0m")

	result = subprocess.run(
			["cmake", "--build", build_path],
			capture_output=False
			)
	if result.returncode != 0:
		compiler_str = "default compiler" if compiler is None else f"compiler = {compiler}"
		raise Exception(f"Could not compile project with {compiler_str} and mode = {mode}\n")

def setup_config(compiler: str | None, mode: str):
	if compiler is None:
		build_path = os.path.join("build", "default", mode.lower())
		print(f"\033[1;35mSetting up default compiler {mode}...\033[0m")
	else:
		build_path = os.path.join("build", compiler, mode.lower())
		print(f"\033[1;35mSetting up {compiler} {mode}...\033[0m")

	cmake_args = [
		"cmake",
		"-B", build_path,
		"-S", ".",
		f"-DCMAKE_BUILD_TYPE={mode}"
	]

	# Only add compiler flag if one is specified
	if compiler is not None:
		cmake_args.append(f"-DCMAKE_C_COMPILER={compiler}")

	result = subprocess.run(cmake_args, capture_output=False)
	if result.returncode != 0:
		compiler_str = "default compiler" if compiler is None else f"compiler = {compiler}"
		raise Exception(f"Could not setup project for {compiler_str} and mode = {mode}\n")

if __name__ == "__main__":
	if len(sys.argv) == 1:
		main(["setup", "release"])
		main(["release"])
	else:
		input = sys.argv
		input.pop(0)
		main(input)
