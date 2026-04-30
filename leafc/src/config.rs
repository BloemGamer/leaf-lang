use crate::parser::Path;
use crate::ExprEnum;

#[allow(unused)]
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub enum Optimization
{
	#[default]
	Debug,
	Release,
}

impl std::fmt::Display for Optimization
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Optimization::Debug => write!(f, "debug"),
			Optimization::Release => write!(f, "release"),
		};
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Architecture
{
	/// 64-bit -> intel windows/linux
	X86_64,
	/// 32-bit -> intel windows/linux
	X86,
	/// 64-bit -> Arm mac
	Aarch64,
	/// 32-bit -> Arm mac/embedded
	Arm,
	/// 64-bit -> Risc-V
	RiscV64,
	/// 32-bit -> Risc-V
	RiscV32,
	/// Default -> need to add more maybe
	Unknown,
}

impl std::fmt::Display for Architecture
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Architecture::X86_64 => write!(f, "x86_64"),
			Architecture::X86 => write!(f, "x86"),
			Architecture::Aarch64 => write!(f, "aarch64"),
			Architecture::Arm => write!(f, "arm"),
			Architecture::RiscV64 => write!(f, "riscv64"),
			Architecture::RiscV32 => write!(f, "riscv32"),
			Architecture::Unknown => write!(f, "unknown"),
		};
	}
}

impl Default for Architecture
{
	fn default() -> Self
	{
		return if cfg!(target_arch = "x86_64") {
			Architecture::X86_64
		} else if cfg!(target_arch = "x86") {
			Architecture::X86
		} else if cfg!(target_arch = "aarch64") {
			Architecture::Aarch64
		} else if cfg!(target_arch = "arm") {
			Architecture::Arm
		} else if cfg!(target_arch = "riscv64") {
			Architecture::RiscV64
		} else if cfg!(target_arch = "riscv32") {
			Architecture::RiscV32
		} else {
			Architecture::Unknown
		};
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum OperatingSystem
{
	Linux,
	Windows,
	MacOS,
	Unknown,
}

impl std::fmt::Display for OperatingSystem
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			OperatingSystem::Linux => write!(f, "linux"),
			OperatingSystem::Windows => write!(f, "windows"),
			OperatingSystem::MacOS => write!(f, "macos"),
			OperatingSystem::Unknown => write!(f, "unknown"),
		};
	}
}

impl Default for OperatingSystem
{
	fn default() -> Self
	{
		return if cfg!(target_os = "linux") {
			OperatingSystem::Linux
		} else if cfg!(target_os = "windows") {
			OperatingSystem::Windows
		} else if cfg!(target_os = "macos") {
			OperatingSystem::MacOS
		} else {
			OperatingSystem::Unknown
		};
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Config
{
	pub optimization: Optimization,
	pub arch: Architecture,
	pub os: OperatingSystem,
}

impl Default for Config
{
	fn default() -> Self
	{
		return Self {
			optimization: Optimization::default(),
			os: OperatingSystem::default(),
			arch: Architecture::default(),
		};
	}
}

impl Config
{
	pub fn lookup(&self, path: &Path) -> Result<ExprEnum, String>
	{
		if path.segments.len() != 1 {
			return Err(format!("`{path}` is not a valid `cfg` path"));
		}
		return Ok(match path.segments[0].name.as_str() {
			"optimization" => ExprEnum::String(self.optimization.to_string()),
			"os" => ExprEnum::String(self.os.to_string()),
			"arch" => ExprEnum::String(self.arch.to_string()),
			_ => return Err(format!("`{path}` is not a valid `cfg` path")),
		});
	}
}
