use crate::ExprEnum;
use crate::parser::Path;

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
#[allow(unused)]
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
		#[cfg(target_arch = "x86_64")]
		{
			return Architecture::X86_64;
		}
		#[cfg(target_arch = "x86")]
		{
			return Architecture::X86;
		}
		#[cfg(target_arch = "aarch64")]
		{
			return Architecture::Aarch64;
		}
		#[cfg(target_arch = "arm")]
		{
			return Architecture::Arm;
		}
		#[cfg(target_arch = "riscv64")]
		{
			return Architecture::RiscV64;
		}
		#[cfg(target_arch = "riscv32")]
		{
			return Architecture::RiscV32;
		}
		#[allow(unreachable_code)]
		return Architecture::Unknown;
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[allow(unused)]
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
		#[cfg(target_os = "linux")]
		{
			return OperatingSystem::Linux;
		}
		#[cfg(target_os = "windows")]
		{
			return OperatingSystem::Windows;
		}
		#[cfg(target_os = "macos")]
		{
			return OperatingSystem::MacOS;
		}
		#[allow(unreachable_code)]
		return OperatingSystem::Unknown;
	}
}

#[allow(unused)]
#[derive(Default, Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Copy, clap::ValueEnum)]
pub enum ColourConf
{
	Always,
	#[default]
	Auto,
	Never,
}

impl std::fmt::Display for ColourConf
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			ColourConf::Always => write!(f, "always"),
			ColourConf::Auto => write!(f, "auto"),
			ColourConf::Never => write!(f, "never"),
		};
	}
}

#[derive(Default, Debug, Eq, PartialEq, Clone)]
pub struct Config
{
	pub optimization: Optimization,
	pub arch: Architecture,
	pub os: OperatingSystem,
	pub colour: ColourConf,
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
