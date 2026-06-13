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
	/// 64-bit Intel (Windows/Linux/macOS-Intel)
	X86_64,
	/// 32-bit Intel
	X86,
	/// 64-bit ARM (Apple Silicon, modern Linux ARM servers)
	Aarch64,
	/// 32-bit ARM (sub-arch like `v7`, `v7s`, `thumbv7m` lives on `Target::sub_arch`)
	Arm,
	/// 64-bit RISC-V
	RiscV64,
	/// 32-bit RISC-V
	RiscV32,
	/// 32-bit WebAssembly
	Wasm32,
	/// Unknown / not specified
	Unknown,
}

impl Architecture
{
	/// Parse the architecture field of an LLVM triple. Accepts the common
	/// aliases LLVM itself accepts (e.g. `amd64` for `x86_64`, `arm64` for
	/// `aarch64`, `i686`/`i386` for `x86`). Returns `None` if unrecognised.
	pub fn parse(s: &str) -> Option<Self>
	{
		return Some(match s {
			"x86_64" | "amd64" => Architecture::X86_64,
			"x86" | "i386" | "i486" | "i586" | "i686" => Architecture::X86,
			"aarch64" | "arm64" => Architecture::Aarch64,
			"arm" | "armv6" | "armv7" | "armv7s" | "thumbv6m" | "thumbv7m" | "thumbv7em" => Architecture::Arm,
			"riscv64" => Architecture::RiscV64,
			"riscv32" => Architecture::RiscV32,
			"wasm32" => Architecture::Wasm32,
			"unknown" => Architecture::Unknown,
			_ => return None,
		});
	}

	/// If `s` is one of the 32-bit ARM variants, return the sub-arch tag
	/// (`v7`, `thumbv7m`, ...) so the caller can attach it to `Target::sub_arch`.
	pub fn extract_sub_arch(s: &str) -> Option<String>
	{
		return match s {
			"armv6" | "armv7" | "armv7s" | "thumbv6m" | "thumbv7m" | "thumbv7em" => Some(s.to_string()),
			_ => None,
		};
	}
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
			Architecture::Wasm32 => write!(f, "wasm32"),
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
		#[cfg(target_arch = "wasm32")]
		{
			return Architecture::Wasm32;
		}
		#[allow(unreachable_code)]
		return Architecture::Unknown;
	}
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
#[allow(unused)]
pub enum Vendor
{
	#[default]
	Unknown,
	/// Conventional vendor for Windows triples (`x86_64-pc-windows-msvc`).
	Pc,
	/// Apple platforms (macOS, iOS).
	Apple,
}

impl Vendor
{
	pub fn parse(s: &str) -> Option<Self>
	{
		return Some(match s {
			"unknown" => Vendor::Unknown,
			"pc" => Vendor::Pc,
			"apple" => Vendor::Apple,
			_ => return None,
		});
	}
}

impl std::fmt::Display for Vendor
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Vendor::Unknown => write!(f, "unknown"),
			Vendor::Pc => write!(f, "pc"),
			Vendor::Apple => write!(f, "apple"),
		};
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

impl OperatingSystem
{
	/// Parse the OS field of an LLVM triple. Accepts `darwin` and `macosx`
	/// as aliases for macOS.
	pub fn parse(s: &str) -> Option<Self>
	{
		return Some(match s {
			"linux" => OperatingSystem::Linux,
			"windows" => OperatingSystem::Windows,
			"macos" | "darwin" | "macosx" => OperatingSystem::MacOS,
			"unknown" | "none" => OperatingSystem::Unknown,
			_ => return None,
		});
	}

	/// How this OS is spelled inside an LLVM triple. Differs from `Display`
	/// for macOS, where the triple form is `darwin`.
	pub const fn triple_str(&self) -> &'static str
	{
		return match self {
			OperatingSystem::Linux => "linux",
			OperatingSystem::Windows => "windows",
			OperatingSystem::MacOS => "darwin",
			OperatingSystem::Unknown => "unknown",
		};
	}
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

/// The ABI/environment slot of an LLVM triple. This is the field that
/// distinguishes e.g. `windows-msvc` from `windows-gnu` (mingw), or
/// `linux-gnu` from `linux-musl`, or `gnueabi` from `gnueabihf` on 32-bit ARM.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
#[allow(unused)]
pub enum Environment
{
	/// No environment field (typical for `apple-darwin`, bare-metal, wasm).
	#[default]
	None,
	/// glibc-based Linux, or mingw on Windows.
	Gnu,
	/// 32-bit ARM glibc, soft-float ABI.
	Gnueabi,
	/// 32-bit ARM glibc, hard-float ABI.
	Gnueabihf,
	/// musl libc.
	Musl,
	/// Microsoft Visual C++ ABI.
	Msvc,
}

impl Environment
{
	pub fn parse(s: &str) -> Option<Self>
	{
		return Some(match s {
			"" | "none" => Environment::None,
			"gnu" => Environment::Gnu,
			"gnueabi" => Environment::Gnueabi,
			"gnueabihf" => Environment::Gnueabihf,
			"musl" => Environment::Musl,
			"msvc" => Environment::Msvc,
			_ => return None,
		});
	}
}

impl std::fmt::Display for Environment
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Environment::None => write!(f, "none"),
			Environment::Gnu => write!(f, "gnu"),
			Environment::Gnueabi => write!(f, "gnueabi"),
			Environment::Gnueabihf => write!(f, "gnueabihf"),
			Environment::Musl => write!(f, "musl"),
			Environment::Msvc => write!(f, "msvc"),
		};
	}
}

/// A full LLVM-style compilation target: `<arch><sub>-<vendor>-<os>-<env>`.
///
/// Use [`Target::default_for_host`] to get a triple matching the build host,
/// [`Target::parse`] to consume `--target=...` style strings, and
/// [`Target::to_llvm_triple`] when handing the triple to a codegen backend.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Target
{
	pub arch: Architecture,
	/// Sub-architecture tag (e.g. `"v7"`, `"thumbv7m"`). Only meaningful for
	/// `Architecture::Arm`. Stored without the leading arch name.
	pub sub_arch: Option<String>,
	pub vendor: Vendor,
	pub os: OperatingSystem,
	pub env: Environment,
}

impl Target
{
	/// Build a `Target` describing the current host.
	pub fn default_for_host() -> Self
	{
		let arch = Architecture::default();
		let os = OperatingSystem::default();
		let (vendor, env) = default_vendor_env(&os);
		return Self {
			arch,
			sub_arch: None,
			vendor,
			os,
			env,
		};
	}

	/// Parse an LLVM-style triple. Permissive: missing fields default to
	/// `Unknown` / `None`, and field order is the canonical
	/// `arch-vendor-os-env`. Returns `Err` if the architecture field can't
	/// be recognised, since without it the rest is meaningless.
	pub fn parse(s: &str) -> Result<Self, String>
	{
		let parts: Vec<&str> = s.split('-').collect();
		if parts.is_empty() || parts[0].is_empty() {
			return Err(format!("empty target triple: `{s}`"));
		}

		let arch_str = parts[0];
		let arch = Architecture::parse(arch_str)
			.ok_or_else(|| return format!("unknown architecture `{arch_str}` in target `{s}`"))?;
		let sub_arch = Architecture::extract_sub_arch(arch_str)
			.and_then(|full| return full.strip_prefix(&arch.to_string()).map(str::to_string))
			.or_else(|| return Architecture::extract_sub_arch(arch_str));

		let mut vendor = Vendor::Unknown;
		let mut os = OperatingSystem::Unknown;
		let mut env = Environment::None;

		// Walk remaining fields and slot each into the first matching category.
		// LLVM's own parser is similarly forgiving about missing/reordered fields.
		let mut vendor_set = false;
		let mut os_set = false;
		let mut env_set = false;
		for tok in &parts[1..] {
			if !vendor_set && let Some(v) = Vendor::parse(tok) {
				vendor = v;
				vendor_set = true;
				continue;
			}
			if !os_set && let Some(o) = OperatingSystem::parse(tok) {
				os = o;
				os_set = true;
				continue;
			}
			if !env_set && let Some(e) = Environment::parse(tok) {
				env = e;
				env_set = true;
				#[allow(clippy::needless_continue)]
				continue;
			}
			// Unknown tokens are tolerated, matching LLVM's behaviour.
		}

		return Ok(Self {
			arch,
			sub_arch,
			vendor,
			os,
			env,
		});
	}

	/// Format as an LLVM target triple. Uses [`OperatingSystem::triple_str`]
	/// so macOS comes out as `darwin`, and omits the environment field when
	/// it's `None` *and* the OS is Apple (Apple triples conventionally have
	/// only three fields).
	pub fn to_llvm_triple(&self) -> String
	{
		let arch = match &self.sub_arch {
			Some(sub) if self.arch == Architecture::Arm => format!("{}{}", self.arch, sub),
			_ => self.arch.to_string(),
		};

		let omit_env = self.env == Environment::None && self.vendor == Vendor::Apple;
		if omit_env {
			return format!("{}-{}-{}", arch, self.vendor, self.os.triple_str());
		}
		return format!("{}-{}-{}-{}", arch, self.vendor, self.os.triple_str(), self.env);
	}
}

impl Default for Target
{
	fn default() -> Self
	{
		return Self::default_for_host();
	}
}

impl std::fmt::Display for Target
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return write!(f, "{}", self.to_llvm_triple());
	}
}

/// Pick sensible vendor + environment defaults for a given OS, matching what
/// a user typing just `--os=windows` would expect.
const fn default_vendor_env(os: &OperatingSystem) -> (Vendor, Environment)
{
	return match os {
		OperatingSystem::Linux => (Vendor::Unknown, Environment::Gnu),
		OperatingSystem::Windows => (Vendor::Pc, Environment::Msvc),
		OperatingSystem::MacOS => (Vendor::Apple, Environment::None),
		OperatingSystem::Unknown => (Vendor::Unknown, Environment::None),
	};
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
	pub target: Target,
	pub colour: ColourConf,
}

impl Config
{
	/// Convenience accessor: the target architecture.
	pub const fn arch(&self) -> &Architecture
	{
		return &self.target.arch;
	}

	/// Convenience accessor: the target operating system.
	pub const fn os(&self) -> &OperatingSystem
	{
		return &self.target.os;
	}

	pub fn lookup(&self, path: &Path) -> Result<ExprEnum, String>
	{
		if path.segments.len() != 1 {
			return Err(format!("`{path}` is not a valid `cfg` path"));
		}
		return Ok(match path.segments[0].name.as_str() {
			"optimization" => ExprEnum::String(self.optimization.to_string()),
			"os" => ExprEnum::String(self.target.os.to_string()),
			"arch" => ExprEnum::String(self.target.arch.to_string()),
			"vendor" => ExprEnum::String(self.target.vendor.to_string()),
			"env" => ExprEnum::String(self.target.env.to_string()),
			"target" => ExprEnum::String(self.target.to_llvm_triple()),
			_ => return Err(format!("`{path}` is not a valid `cfg` path")),
		});
	}
}
