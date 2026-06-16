pub mod c;

use std::path::PathBuf;

use crate::{
	config::{Architecture, Config, Environment, OperatingSystem, Optimization, Target, Vendor},
	diagnostics::DiagnosticBuilder,
	monomorphization::MonoModule,
	source_map::SourceMap,
	symbol_collection::GlobalSymbolTable,
};

/// What kind of artifact the backend should emit.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(unused)]
pub enum OutputKind
{
	/// A standalone executable.
	Executable,
	/// A relocatable object file (`.o` / `.obj`).
	Object,
	/// A static library (`.a` / `.lib`).
	StaticLib,
	/// A dynamic library (`.so` / `.dll` / `.dylib`).
	DynamicLib,
	/// Backend-specific textual IR (LLVM IR, C source, ...). Useful for `--emit=ir`.
	Ir,
	/// Target assembly text.
	Asm,
}

/// Backend-side configuration. The fields that overlap with the rest of the
/// compiler (`optimization`, `target`) are pulled directly from [`Config`]
/// via [`BackendOptions::from_config`] so a driver never has to translate
/// between two parallel enums.
#[derive(Debug, Clone)]
pub struct BackendOptions
{
	pub optimization: Optimization,
	/// Full LLVM-style target (`arch[-sub]-vendor-os[-env]`). Backends should
	/// prefer reading from this rather than caching individual fields.
	pub target: Target,

	/// CPU name passed to the codegen layer (e.g. `znver3`, `generic`).
	/// `None` means "let the backend pick a sensible default for `target.arch`".
	pub cpu: Option<String>,
	/// Comma-separated target feature list (e.g. `+avx2,+fma`).
	pub features: Option<String>,

	pub debug_info: bool,

	pub output_kind: OutputKind,
	pub output_path: PathBuf,

	/// Where to dump intermediate artifacts when requested (IR, asm). If
	/// `None`, intermediates are not emitted.
	pub emit_dir: Option<PathBuf>,
}

impl BackendOptions
{
	/// Build options from the compiler-wide [`Config`], filling backend-only
	/// fields with sensible defaults. The driver typically calls this and then
	/// mutates `output_kind` / `output_path` based on CLI flags.
	pub fn from_config(cfg: &Config) -> Self
	{
		return Self {
			optimization: cfg.optimization.clone(),
			target: cfg.target.clone(),
			cpu: None,
			features: None,
			debug_info: matches!(cfg.optimization, Optimization::Debug),
			output_kind: OutputKind::Executable,
			output_path: PathBuf::from(default_exe_name(&cfg.target.os)),
			emit_dir: None,
		};
	}

	/// Convenience: architecture of the configured target.
	#[allow(unused)]
	pub const fn arch(&self) -> &Architecture
	{
		return &self.target.arch;
	}

	/// Convenience: OS of the configured target.
	#[allow(unused)]
	pub const fn os(&self) -> &OperatingSystem
	{
		return &self.target.os;
	}

	/// Convenience: vendor of the configured target.
	#[allow(unused)]
	pub const fn vendor(&self) -> &Vendor
	{
		return &self.target.vendor;
	}

	/// Convenience: ABI/environment of the configured target.
	#[allow(unused)]
	pub const fn env(&self) -> &Environment
	{
		return &self.target.env;
	}

	/// The LLVM-style triple string. Backends shelling out to `clang`,
	/// `llc`, or feeding `TargetMachine::create` want exactly this.
	#[allow(unused)]
	pub fn llvm_triple(&self) -> String
	{
		return self.target.to_llvm_triple();
	}
}

const fn default_exe_name(os: &OperatingSystem) -> &'static str
{
	return match os {
		OperatingSystem::Windows => "a.exe",
		_ => "a.out",
	};
}

/// Everything a backend gets handed to do its job. Borrowed so backends don't
/// need to take ownership of compiler-wide state.
pub struct BackendInput<'a>
{
	pub module: &'a MonoModule,
	pub symbols: &'a GlobalSymbolTable,
	pub options: &'a BackendOptions,
	pub source_map: &'a SourceMap,
}

/// What a backend produced. The `artifacts` vector lets backends report
/// secondary outputs (debug info files, IR dumps, ...).
#[derive(Debug, Default)]
pub struct BackendOutput
{
	pub primary: PathBuf,
	pub artifacts: Vec<PathBuf>,
}

/// Result type used by backends. Diagnostics are returned alongside the value
/// so a backend can emit warnings on success too, matching how `monomorphize`
/// already works in your pipeline.
pub type BackendResult<T> = Result<(T, Vec<DiagnosticBuilder>), Vec<DiagnosticBuilder>>;

/// The backend interface.
///
/// Implementors are expected to be cheap to construct; expensive per-compile
/// state should live inside `compile`. A single backend instance may be reused
/// for multiple compilations (the trait takes `&mut self` so caches are fine).
pub trait CompilerBackend
{
	/// Short identifier shown to users, e.g. `"llvm"`, `"c"`, `"cranelift"`.
	#[allow(unused)]
	fn name(&self) -> &'static str;

	/// Output kinds this backend can emit. The driver uses this to validate
	/// `BackendOptions::output_kind` before invoking `compile`.
	#[allow(unused)]
	fn supported_outputs(&self) -> &'static [OutputKind];

	/// Architectures this backend can target. Default impl returns an empty
	/// slice meaning "ask via `supports_target` instead"; most real backends
	/// should override one or the other.
	#[allow(unused)]
	fn supported_arches(&self) -> &'static [Architecture]
	{
		return &[];
	}

	/// Whether this backend can target the given configuration. Default impl
	/// consults `supported_arches`; override for finer-grained control (e.g.
	/// "LLVM supports almost anything, but our C backend has no msvc glue").
	#[allow(unused)]
	fn supports_target(&self, target: &Target) -> bool
	{
		let arches = self.supported_arches();
		return arches.is_empty() || arches.contains(&target.arch);
	}

	/// Lightweight sanity check that can fail before doing real work. Useful
	/// for "missing `clang` on PATH" style errors. Default is a no-op.
	fn validate(&self, _input: &BackendInput<'_>) -> Result<(), Vec<DiagnosticBuilder>>
	{
		return Ok(());
	}

	/// The main entry point. Consume a monomorphized module and produce an
	/// artifact at `options.output_path`.
	fn compile(&mut self, input: &BackendInput<'_>) -> BackendResult<BackendOutput>;
}

/// Convenience wrapper that boxes a backend so the driver can store a list of
/// them keyed by name without generics infecting everything upstream.
#[allow(unused)]
pub type DynBackend = Box<dyn CompilerBackend>;

/// Helper for drivers: pick a backend by name from a registry.
#[allow(unused)]
pub fn select_backend<'a>(backends: &'a mut [DynBackend], name: &str) -> Option<&'a mut dyn CompilerBackend>
{
	for b in backends {
		if b.name() == name {
			return Some(b.as_mut());
		}
	}
	return None;
}
