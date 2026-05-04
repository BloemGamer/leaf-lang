use core::ffi;

#[cfg(target_os = "linux")]
unsafe extern "C" {
	pub unsafe fn isatty(fd: ffi::c_int) -> ffi::c_int;
}
