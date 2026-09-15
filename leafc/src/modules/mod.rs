use std::{fs, path};

use crate::CrateInput;

#[derive(Debug)]
pub struct File
{
	pub path: path::PathBuf,
	pub module_path: Vec<String>,
}

pub fn find_all_files(crate_input: &CrateInput) -> impl Iterator<Item = File>
{
	return find_all_files_internal(&crate_input.root, crate_input.name.clone());
}

fn find_all_files_internal(start_dir: &path::Path, name: String) -> impl Iterator<Item = File>
{
	let mut stack = vec![(fs::read_dir(start_dir).expect("no directory path"), vec![name])];

	return std::iter::from_fn(move || {
		loop {
			let (entries, module_path) = stack.last_mut()?;

			match entries.next() {
				Some(Ok(entry)) => {
					let path = entry.path();

					if path.is_dir() {
						let mut child_module_path = module_path.clone();

						child_module_path.push(
							path.file_name()
								.expect("it should just exsist")
								.to_string_lossy()
								.into_owned(),
						);

						if let Ok(entries) = fs::read_dir(&path) {
							stack.push((entries, child_module_path));
						}
					} else if path.is_file() && matches!(path.extension(), Some(ext) if ext == "leaf") {
						return Some(File {
							path,
							module_path: module_path.clone(),
						});
					}
				}

				Some(Err(_)) => {}

				None => {
					stack.pop();
				}
			}
		}
	});
}
