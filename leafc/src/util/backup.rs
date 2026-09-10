pub trait Backup {
    type Backup;

    fn make_backup(&self) -> Self::Backup;
    fn load_backup(&mut self, backup: Self::Backup);
}
