use std::{io, fs, env};

use mzspeclib;

fn main() -> io::Result<()> {
    let mut it = env::args().skip(1);
    let fname = it.next().expect("Failed to provide filename");
    let handle = fs::File::open(fname)?;
    let reader = mzspeclib::MzSpecLibParser::new(io::BufReader::new(handle)).unwrap();
    eprintln!("{}", reader.header());
    for spec in reader {
        let spec = spec.unwrap();
        eprintln!("{spec}");
    }
    Ok(())
}
