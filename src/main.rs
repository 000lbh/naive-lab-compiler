use koopa::ir::Type;
use parser::parse;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::{self, Result, Write};
use koopa::back::KoopaGenerator;

pub mod types;
pub mod parser;
pub mod irgen;
pub mod rvgen;

struct FileOrStdout (Box<dyn Write>);

impl Write for FileOrStdout {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        self.0.write(buf)
    }
    fn flush(&mut self) -> Result<()> {
        self.0.flush()
    }
    fn write_all(&mut self, mut buf: &[u8]) -> Result<()> {
        self.0.write_all(buf)
    }
    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> Result<()> {
        self.0.write_fmt(fmt)
    }
    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> Result<usize> {
        self.0.write_vectored(bufs)
    }
}

fn main() -> Result<()> {
    Type::set_ptr_size(4);
    let mut output_ir = false;
    let mut input = String::new();
    let mut output = String::new();
    {
        let args: Vec<String> = args().collect();
        output_ir = args[1] == "-koopa";
        input = args[2].clone();
        output = args[4].clone();
    }

    let input = read_to_string(input)?;
    let ast = parse(&input).unwrap();
    let mut output = 
        if output != "" {
            FileOrStdout(Box::new(File::create(output)?))
        } else {
            FileOrStdout(Box::new(io::stdout()))
        }
    ;
    if output_ir {
        let mut generator = KoopaGenerator::new(&mut output);
        generator.generate_on(&irgen::ast_to_koopa(ast))?;
    } else {
        rvgen::generator_rv(&irgen::ast_to_koopa(ast), &mut output)?;
    }
    Ok(())
}
