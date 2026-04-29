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
pub mod x86gen;
pub mod lagen;
pub mod y86gen;

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
    let mut output_x64 = false;
    let mut output_la64 = false;
    let mut input = String::new();
    let mut output = String::new();
    {
        let args: Vec<String> = args().collect();
        if args.len() < 2 {
            eprintln!("Usage: {} [-koopa] [-x64] <input> [-o <output>]", args[0]);
            std::process::exit(1);
        }
        let mut i = 1;
        while i < args.len() {
            match args[i].as_str() {
                "-koopa" => {
                    output_ir = true;
                    i += 1;
                }
                "-x64" => {
                    output_x64 = true;
                    i += 1;
                }
                "-la64" => {
                    output_la64 = true;
                    i += 1;
                }
                "-o" => {
                    i += 1;
                    if i < args.len() {
                        output = args[i].clone();
                    }
                    i += 1;
                }
                _ => {
                    input = args[i].clone();
                    i += 1;
                }
            }
        }
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
    } else if output_x64 {
        x86gen::generator_x64(&irgen::ast_to_koopa(ast), &mut output)?;
    } else if output_la64 {
        lagen::generator_la(&irgen::ast_to_koopa(ast), &mut output)?;
    } else {
        rvgen::generator_rv(&irgen::ast_to_koopa(ast), &mut output)?;
    }
    Ok(())
}
