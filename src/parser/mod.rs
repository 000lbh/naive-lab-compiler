use lalrpop_util::lalrpop_mod;
use super::types::ast::*;

lalrpop_mod!(sysy, "/src/parser/sysy.rs");

pub fn parse(src: &str) -> Result<CompUnit, String> {
    match sysy::CompUnitParser::new().parse(src) {
        Ok(result) => Ok(result),
        Err(err) => Err(err.to_string()),
    }
}
