use std::io::{Write, Result};

/// Trait for structured assembly instructions.
/// Each ISA backend defines its own `Inst` enum implementing this trait.
pub trait AsmEmit {
    fn emit(&self, writer: &mut dyn Write) -> Result<()>;
}

/// Apply a sliding window optimization pass to a list of instructions.
///
/// The `rewrite` callback receives a mutable slice of `window_size` elements.
/// Return `Some(n)` to replace `n` elements (at the window start) with new ones,
/// or `None` to advance by 1 element.
pub fn peephole_optimize<I: Clone>(
    insts: &mut Vec<I>,
    window_size: usize,
    mut rewrite: impl FnMut(&[I]) -> Option<(usize, Vec<I>)>,
) {
    let mut i = 0;
    while i + window_size <= insts.len() {
        let window = &insts[i..i + window_size];
        if let Some((remove, replacements)) = rewrite(window) {
            let end = i + remove;
            insts.splice(i..end, replacements);
            // Don't advance; re-examine from current position
        } else {
            i += 1;
        }
    }
}

/// Simple peephole: try to match a 2-instruction pattern.
/// Returns `Some(replacements)` to replace both instructions, or `None`.
pub fn peephole2<I: Clone>(
    insts: &mut Vec<I>,
    mut rewrite: impl FnMut(&I, &I) -> Option<Vec<I>>,
) {
    peephole_optimize(insts, 2, move |w| {
        rewrite(&w[0], &w[1]).map(|r| (2, r))
    })
}

/// Peephole with 3-instruction window.
pub fn peephole3<I: Clone>(
    insts: &mut Vec<I>,
    mut rewrite: impl FnMut(&I, &I, &I) -> Option<Vec<I>>,
) {
    peephole_optimize(insts, 3, move |w| {
        rewrite(&w[0], &w[1], &w[2]).map(|r| (3, r))
    })
}
