use std::{fmt::Debug, marker::PhantomData};

/// A location in a source file. Contains redundant data to avoid extra computation.
#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub byte: usize,
    pub line: usize,
    pub col: usize,
}

/// Converts byte indices in a particular source text to `Loc`s.
#[derive(Debug)]
pub struct Locator<'i> {
    line_starts: Vec<usize>,
    _phantom: PhantomData<&'i ()>,
}

impl<'i> Locator<'i> {
    /// Returns a new `Locator` for the given source text.
    pub fn new(text: &'i str) -> Locator<'i> {
        let mut line_starts = [0usize]
            .into_iter()
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect::<Vec<_>>();
        if *line_starts.last().unwrap() >= text.len() {
            line_starts.pop();
        }
        Locator {
            line_starts,
            _phantom: PhantomData,
        }
    }

    /// Converts a byte offset in the original source text to a `Loc`.
    ///
    /// NOTE: `locate()` does not check that `byte` falls within the range of the original text.
    /// Instead, it will convert byte offsets from outside the original source text as if the last
    /// line extended indefinitely.
    pub fn locate(&self, byte: usize) -> Loc {
        match self.line_starts.binary_search(&byte) {
            Ok(i) => Loc {
                byte: byte,
                line: i,
                col: 0,
            },
            Err(i) => Loc {
                byte: byte,
                line: i - 1,
                col: byte - self.line_starts[i - 1],
            },
        }
    }
}
