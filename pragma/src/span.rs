use std::ops::Range;

#[derive(Debug, Copy, Clone)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Span(start, end)
    }

    pub fn merge(self, other: Span) -> Self {
        Span::new(self.0, other.1)
    }

    pub fn range(self) -> Range<usize> {
        self.0 as usize..self.1 as usize
    }
}

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Self {
        Span::new(span.start as u32, span.end as u32)
    }
}
