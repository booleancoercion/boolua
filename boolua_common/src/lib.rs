use std::ops::Range;

pub type Span = Range<usize>;

pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

pub trait WithSpan {
    fn with_span(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned { data: self, span }
    }
}

impl<T> WithSpan for T {}
