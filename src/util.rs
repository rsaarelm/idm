use std::fmt;

/// Wrapper to truncate possibly very long values into readable prefixes when
/// debug-printing.
pub struct Trunc<'a, T: fmt::Debug + ?Sized>(pub &'a T);

impl<'a, T: fmt::Debug + ?Sized> fmt::Debug for Trunc<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const MAX_LEN: usize = 72;

        let fmt = format!("{:?}", self.0);

        if fmt.len() > MAX_LEN {
            let cut_pos = fmt.char_indices().take(MAX_LEN-3).map(|(i, _)| i).last().unwrap_or(0);
            write!(f, "{}...", &fmt[0..cut_pos])
        } else {
            write!(f, "{}", fmt)
        }
    }
}
