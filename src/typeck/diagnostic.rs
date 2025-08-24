use crate::typeck::TypeChecker;
use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};

impl TypeChecker {
    pub fn report_error(&mut self, message: &str, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(self.file_id, span)]),
        );
    }
}
