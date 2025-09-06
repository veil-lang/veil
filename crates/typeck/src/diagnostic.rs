use crate::TypeChecker;
use veil_diagnostics::{Diagnostic, Label, Span};

impl TypeChecker {
    pub fn report_error(&mut self, message: &str, span: Span) {
        self.errors.push(
            Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(self.context.file_id, span)]),
        );
    }
}
