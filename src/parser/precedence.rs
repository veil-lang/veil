use crate::lexer::Token;

pub type Precedence = u8;


impl<'a> super::Parser<'a> {
    pub fn get_prefix_bp(&self, token: &Token) -> Precedence {
        match token {
            Token::Star | Token::Plus | Token::Minus | Token::Bang => 8,
            _ => 0,
        }
    }

    pub fn get_infix_bp(&self, token: &Token) -> Option<(Precedence, Precedence)> {
        match token {
            Token::OrOr => Some((1, 2)),
            Token::AndAnd => Some((3, 4)),
            Token::EqEq | Token::NotEq => Some((5, 6)),
            Token::Gt | Token::Lt | Token::GtEq | Token::LtEq => Some((7, 8)),
            Token::Plus | Token::Minus => Some((9, 10)),
            Token::Star | Token::Slash | Token::Percent => Some((11, 12)),
            Token::DoubleStar | Token::Caret => Some((13, 14)),
            Token::DotDot | Token::DotDotEq | Token::DotDotGt | Token::DotDotLt => Some((15, 16)),
            Token::KwAs => Some((17, 18)),
            Token::Dot => Some((19, 20)),
            Token::LBracket => Some((21, 22)),
            Token::Eq => Some((1, 2)),
            _ => None,
        }
    }
}