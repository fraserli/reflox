use anyhow::{anyhow, Result};

use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Token<'a> {
    pub ttype: TokenType,
    pub value: &'a str,
    pub row: usize,
    pub col: usize,
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    /* Single character tokens */
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    /* One or two character tokens */
    Bang, BangEqual, Equal, EqualEqual, Greater,
    GreaterEqual, Less, LessEqual,

    /* Literals */
    Identifier, LString, Number,

    /* Keywords */
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
}

pub fn lex<'a>(mut source: &'a str) -> Result<VecDeque<Token<'a>>> {
    use TokenType::*;

    let mut chars = source.chars().peekable();

    let mut row = 0;
    let mut col = 0;

    let mut tokens = VecDeque::new();

    while let Some(c) = chars.next() {
        if c == '\n' {
            row += 1;
            col = 0;
            source = &source[1..];
            continue;
        } else if c.is_whitespace() {
            col += 1;
            source = &source[c.len_utf8()..];
            continue;
        }

        let mut len = 1;
        let ttype = match c {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ',' => Comma,
            '.' => Dot,
            '-' => Minus,
            '+' => Plus,
            ';' => Semicolon,
            '*' => Star,
            '!' => {
                if chars.next_if(|c| *c == '=').is_some() {
                    len += 1;
                    BangEqual
                } else {
                    Bang
                }
            }
            '=' => {
                if chars.next_if(|c| *c == '=').is_some() {
                    len += 1;
                    EqualEqual
                } else {
                    Equal
                }
            }
            '>' => {
                if chars.next_if(|c| *c == '=').is_some() {
                    len += 1;
                    GreaterEqual
                } else {
                    Greater
                }
            }
            '<' => {
                if chars.next_if(|c| *c == '=').is_some() {
                    len += 1;
                    LessEqual
                } else {
                    Less
                }
            }
            '/' => {
                if chars.next_if(|c| *c == '/').is_some() {
                    len += 1 + count_bytes_while(&mut chars, |c| *c != '\n');
                    row += 1;
                    col = 0;
                    source = &source[len..];
                    continue;
                } else {
                    Slash
                }
            }
            '"' => {
                loop {
                    len += count_bytes_while(&mut chars, |c| *c != '"' && *c != '\n');
                    if let Some(c) = chars.next() {
                        len += 1;
                        if c == '"' {
                            break;
                        } else {
                            row += 1;
                            col = 0;
                        }
                    } else {
                        return Err(anyhow!(
                            "Unterminated string literal at line {} column {}",
                            row,
                            col
                        ));
                    }
                }
                LString
            }
            _ => {
                if c.is_ascii_digit() {
                    len += count_bytes_while(&mut chars, |c| c.is_ascii_digit());

                    if chars.peek() == Some(&'.') {
                        let mut p = chars.clone();
                        p.next();
                        if p.next_if(|c| c.is_ascii_digit()).is_some() {
                            chars.next();
                            len += 1 + count_bytes_while(&mut chars, |c| c.is_ascii_digit());
                        }
                    }

                    Number
                } else if c.is_ascii_alphabetic() {
                    len += count_bytes_while(&mut chars, |c| c.is_ascii_alphanumeric());
                    match &source[..len] {
                        "and" => And,
                        "class" => Class,
                        "else" => Else,
                        "false" => False,
                        "fun" => Fun,
                        "for" => For,
                        "if" => If,
                        "nil" => Nil,
                        "or" => Or,
                        "print" => Print,
                        "return" => Return,
                        "super" => Super,
                        "this" => This,
                        "true" => True,
                        "var" => Var,
                        "while" => While,
                        _ => Identifier,
                    }
                } else {
                    dbg!(source);
                    panic!("{c}");
                }
            }
        };

        let value = &source[..len];

        source = &source[len..];
        col += len;

        tokens.push_back(Token {
            ttype,
            value,
            row,
            col,
        });
    }

    Ok(tokens)
}

fn count_bytes_while<F>(chars: &mut Peekable<Chars>, func: F) -> usize
where
    F: FnOnce(&char) -> bool + Copy,
{
    let mut bytes = 0;

    while let Some(c) = chars.next_if(func) {
        bytes += c.len_utf8();
    }

    bytes
}
