//! BNF Grammar:
//! ```
//! expression -> unary ( binop expression )*
//! unary -> unop unary | primary
//! primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
//!
//! binop = "!=" | "==" | ">" | ">=" | "<" | "<=" | "-" | "+" | "/" | "*"
//! unop = "!" | "-"
//! ```

use crate::lexer::{Token, TokenType, TokenType::*};

use anyhow::{anyhow, Context, Result};

use std::collections::VecDeque;
use std::fmt::Display;

pub struct Parser<'a> {
    tokens: VecDeque<Token<'a>>,
}

#[derive(Debug)]
struct Expr {
    unary: Unary,
    ops: Vec<(TokenType, Box<Expr>)>,
}

#[derive(Debug)]
enum Unary {
    Unop(TokenType, Box<Unary>),
    Primary(Primary),
}

#[derive(Debug)]
enum Primary {
    Number(i64),
    String(String),
    Boolean(bool),
    Nil,
    Expr(Box<Expr>),
}

macro_rules! eat_matches {
    ($parser:expr, $($pattern:pat_param)|+) => {
        if matches!(
            $parser.tokens.get(0),
            Some(Token {
                ttype: $($pattern)|+,
                ..
            })
        ) {
            $parser.tokens.pop_front()
        } else {
            None
        }
    }
}

macro_rules! peek_matches {
    ($parser:expr, $($pattern:pat_param)|+) => {
        if matches!(
            $parser.tokens.get(0),
            Some(Token {
                ttype: $($pattern)|+,
                ..
            })
        ) {
            $parser.tokens.get(0)
        } else {
            None
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token<'a>>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<()> {
        let expr = self.expression(0)?;
        println!("{}", expr);
        Ok(())
    }

    fn consume(&mut self) -> Result<Token> {
        self.tokens.pop_front().ok_or(anyhow!("Unexpected EOF"))
    }

    fn eat(&mut self, ttype: TokenType) -> Result<Token> {
        if let Some(token) = self.tokens.pop_front() {
            if token.ttype == ttype {
                Ok(token)
            } else {
                Err(anyhow!(
                    "Expected {:?} got {:?} at line {} column {}",
                    ttype,
                    token.ttype,
                    token.row,
                    token.col
                ))
            }
        } else {
            Err(anyhow!("Unexpected EOF"))
        }
    }

    fn expression(&mut self, min_prec: usize) -> Result<Expr> {
        let unary = self.unary()?;

        let mut ops = Vec::new();

        while let Some(op) = peek_matches!(
            self,
            BangEqual
                | EqualEqual
                | Greater
                | GreaterEqual
                | Less
                | LessEqual
                | Minus
                | Plus
                | Slash
                | Star
        ) {
            let ttype = op.ttype;
            let (prec, assoc) = binop_props(ttype);

            if prec < min_prec {
                break;
            } else {
                self.consume().unwrap();

                let next_min_prec = match assoc {
                    Assoc::Left => prec + 1,
                    Assoc::Right => prec,
                };

                let rhs = self.expression(next_min_prec)?;

                ops.push((ttype, Box::new(rhs)));
            }
        }

        Ok(Expr { unary, ops })
    }

    fn unary(&mut self) -> Result<Unary> {
        if let Some(unop) = eat_matches!(self, Bang | Minus) {
            let rhs = self.unary()?;
            return Ok(Unary::Unop(unop.ttype, Box::new(rhs)));
        }

        let primary = self.primary()?;

        Ok(Unary::Primary(primary))
    }

    fn primary(&mut self) -> Result<Primary> {
        let token = self.consume()?;

        if matches!(token.ttype, Number | LString | True | False | Nil) {
            let primary = match token.ttype {
                Number => {
                    let num = token.value.parse().with_context(|| {
                        format!(
                            "invalid number literal at line {} column {}",
                            token.row, token.col
                        )
                    })?;

                    Primary::Number(num)
                }
                LString => Primary::String(token.value.to_owned()),
                True => Primary::Boolean(true),
                False => Primary::Boolean(false),
                Nil => Primary::Nil,
                _ => unreachable!(),
            };
            Ok(primary)
        } else if token.ttype == LeftParen {
            let expr = self.expression(0)?;
            let _rp = self.eat(RightParen)?;
            Ok(Primary::Expr(Box::new(expr)))
        } else {
            Err(anyhow!(
                "Expected expression at line {} column {}",
                token.row,
                token.col
            ))
        }
    }
}

#[allow(dead_code)]
enum Assoc {
    Left,
    Right,
}

fn binop_props(ttype: TokenType) -> (usize, Assoc) {
    use Assoc::*;
    match ttype {
        BangEqual | EqualEqual => (0, Left),
        Greater | GreaterEqual | Less | LessEqual => (1, Left),
        Minus | Plus => (2, Left),
        Slash | Star => (3, Left),
        _ => unreachable!(),
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ops.is_empty() {
            write!(f, "{}", self.unary)?;
        } else {
            write!(f, "({}", self.unary)?;
            for (op, expr) in &self.ops {
                write!(f, " {:?} {}", op, expr)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unop(op, unary) => write!(f, "{:?} {}", op, unary)?,
            Self::Primary(primary) => write!(f, "{}", primary)?,
        }
        Ok(())
    }
}

impl Display for Primary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num)?,
            Self::String(s) => write!(f, "{}", s)?,
            Self::Boolean(b) => write!(f, "{}", b)?,
            Self::Nil => write!(f, "NIL")?,
            Self::Expr(expr) => write!(f, "{}", expr)?,
        }
        Ok(())
    }
}
