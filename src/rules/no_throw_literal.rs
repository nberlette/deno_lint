// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use ::deno_ast::view::NodeTrait;
use deno_ast::view::{Expr, ThrowStmt};
use deno_ast::SourceRanged;
use derive_more::Display;

use super::{Context, LintRule};
use crate::diagnostic::LintFix;
use crate::diagnostic::LintFixChange;
use crate::handler::{Handler, Traverse};
use crate::Program;

#[derive(Debug)]
pub struct NoThrowLiteral;

const CODE: &str = "no-throw-literal";
const HINT: &str = r#"Use `throw new Error("...")` instead"#;
const FIX_DESC: &str = "Wrap the thrown value in an Error object";
const FIX_DESC_UNDEFINED: &str = "Replace `undefined` with `new Error()`";

#[derive(Display)]
enum NoThrowLiteralMessage {
  #[display(fmt = "expected an error object to be thrown")]
  ErrObjectExpected,

  #[display(fmt = "do not throw undefined")]
  Undefined,
}

impl LintRule for NoThrowLiteral {
  fn code(&self) -> &'static str {
    CODE
  }

  fn lint_program_with_ast_view(
    &self,
    context: &mut Context,
    program: Program,
  ) {
    NoThrowLiteralHandler.traverse(program, context);
  }
}

struct NoThrowLiteralHandler;

impl Handler for NoThrowLiteralHandler {
  fn throw_stmt(&mut self, throw_stmt: &ThrowStmt, ctx: &mut Context) {
    match throw_stmt.arg {
      Expr::Tpl(tpl) => {
        ctx.add_diagnostic_with_fixes(
          throw_stmt.range(),
          CODE,
          NoThrowLiteralMessage::ErrObjectExpected,
          Some(HINT.to_string()),
          vec![LintFix {
            description: FIX_DESC.into(),
            changes: vec![LintFixChange {
              range: tpl.range(),
              new_text: format!("new Error({})", tpl.text()).into(),
            }],
          }],
        );
      }
      Expr::Lit(lit) => ctx.add_diagnostic_with_fixes(
        throw_stmt.range(),
        CODE,
        NoThrowLiteralMessage::ErrObjectExpected,
        Some(HINT.to_string()),
        vec![LintFix {
          description: FIX_DESC.into(),
          changes: vec![LintFixChange {
            range: lit.range(),
            new_text: format!("new Error({})", {
              let s = lit.text();
              if s.starts_with('"') && s.ends_with('"')
                || s.starts_with('\'') && s.ends_with('\'')
                || s.starts_with('`') && s.ends_with('`')
              {
                s.to_string()
              } else {
                format!("\"{}\"", s)
              }
            })
            .into(),
          }],
        }],
      ),
      Expr::Unary(unary) if unary.op().as_str() == "void" => ctx
        .add_diagnostic_with_fixes(
          throw_stmt.range(),
          CODE,
          NoThrowLiteralMessage::Undefined,
          Some(HINT.to_string()),
          vec![LintFix {
            description: FIX_DESC_UNDEFINED.into(),
            changes: vec![LintFixChange {
              range: unary.range(),
              new_text: "new Error()".into(),
            }],
          }],
        ),
      Expr::Ident(ident) if *ident.sym() == *"undefined" => {
        ctx.add_diagnostic_with_fixes(
          throw_stmt.range(),
          CODE,
          NoThrowLiteralMessage::Undefined,
          Some(HINT.to_string()),
          vec![LintFix {
            description: FIX_DESC_UNDEFINED.into(),
            changes: vec![LintFixChange {
              range: ident.range(),
              new_text: "new Error()".into(),
            }],
          }],
        );
      }
      _ => {}
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn no_throw_literal_valid() {
    assert_lint_ok! {
      NoThrowLiteral,
      "throw e",
    };
  }

  #[test]
  fn no_throw_literal_tagged_template_literals_valid() {
    assert_lint_ok! {
      NoThrowLiteral, r#"throw err`kumiko`"#,
    }
  }

  #[test]
  fn no_throw_literal_preserve_quotes_invalid() {
    assert_lint_err! {
      NoThrowLiteral,
      r#"throw "kumiko""#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error("kumiko")"#),
      }],
      r#"throw 'kumiko'"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error('kumiko')"#),
      }],
      r#"throw `kumiko`"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error(`kumiko`)"#),
      }],
    }
  }

  #[test]
  fn no_throw_literal_invalid() {
    assert_lint_err! {
      NoThrowLiteral,
      r#"throw 'kumiko'"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error('kumiko')"#),
      }],
      r#"throw "kumiko""#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error("kumiko")"#),
      }],
      r#"throw true"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error("true")"#),
      }],
      r#"throw 1096"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error("1096")"#),
      }],
      r#"throw null"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::ErrObjectExpected,
        fix: (FIX_DESC, r#"throw new Error("null")"#),
      }],
      r#"throw undefined"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::Undefined,
        fix: (FIX_DESC_UNDEFINED, "throw new Error()"),
      }],
      r#"throw void 0"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::Undefined,
        fix: (FIX_DESC_UNDEFINED, "throw new Error()"),
      }],
      r#"throw void 1"#: [
      {
        col: 0,
        message: NoThrowLiteralMessage::Undefined,
        fix: (FIX_DESC_UNDEFINED, "throw new Error()"),
      }],
    }
  }
}
