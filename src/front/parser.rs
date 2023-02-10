use crate::define::ast::*;
use crate::define::Radix;
use crate::define::Span;
use crate::diagnostic::Diag;
use crate::diagnostic::DiagKind;
use crate::diagnostic::DiagLevel;
use crate::diagnostic::Diagnostics;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "front/grammar.pest"]
struct OriaParser;

/// Generate the parse tree.
pub fn generate_parse_tree(source: &str) -> Result<Pairs<Rule>, Error<Rule>> {
  OriaParser::parse(Rule::CompUnit, source)
}

/// Generate AST from the parse tree.
pub fn handle_compunit(
  source: &str,
  diagnostics: &mut Diagnostics,
) -> Result<CompUnit, Error<Rule>> {
  let pair = OriaParser::parse(Rule::CompUnit, source)?.next().unwrap();
  let span = pair.as_span();
  let line_col = pair.line_col();
  let items = pair
    .into_inner()
    .map(|pair| {
      if let Rule::Item = pair.as_rule() {
        Some(handle_item(pair, diagnostics))
      } else {
        None
      }
    })
    .filter(|maybe_item| maybe_item.is_some())
    .map(|maybe_item| maybe_item.unwrap())
    .collect();
  return Ok(CompUnit::new(items, line_col, span));
}

fn handle_expr(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Return => handle_return(pair, diagnostics),
    Rule::Break => handle_break(pair, diagnostics),
    Rule::Continue => handle_continue(pair, diagnostics),
    Rule::Assign => handle_assign(pair, diagnostics),
    Rule::OpExpr => handle_op_expr(pair, diagnostics),
    Rule::FnExpr => handle_fn_expr(pair, diagnostics),
    _ => unreachable!(),
  }
}

fn handle_return(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let maybe_pair = pair.into_inner().next();
  let mut expr = match maybe_pair {
    None => Expr::mk_return(None),
    Some(pair) => Expr::mk_return(Some(handle_expr(pair, diagnostics))),
  };
  expr.set_span(line_col, span);
  return expr;
}

fn handle_break(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let mut label = None;
  let mut expr = None;
  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => {
        label = Some(handle_label(pair, diagnostics));
      }
      Rule::Expr => {
        expr = Some(handle_expr(pair, diagnostics));
      }
      _ => unreachable!(),
    }
  }
  let mut expr = Expr::mk_break(label, expr);
  expr.set_span(line_col, span);
  return expr;
}

fn handle_continue(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let maybe_pair = pair.into_inner().next();
  let mut expr = match maybe_pair {
    None => Expr::mk_continue(None),
    Some(pair) => Expr::mk_continue(Some(handle_label(pair, diagnostics))),
  };
  expr.set_span(line_col, span);
  return expr;
}

fn handle_assign(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();

  let mut pairs = pair.into_inner();
  let lhs = handle_op_expr(pairs.next().unwrap(), diagnostics);
  let rhs = handle_expr(pairs.next().unwrap(), diagnostics);
  let mut expr = Expr::mk_assign(lhs, rhs);
  expr.set_span(line_col, span);
  return expr;
}

fn handle_fn_expr(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let mut pairs = pair.into_inner();
  let mut lhs = handle_fn_lhs(pairs.next().unwrap(), diagnostics);
  let rhs = handle_expr(pairs.next().unwrap(), diagnostics);

  lhs.expr = Some(Box::new(rhs));

  let mut expr = Expr::mk_fn(lhs);
  expr.set_span(line_col, span);
  return expr;
}

fn handle_fn_lhs(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Fn {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  let mut ret_ty = None;
  for pair in pairs {
    match pair.as_rule() {
      Rule::FnParams => {
        params = handle_fn_params(pair, diagnostics);
      }
      Rule::OpExpr => {
        ret_ty = Some(handle_op_expr(pair, diagnostics));
      }
      _ => unreachable!(),
    }
  }
  let lhs = Fn::new(None, params, ret_ty, None);
  return lhs;
}

fn handle_fn_params(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Vec<FnParam> {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  for pair in pairs {
    // `Ident?` and `QualifiedPath`
    let pairs = pair.into_inner();
    // Optional name of the parameter
    let mut name = None;
    // Optional type of the parameter
    let mut ty = None;
    let mut implicit = false;
    // Traverse the `Ident?` and `QualifiedPath`
    for pair in pairs {
      match pair.as_rule() {
        Rule::ParamSpec => implicit = true,
        Rule::Ident => name = Some(pair.as_str().to_string()),
        Rule::OpExpr => ty = Some(handle_op_expr(pair, diagnostics)),
        _ => unreachable!(),
      }
    }
    params.push(FnParam::new(name, ty, implicit));
  }
  return params;
}

fn handle_label(pair: Pair<Rule>, _diagnostics: &mut Diagnostics) -> Label {
  pair.as_str().to_string()
}

fn handle_op_expr(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pairs = pair.into_inner();

  use pest::pratt_parser::Assoc::*;
  use pest::pratt_parser::Op;
  use pest::pratt_parser::PrattParser;

  let pratt = PrattParser::new()
    .op(Op::infix(Rule::IntervalMin, Left) | Op::infix(Rule::IntervalMax, Left))
    .op(Op::infix(Rule::Range, Left) | Op::infix(Rule::RangeInclusive, Left))
    .op(Op::infix(Rule::LogicalOr, Left))
    .op(Op::infix(Rule::LogicalAnd, Left))
    .op(
      Op::infix(Rule::Eq, Left)
        | Op::infix(Rule::Ne, Left)
        | Op::infix(Rule::Lt, Left)
        | Op::infix(Rule::Le, Left)
        | Op::infix(Rule::Gt, Left)
        | Op::infix(Rule::Ge, Left),
    )
    .op(Op::infix(Rule::BitwiseOr, Left))
    .op(Op::infix(Rule::BitwiseXor, Left))
    .op(Op::infix(Rule::BitwiseAnd, Left))
    .op(Op::infix(Rule::Add, Left) | Op::infix(Rule::Sub, Left))
    .op(
      Op::infix(Rule::Mul, Left)
        | Op::infix(Rule::Div, Left)
        | Op::infix(Rule::Rem, Left),
    )
    .op(Op::infix(Rule::Shl, Right) | Op::infix(Rule::Shr, Right))
    .op(
      Op::prefix(Rule::RangeTo)
        | Op::prefix(Rule::RangeToInclusive)
        | Op::prefix(Rule::Pos)
        | Op::prefix(Rule::Neg)
        | Op::prefix(Rule::IntervalNeg)
        | Op::prefix(Rule::LogicalNot)
        | Op::prefix(Rule::BitwiseNot)
        | Op::prefix(Rule::Ref)
        | Op::prefix(Rule::Deref),
    )
    .op(Op::postfix(Rule::RangeFrom))
    .op(Op::infix(Rule::Path, Left));

  let mut parser = pratt
    .map_primary(|pair| handle_access_expr(pair, diagnostics))
    .map_prefix(|op, expr| match op.as_rule() {
      Rule::RangeTo => Expr::mk_range(RangeKind::To, None, Some(expr)),
      Rule::RangeToInclusive => {
        Expr::mk_range(RangeKind::ToInclusive, None, Some(expr))
      }
      Rule::Pos => Expr::mk_unary(UnaryOp::Pos, expr),
      Rule::Neg => Expr::mk_unary(UnaryOp::Neg, expr),
      Rule::LogicalNot => Expr::mk_unary(UnaryOp::Not, expr),
      Rule::BitwiseNot => Expr::mk_unary(UnaryOp::BitNot, expr),
      Rule::Ref => Expr::mk_unary(UnaryOp::Ref, expr),
      Rule::Deref => Expr::mk_unary(UnaryOp::Deref, expr),
      Rule::IntervalNeg => Expr::mk_unary(UnaryOp::IntervalNeg, expr),
      _ => unreachable!(),
    })
    .map_postfix(|expr, op| match op.as_rule() {
      Rule::RangeFrom => Expr::mk_range(RangeKind::From, Some(expr), None),
      _ => unreachable!(),
    })
    .map_infix(|lhs, op, rhs| match op.as_rule() {
      Rule::Range => Expr::mk_range(RangeKind::Exclusive, Some(lhs), Some(rhs)),
      Rule::RangeInclusive => {
        Expr::mk_range(RangeKind::Inclusive, Some(lhs), Some(rhs))
      }
      Rule::LogicalOr => Expr::mk_binary(BinOp::Or, lhs, rhs),
      Rule::LogicalAnd => Expr::mk_binary(BinOp::And, lhs, rhs),
      Rule::Eq => Expr::mk_binary(BinOp::Eq, lhs, rhs),
      Rule::Ne => Expr::mk_binary(BinOp::Ne, lhs, rhs),
      Rule::Lt => Expr::mk_binary(BinOp::Lt, lhs, rhs),
      Rule::Le => Expr::mk_binary(BinOp::Le, lhs, rhs),
      Rule::Gt => Expr::mk_binary(BinOp::Gt, lhs, rhs),
      Rule::Ge => Expr::mk_binary(BinOp::Ge, lhs, rhs),
      Rule::BitwiseOr => Expr::mk_binary(BinOp::BitOr, lhs, rhs),
      Rule::BitwiseXor => Expr::mk_binary(BinOp::BitXor, lhs, rhs),
      Rule::BitwiseAnd => Expr::mk_binary(BinOp::BitAnd, lhs, rhs),
      Rule::Add => Expr::mk_binary(BinOp::Add, lhs, rhs),
      Rule::Sub => Expr::mk_binary(BinOp::Sub, lhs, rhs),
      Rule::Mul => Expr::mk_binary(BinOp::Mul, lhs, rhs),
      Rule::Div => Expr::mk_binary(BinOp::Div, lhs, rhs),
      Rule::Rem => Expr::mk_binary(BinOp::Rem, lhs, rhs),
      Rule::Shl => Expr::mk_binary(BinOp::Shl, lhs, rhs),
      Rule::Shr => Expr::mk_binary(BinOp::Shr, lhs, rhs),
      Rule::Path => Expr::mk_binary(BinOp::Path, lhs, rhs),
      Rule::IntervalMin => Expr::mk_binary(BinOp::IntervalMin, lhs, rhs),
      Rule::IntervalMax => Expr::mk_binary(BinOp::IntervalMax, lhs, rhs),
      _ => unreachable!(),
    });

  let mut expr = parser.parse(pairs);
  expr.set_span(line_col, span);

  return expr;
}

fn handle_access_expr(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut pairs = pair.into_inner();

  let pair = pairs.next().unwrap();
  let span = pair.as_span();
  let line_col = pair.line_col();

  let mut expr = handle_primary_expr(pair, diagnostics);
  expr.set_span(line_col, span);

  for pair in pairs {
    let span = pair.as_span();
    let line_col = pair.line_col();
    match pair.as_rule() {
      Rule::PrimaryExpr => {
        expr = Expr::mk_access(expr, handle_primary_expr(pair, diagnostics));
      }
      Rule::FnArgs => {
        let args = handle_fn_args(pair, diagnostics);
        expr = Expr::mk_apply(expr, args);
      }
      _ => unreachable!(),
    }
    expr.set_span(line_col, span);
  }
  return expr;
}

fn handle_qualify_expr(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Expr {
  let mut pairs = pair.into_inner();

  let pair = pairs.next().unwrap();
  let span = pair.as_span();
  let line_col = pair.line_col();
  let mut expr = Expr::mk_ident(pair.as_str().to_string());
  expr.set_span(line_col, span);

  for pair in pairs {
    let span = pair.as_span();
    let line_col = pair.line_col();
    match pair.as_rule() {
      Rule::Ident => {
        expr = Expr::mk_qualify_expr(
          expr,
          Expr::mk_ident(pair.as_str().to_string()),
        );
      }
      Rule::FnArgs => {
        let args = handle_fn_args(pair, diagnostics);
        expr = Expr::mk_apply(expr, args);
      }
      _ => unreachable!(),
    }
    expr.set_span(line_col, span);
  }

  return expr;
}

fn handle_primary_expr(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Expr {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pair = pair.into_inner().next().unwrap();
  let mut expr = match pair.as_rule() {
    Rule::Universe => Expr::mk_universe(),
    Rule::Unit => Expr::mk_unit(),
    Rule::Literal => Expr::mk_lit(handle_literal(pair, diagnostics)),
    Rule::ExprWithBlock => handle_expr_with_block(pair, diagnostics),
    Rule::Ident => Expr::mk_ident(handle_identifier(pair, diagnostics)),
    Rule::RangeFull => Expr::mk_range(RangeKind::Full, None, None),
    Rule::FnTy => handle_fn_ty(pair, diagnostics),
    Rule::Tuple => handle_tuple(pair, diagnostics),
    Rule::Expr => handle_expr(pair, diagnostics),
    Rule::QualifyExpr => handle_qualify_expr(pair, diagnostics),
    _ => unreachable!(),
  };
  expr.set_span(line_col, span);
  return expr;
}

fn handle_identifier(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> String {
  let span = pair.as_span();
  let line_col = pair.line_col();

  let name = pair.into_inner().next().unwrap().as_str().to_string();

  if name.contains("__") {
    let span = Span::from_pest(line_col, span);
    diagnostics.add_diag(Diag(
      DiagLevel::Warning,
      DiagKind::AstTooManyUnderscoreInIdent(name.clone()),
      span,
    ));
  }

  return name;
}

fn handle_literal(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Lit {
  let pair = pair.into_inner().next().unwrap();
  let kind = match pair.as_rule() {
    Rule::DecInteger
    | Rule::HexInteger
    | Rule::OctInteger
    | Rule::BinInteger => LitKind::Integer,
    Rule::Floating => LitKind::Floating,
    _ => unreachable!(),
  };
  let radix = match pair.as_rule() {
    Rule::DecInteger => Radix::Dec,
    Rule::HexInteger => Radix::Hex,
    Rule::OctInteger => Radix::Oct,
    Rule::BinInteger => Radix::Bin,
    _ => Radix::Dec,
  };
  let pairs = pair.into_inner();
  let mut text = String::new();
  let mut suffix = None;
  for pair in pairs {
    match pair.as_rule() {
      Rule::LiteralSuffix => {
        suffix = Some(handle_literal_suffix(pair, diagnostics))
      }
      Rule::HexPrefix | Rule::OctPrefix | Rule::BinPrefix => {}
      _ => text.push_str(pair.as_str()),
    }
  }
  let lit = Lit {
    kind,
    radix,
    suffix,
    text,
  };
  return lit;
}

fn handle_literal_suffix(
  pair: Pair<Rule>,
  _diagnostics: &mut Diagnostics,
) -> LitSuffix {
  match pair.as_str() {
    "i8" => LitSuffix::I8,
    "i16" => LitSuffix::I16,
    "i32" => LitSuffix::I32,
    "i64" => LitSuffix::I64,
    "u8" => LitSuffix::U8,
    "u16" => LitSuffix::U16,
    "u32" => LitSuffix::U32,
    "u64" => LitSuffix::U64,
    "f32" => LitSuffix::F32,
    "f64" => LitSuffix::F64,
    "isize" => LitSuffix::ISize,
    "usize" => LitSuffix::USize,
    _ => unreachable!(),
  }
}

fn handle_expr_with_block(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Expr {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Loop => handle_loop(pair, diagnostics),
    Rule::While => handle_while(pair, diagnostics),
    Rule::For => handle_for(pair, diagnostics),
    Rule::If => handle_if(pair, diagnostics),
    Rule::IfLet => handle_if_let(pair, diagnostics),
    Rule::Match => handle_match(pair, diagnostics),
    Rule::Block => Expr::mk_block(handle_block(pair, diagnostics)),
    Rule::StructInit => handle_struct_init(pair, diagnostics),
    _ => unreachable!(),
  }
}

fn handle_struct_init(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut pairs = pair.into_inner();
  let qualified = handle_qualify_expr(pairs.next().unwrap(), diagnostics);
  let fields = pairs
    .map(|pair| {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let expr = handle_expr(pairs.next().unwrap(), diagnostics);
      return (name, expr);
    })
    .collect();
  return Expr::mk_struct(qualified, fields);
}

fn handle_loop(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut label = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair, diagnostics)),
      Rule::Block => block = Some(handle_block(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_loop(label, block.unwrap());
}

fn handle_while(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut label = None;
  let mut expr = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair, diagnostics)),
      Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
      Rule::Block => block = Some(handle_block(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_while(label, expr.unwrap(), block.unwrap());
}

fn handle_for(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut label = None;
  let mut pat = None;
  let mut expr = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair, diagnostics)),
      Rule::Pattern => pat = Some(handle_pattern(pair, diagnostics)),
      Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
      Rule::Block => block = Some(handle_block(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_for(label, pat.unwrap(), expr.unwrap(), block.unwrap());
}

fn handle_if(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut expr = None;
  let mut block = None;
  let mut else_ = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
      Rule::Block => block = Some(handle_block(pair, diagnostics)),
      Rule::ExprWithBlock => {
        else_ = Some(handle_expr_with_block(pair, diagnostics))
      }
      _ => unreachable!(),
    }
  }

  return Expr::mk_if(expr.unwrap(), block.unwrap(), else_);
}

fn handle_if_let(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut pat = None;
  let mut expr = None;
  let mut block = None;
  let mut else_ = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Pattern => pat = Some(handle_pattern(pair, diagnostics)),
      Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
      Rule::Block => block = Some(handle_block(pair, diagnostics)),
      Rule::ExprWithBlock => {
        else_ = Some(handle_expr_with_block(pair, diagnostics))
      }
      _ => unreachable!(),
    }
  }

  return Expr::mk_if_let(IfLet::new(
    pat.unwrap(),
    expr.unwrap(),
    block.unwrap(),
    else_,
  ));
}

fn handle_match(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let mut pairs = pair.into_inner();
  let expr = handle_expr(pairs.next().unwrap(), diagnostics);
  let arms = pairs
    .map(|pair| handle_match_arm(pair, diagnostics))
    .collect();
  return Expr::mk_match(expr, arms);
}

fn handle_match_arm(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> MatchArm {
  let mut pairs = pair.into_inner();
  let pat = handle_pattern(pairs.next().unwrap(), diagnostics);
  let expr = handle_expr(pairs.next().unwrap(), diagnostics);
  return MatchArm::new(pat, expr);
}

fn handle_block(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Block {
  let pairs = pair.into_inner();
  let mut label = None;
  let mut stmts = Vec::new();
  let mut comptime = false;

  for pair in pairs {
    match pair.as_rule() {
      Rule::BlockSpec => comptime = true,
      Rule::Label => label = Some(handle_label(pair, diagnostics)),
      Rule::Stmt => stmts.push(handle_stmt(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Block::new(label, stmts, comptime);
}

fn handle_fn_ty(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  let mut ret_ty = None;

  for pair in pairs {
    match pair.as_rule() {
      Rule::FnTyParams => {
        params = handle_fn_ty_params(pair, diagnostics);
      }
      Rule::OpExpr => {
        ret_ty = Some(handle_op_expr(pair, diagnostics));
      }
      _ => unreachable!(),
    }
  }

  let expr = Expr::mk_fn_ty(params, ret_ty.unwrap());
  return expr;
}

fn handle_fn_ty_params(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Vec<FnParam> {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  for pair in pairs {
    // `Ident?` and `QualifiedPath`
    let pairs = pair.into_inner();
    // Optional name of the parameter
    let mut name = None;
    // Optional type of the parameter
    let mut ty = None;
    let mut implicit = false;
    // Traverse the `Ident?` and `QualifiedPath`
    for pair in pairs {
      match pair.as_rule() {
        Rule::ParamSpec => implicit = true,
        Rule::Ident => name = Some(pair.as_str().to_string()),
        Rule::OpExpr => ty = Some(handle_op_expr(pair, diagnostics)),
        _ => unreachable!(),
      }
    }
    params.push(FnParam::new(name, ty, implicit));
  }
  return params;
}

fn handle_tuple(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Expr {
  Expr::mk_tuple(
    pair
      .into_inner()
      .map(|pair| handle_expr(pair, diagnostics))
      .collect(),
  )
}

fn handle_fn_args(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Vec<FnArg> {
  let mut args = Vec::new();
  // Traverse the list of `FnArg`
  for pair in pair.into_inner() {
    // `Ident?` and `Expr`
    let pairs = pair.into_inner();
    // Optional name of the argument
    let mut name = None;
    // Expression of the argument
    let mut expr = None;
    // Traverse the `Ident?` and `Expr`
    for pair in pairs {
      match pair.as_rule() {
        Rule::Ident => name = Some(pair.as_str().to_string()),
        Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
        _ => unreachable!(),
      }
    }
    args.push(FnArg::new(name, expr.unwrap()));
  }
  return args;
}

fn handle_pattern(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Pat {
  let pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
  if pairs.len() == 1 {
    return handle_primary_pattern(pairs[0].clone(), diagnostics);
  } else {
    return Pat::mk_or(
      pairs
        .into_iter()
        .map(|pair| handle_primary_pattern(pair, diagnostics))
        .collect(),
    );
  }
}

fn handle_primary_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::RangePattern => handle_range_pattern(pair, diagnostics),
    Rule::PatternWithoutRange => {
      handle_pattern_without_range(pair, diagnostics)
    }
    _ => unreachable!(),
  }
}

fn handle_range_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pair = pair.into_inner().next().unwrap();
  let kind = match pair.as_rule() {
    Rule::RangePatternInclusive => RangePatKind::Inclusive,
    Rule::RangePatternExclusive => RangePatKind::Exclusive,
    Rule::RangePatternFrom => RangePatKind::From,
    Rule::RangePatternTo => RangePatKind::To,
    Rule::RangePatternToInclusive => RangePatKind::ToInclusive,
    _ => unreachable!(),
  };
  let mut pat = match pair.as_rule() {
    Rule::RangePatternInclusive | Rule::RangePatternExclusive => {
      let mut pairs = pair.into_inner();
      let lhs = handle_range_pattern_bound(pairs.next().unwrap(), diagnostics);
      let rhs = handle_range_pattern_bound(pairs.next().unwrap(), diagnostics);
      Pat::mk_range(kind, lhs, rhs)
    }
    Rule::RangePatternFrom => {
      let bound = handle_range_pattern_bound(
        pair.into_inner().next().unwrap(),
        diagnostics,
      );
      Pat::mk_range(kind, bound, None)
    }
    Rule::RangePatternTo | Rule::RangePatternToInclusive => {
      let bound = handle_range_pattern_bound(
        pair.into_inner().next().unwrap(),
        diagnostics,
      );
      Pat::mk_range(kind, None, bound)
    }
    _ => unreachable!(),
  };
  pat.set_span(line_col, span);
  return pat;
}

fn handle_range_pattern_bound(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Option<RangePatBound> {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Literal => {
      Some(RangePatBound::Lit(handle_literal(pair, diagnostics)))
    }
    Rule::QualifyExpr => Some(RangePatBound::Qualify(Box::new(
      handle_qualify_expr(pair, diagnostics),
    ))),
    _ => unreachable!(),
  }
}

fn handle_pattern_without_range(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pair = pair.into_inner().next().unwrap();

  let mut pat = match pair.as_rule() {
    Rule::LiteralPattern => handle_literal_pattern(pair, diagnostics),
    Rule::IdentPattern => handle_ident_pattern(pair, diagnostics),
    Rule::WildcardPattern => Pat::mk_wildcard(),
    Rule::RestPattern => Pat::mk_rest(),
    Rule::StructPattern => handle_struct_pattern(pair, diagnostics),
    Rule::TuplePattern => handle_tuple_pattern(pair, diagnostics),
    Rule::QualifyPattern => Pat::mk_qualify(handle_qualify_expr(
      pair.into_inner().next().unwrap(),
      diagnostics,
    )),
    Rule::Pattern => handle_pattern(pair, diagnostics),
    Rule::ConstructorPattern => handle_constructor_pattern(pair, diagnostics),
    _ => unreachable!(),
  };

  pat.set_span(line_col, span);
  return pat;
}

fn handle_constructor_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let mut pairs = pair.into_inner();
  let qualified = handle_qualify_expr(pairs.next().unwrap(), diagnostics);

  let args = pairs
    .next()
    .unwrap()
    .into_inner()
    .map(|pair| handle_constructor_pattern_arg(pair, diagnostics))
    .collect();
  return Pat::mk_constructor(qualified, args);
}

fn handle_constructor_pattern_arg(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> ConstructorPatArg {
  let mut name = None;
  let mut pat = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Ident => name = Some(pair.as_str().to_string()),
      Rule::Pattern => pat = Some(handle_pattern(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return ConstructorPatArg::new(name, pat.unwrap());
}

fn handle_tuple_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  Pat::mk_tuple(
    pair
      .into_inner()
      .map(|pair| handle_pattern(pair, diagnostics))
      .collect(),
  )
}

fn handle_struct_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let mut pairs = pair.into_inner();
  let mut elems = Vec::new();

  let qualified = handle_qualify_expr(pairs.next().unwrap(), diagnostics);
  for pair in pairs {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
      Rule::RestPattern => elems.push(StructPatElem::Rest),
      Rule::StructPatternField => {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap().as_str().to_string();
        let pat = handle_pattern(pairs.next().unwrap(), diagnostics);
        elems.push(StructPatElem::Field(name, pat));
      }
      _ => unreachable!(),
    }
  }

  return Pat::mk_struct(qualified, elems);
}

fn handle_literal_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  Pat::mk_lit(handle_literal(
    pair.into_inner().next().unwrap(),
    diagnostics,
  ))
}

fn handle_ident_pattern(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> Pat {
  let mut name = String::new();
  let mut spec = IdentPatSpec::None;
  let pairs = pair.into_inner();
  for pair in pairs {
    match pair.as_rule() {
      Rule::IdentPatternSpec => {
        spec = hanlde_ident_pattern_spec(pair, diagnostics)
      }
      Rule::Ident => name = pair.as_str().to_string(),
      _ => unreachable!(),
    }
  }

  return Pat::mk_ident(spec, name);
}

fn hanlde_ident_pattern_spec(
  pair: Pair<Rule>,
  _diagnostics: &mut Diagnostics,
) -> IdentPatSpec {
  let spec_str = pair.as_str().to_string();
  let spec_str = spec_str.split(" ").collect::<Vec<_>>();
  let mut spec = IdentPatSpec::None;
  for s in spec_str {
    match s {
      "comptime" => spec = IdentPatSpec::Comptime,
      "ref" => spec = IdentPatSpec::Ref,
      "mut" => {
        if let IdentPatSpec::Ref = spec {
          spec = IdentPatSpec::RefMut;
        } else {
          spec = IdentPatSpec::Mut
        }
      }
      _ => {}
    }
  }
  return spec;
}

fn handle_stmt(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Stmt {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pair = pair.into_inner().next().unwrap();
  let mut stmt = match pair.as_rule() {
    Rule::Let => handle_let(pair, diagnostics),
    Rule::Item => Stmt::mk_item(handle_item(pair, diagnostics)),
    Rule::Expr => Stmt::mk_expr(handle_expr(pair, diagnostics)),
    _ => unreachable!(),
  };
  stmt.set_span(line_col, span);
  return stmt;
}

fn handle_let(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Stmt {
  let mut pairs = pair.into_inner();
  let pat = handle_pattern_without_range(pairs.next().unwrap(), diagnostics);
  let mut init = None;
  let mut ty = None;
  for pair in pairs {
    match pair.as_rule() {
      Rule::AccessExpr => {
        ty = Some(handle_access_expr(pair, diagnostics));
      }
      Rule::Expr => {
        init = Some(handle_expr(pair, diagnostics));
      }
      _ => unreachable!(),
    }
  }

  return Stmt::mk_let(Let::new(pat, ty, init));
}

fn handle_def(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let mut builtin = false;
  let mut name = None;
  let mut func = None;
  let mut expr = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::DefSpec => builtin = true,
      Rule::Ident => name = Some(pair.as_str().to_string()),
      Rule::FnLhs => func = Some(handle_fn_lhs(pair, diagnostics)),
      Rule::Expr => expr = Some(handle_expr(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  let mut func = func.unwrap();
  func.name = Some(name.unwrap());

  func.expr = expr.map(Box::new);

  return Item::mk_def(Def::new(builtin, func));
}

fn handle_item(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let span = pair.as_span();
  let line_col = pair.line_col();
  let pair = pair.into_inner().next().unwrap();
  let mut item = match pair.as_rule() {
    Rule::Use => handle_use(pair, diagnostics),
    Rule::Import => handle_import(pair, diagnostics),
    Rule::Def => handle_def(pair, diagnostics),
    Rule::Const => handle_const(pair, diagnostics),
    Rule::Type => handle_type(pair, diagnostics),
    Rule::Impl => hanlde_impl(pair, diagnostics),
    Rule::Module => handle_module(pair, diagnostics),
    Rule::Interface => handle_interface(pair, diagnostics),
    _ => unreachable!(),
  };
  item.set_span(line_col, span);
  return item;
}

fn handle_use(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  Item::mk_use(handle_use_tree(
    pair.into_inner().next().unwrap(),
    diagnostics,
  ))
}

fn handle_use_tree(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> UseTree {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Ident => {
      UseTree::new(pair.as_str().to_string(), None, false, Vec::new())
    }
    Rule::UseTreeAll => UseTree::new(
      pair.into_inner().next().unwrap().as_str().to_string(),
      None,
      true,
      Vec::new(),
    ),
    Rule::UseTreeAlias => {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let alias = pairs.next().unwrap().as_str().to_string();
      UseTree::new(name, Some(alias), false, Vec::new())
    }
    Rule::UseTreeMulti => {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let children = pairs
        .map(|pair| handle_use_tree(pair, diagnostics))
        .collect();
      UseTree::new(name, None, false, children)
    }
    Rule::UseTreeSimple => {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let children = pairs
        .map(|pair| handle_use_tree(pair, diagnostics))
        .collect();
      UseTree::new(name, None, false, children)
    }
    _ => unreachable!(),
  }
}

fn handle_import(pair: Pair<Rule>, _diagnostics: &mut Diagnostics) -> Item {
  Item::mk_import(pair.into_inner().next().unwrap().as_str().to_string())
}

fn handle_const(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let ty = handle_access_expr(pairs.next().unwrap(), diagnostics);
  let init = handle_expr(pairs.next().unwrap(), diagnostics);
  return Item::mk_const(Const::new(name, ty, init));
}

fn handle_type(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let pairs = pair.into_inner();
  let mut builtin = false;
  let mut name = String::new();
  let mut ty = None;
  let mut body = None;

  for pair in pairs {
    match pair.as_rule() {
      Rule::TypeSpec => builtin = true,
      Rule::Ident => name = handle_identifier(pair, diagnostics),
      Rule::FnLhs => ty = Some(handle_fn_lhs(pair, diagnostics)),
      Rule::TypeBody => body = Some(handle_type_body(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Item::mk_type(Type::new(builtin, name, ty, body));
}

fn handle_type_body(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> TypeBody {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Struct => handle_struct(pair, diagnostics),
    Rule::Expr => TypeBody::Expr(handle_expr(pair, diagnostics)),
    Rule::Constructors => handle_constructors(pair, diagnostics),
    _ => unreachable!(),
  }
}

fn handle_constructors(
  pair: Pair<Rule>,
  diagnostics: &mut Diagnostics,
) -> TypeBody {
  let pairs = pair.into_inner();
  let constructors = pairs
    .map(|pair| {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let mut params = Vec::new();
      let mut ret_ty = None;
      for pair in pairs {
        match pair.as_rule() {
          Rule::FnTyParams => {
            params = handle_fn_ty_params(pair, diagnostics);
          }
          Rule::AccessExpr => {
            ret_ty = Some(handle_access_expr(pair, diagnostics));
          }
          _ => unreachable!(),
        }
      }
      let func = Fn::new(Some(name.clone()), params, ret_ty, None);
      (name, Some(func))
    })
    .collect();
  return TypeBody::Constructors(constructors);
}

fn handle_interface(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let pairs = pair.into_inner();
  let mut name = String::new();
  let mut params = None;
  let mut body = Vec::new();

  for pair in pairs {
    match pair.as_rule() {
      Rule::Ident => name = pair.as_str().to_string(),
      Rule::FnParams => params = Some(handle_fn_params(pair, diagnostics)),
      Rule::Def => body.push(handle_def(pair, diagnostics)),
      _ => unreachable!(),
    }
  }

  return Item::mk_interface(Interface::new(name, params, body));
}

fn handle_struct(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> TypeBody {
  let pairs = pair.into_inner();
  let mut fields = Vec::new();

  for pair in pairs {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_string();
    let expr = handle_expr(pairs.next().unwrap(), diagnostics);
    fields.push((name, expr));
  }
  return TypeBody::Struct(fields);
}

fn hanlde_impl(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let mut pairs = pair.into_inner();
  let expr = handle_qualify_expr(pairs.next().unwrap(), diagnostics);
  let items = pairs.map(|pair| handle_def(pair, diagnostics)).collect();
  return Item::mk_impl(expr, items);
}

fn handle_module(pair: Pair<Rule>, diagnostics: &mut Diagnostics) -> Item {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let items = pairs.map(|pair| handle_item(pair, diagnostics)).collect();
  return Item::mk_module(Module::new(name, items));
}
