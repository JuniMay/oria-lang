use crate::define::ast::*;
use crate::define::Radix;
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
pub fn handle_compunit(source: &str) -> Result<CompUnit, Error<Rule>> {
  let pair = OriaParser::parse(Rule::CompUnit, source)?.next().unwrap();
  let span = pair.as_span();
  let items = pair
    .into_inner()
    .map(|pair| {
      if let Rule::Item = pair.as_rule() {
        Some(handle_item(pair))
      } else {
        None
      }
    })
    .filter(|maybe_item| maybe_item.is_some())
    .map(|maybe_item| maybe_item.unwrap())
    .collect();
  return Ok(CompUnit::new(items, span));
}

fn handle_expr(pair: Pair<Rule>) -> Expr {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Return => handle_return(pair),
    Rule::Break => handle_break(pair),
    Rule::Continue => handle_continue(pair),
    Rule::Assign => handle_assign(pair),
    Rule::OpExpr => handle_op_expr(pair),
    Rule::FnExpr => handle_fn_expr(pair),
    _ => unreachable!(),
  }
}

fn handle_return(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let maybe_pair = pair.into_inner().next();
  let mut expr = match maybe_pair {
    None => Expr::mk_return(None),
    Some(pair) => Expr::mk_return(Some(handle_expr(pair))),
  };
  expr.set_span(span);
  return expr;
}

fn handle_break(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let mut label = None;
  let mut expr = None;
  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => {
        label = Some(handle_label(pair));
      }
      Rule::Expr => {
        expr = Some(handle_expr(pair));
      }
      _ => unreachable!(),
    }
  }
  let mut expr = Expr::mk_break(label, expr);
  expr.set_span(span);
  return expr;
}

fn handle_continue(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let maybe_pair = pair.into_inner().next();
  let mut expr = match maybe_pair {
    None => Expr::mk_continue(None),
    Some(pair) => Expr::mk_continue(Some(handle_label(pair))),
  };
  expr.set_span(span);
  return expr;
}

fn handle_assign(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let mut pairs = pair.into_inner();
  let lhs = handle_op_expr(pairs.next().unwrap());
  let rhs = handle_expr(pairs.next().unwrap());
  let mut expr = Expr::mk_assign(lhs, rhs);
  expr.set_span(span);
  return expr;
}

fn handle_fn_expr(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let mut pairs = pair.into_inner();
  let mut lhs = handle_fn_lhs(pairs.next().unwrap());
  let rhs = handle_expr(pairs.next().unwrap());

  lhs.expr = Some(Box::new(rhs));

  let mut expr = Expr::mk_fn(lhs);
  expr.set_span(span);
  return expr;
}

fn handle_fn_lhs(pair: Pair<Rule>) -> Fn {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  let mut ret_ty = None;
  for pair in pairs {
    match pair.as_rule() {
      Rule::FnParamsImplicit => {
        params.extend(handle_fn_params(pair, true));
      }
      Rule::FnParamsExplicit => {
        params.extend(handle_fn_params(pair, false));
      }
      Rule::QualifiedExpr => {
        ret_ty = Some(handle_qualified_expr(pair));
      }
      _ => unreachable!(),
    }
  }
  let lhs = Fn::new(None, params, ret_ty, None);
  return lhs;
}

fn handle_fn_params(pair: Pair<Rule>, implicit: bool) -> Vec<FnParam> {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  for pair in pairs {
    // `Ident?` and `QualifiedPath`
    let pairs = pair.into_inner();
    // Optional name of the parameter
    let mut name = None;
    // Optional type of the parameter
    let mut ty = None;
    // Traverse the `Ident?` and `QualifiedPath`
    for pair in pairs {
      match pair.as_rule() {
        Rule::Ident => name = Some(pair.as_str().to_string()),
        Rule::QualifiedExpr => ty = Some(handle_qualified_expr(pair)),
        _ => unreachable!(),
      }
    }
    params.push(FnParam::new(name, ty, implicit));
  }
  return params;
}

fn handle_label(pair: Pair<Rule>) -> Label {
  pair.as_str().to_string()
}

fn handle_op_expr(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let pairs = pair.into_inner();

  use pest::pratt_parser::Assoc::*;
  use pest::pratt_parser::Op;
  use pest::pratt_parser::PrattParser;

  let pratt = PrattParser::new()
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
        | Op::prefix(Rule::LogicalNot)
        | Op::prefix(Rule::BitwiseNot)
        | Op::prefix(Rule::Ref)
        | Op::prefix(Rule::Deref),
    )
    .op(Op::postfix(Rule::RangeFrom));

  let mut parser = pratt
    .map_primary(handle_qualified_expr)
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
      _ => unreachable!(),
    });

  let mut expr = parser.parse(pairs);
  expr.set_span(span);

  return expr;
}

fn handle_qualified_expr(pair: Pair<Rule>) -> Expr {
  let path = handle_qualified_path(pair.into_inner().next().unwrap());
  let expr = if path.len() == 1 {
    path[0].clone()
  } else {
    Expr::mk_qualified_path(path)
  };
  return expr;
}

fn handle_qualified_path(pair: Pair<Rule>) -> QualifiedPath {
  pair.into_inner().map(handle_atomic_expr).collect()
}

fn handle_atomic_expr(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let mut pairs = pair.into_inner();
  let mut expr = handle_primary_expr(pairs.next().unwrap());
  for pair in pairs {
    if let Rule::FnArgs = pair.as_rule() {
      expr = Expr::mk_apply(expr, handle_fn_args(pair));
    }
  }
  expr.set_span(span);
  return expr;
}

fn handle_primary_expr(pair: Pair<Rule>) -> Expr {
  let span = pair.as_span();
  let pair = pair.into_inner().next().unwrap();
  let mut expr = match pair.as_rule() {
    Rule::Universe => Expr::mk_universe(),
    Rule::Unit => Expr::mk_unit(),
    Rule::Literal => Expr::mk_lit(handle_literal(pair)),
    Rule::ExprWithBlock => handle_expr_with_block(pair),
    Rule::Ident => Expr::mk_ident(pair.as_str().to_string()),
    Rule::RangeFull => Expr::mk_range(RangeKind::Full, None, None),
    Rule::FnTy => handle_fn_ty(pair),
    Rule::Tuple => handle_tuple(pair),
    Rule::Expr => handle_expr(pair),
    _ => unreachable!(),
  };
  expr.set_span(span);
  return expr;
}

fn handle_literal(pair: Pair<Rule>) -> Lit {
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
      Rule::LiteralSuffix => suffix = Some(handle_literal_suffix(pair)),
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

fn handle_literal_suffix(pair: Pair<Rule>) -> LitSuffix {
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

fn handle_expr_with_block(pair: Pair<Rule>) -> Expr {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Loop => handle_loop(pair),
    Rule::While => handle_while(pair),
    Rule::For => handle_for(pair),
    Rule::If => handle_if(pair),
    Rule::IfLet => handle_if_let(pair),
    Rule::Match => handle_match(pair),
    Rule::Block => Expr::mk_block(handle_block(pair)),
    _ => unreachable!(),
  }
}

fn handle_loop(pair: Pair<Rule>) -> Expr {
  let mut label = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair)),
      Rule::Block => block = Some(handle_block(pair)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_loop(label, block.unwrap());
}

fn handle_while(pair: Pair<Rule>) -> Expr {
  let mut label = None;
  let mut expr = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair)),
      Rule::Expr => expr = Some(handle_expr(pair)),
      Rule::Block => block = Some(handle_block(pair)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_while(label, expr.unwrap(), block.unwrap());
}

fn handle_for(pair: Pair<Rule>) -> Expr {
  let mut label = None;
  let mut pat = None;
  let mut expr = None;
  let mut block = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Label => label = Some(handle_label(pair)),
      Rule::Pattern => pat = Some(handle_pattern(pair)),
      Rule::Expr => expr = Some(handle_expr(pair)),
      Rule::Block => block = Some(handle_block(pair)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_for(label, pat.unwrap(), expr.unwrap(), block.unwrap());
}

fn handle_if(pair: Pair<Rule>) -> Expr {
  let mut expr = None;
  let mut block = None;
  let mut else_ = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Expr => expr = Some(handle_expr(pair)),
      Rule::Block => block = Some(handle_block(pair)),
      Rule::ExprWithBlock => else_ = Some(handle_expr_with_block(pair)),
      _ => unreachable!(),
    }
  }

  return Expr::mk_if(expr.unwrap(), block.unwrap(), else_);
}

fn handle_if_let(pair: Pair<Rule>) -> Expr {
  let mut pat = None;
  let mut expr = None;
  let mut block = None;
  let mut else_ = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Pattern => pat = Some(handle_pattern(pair)),
      Rule::Expr => expr = Some(handle_expr(pair)),
      Rule::Block => block = Some(handle_block(pair)),
      Rule::ExprWithBlock => else_ = Some(handle_expr_with_block(pair)),
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

fn handle_match(pair: Pair<Rule>) -> Expr {
  let mut pairs = pair.into_inner();
  let expr = handle_expr(pairs.next().unwrap());
  let arms = pairs.map(handle_match_arm).collect();
  return Expr::mk_match(expr, arms);
}

fn handle_match_arm(pair: Pair<Rule>) -> MatchArm {
  let mut pairs = pair.into_inner();
  let pat = handle_pattern(pairs.next().unwrap());
  let expr = handle_expr(pairs.next().unwrap());
  return MatchArm::new(pat, expr);
}

fn handle_block(pair: Pair<Rule>) -> Block {
  let pairs = pair.into_inner();
  let mut label = None;
  let mut stmts = Vec::new();
  let mut comptime = false;

  for pair in pairs {
    match pair.as_rule() {
      Rule::BlockSpec => comptime = true,
      Rule::Label => label = Some(handle_label(pair)),
      Rule::Stmt => stmts.push(handle_stmt(pair)),
      _ => unreachable!(),
    }
  }

  return Block::new(label, stmts, comptime);
}

fn handle_fn_ty(pair: Pair<Rule>) -> Expr {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  let mut ret_ty = None;

  for pair in pairs {
    match pair.as_rule() {
      Rule::FnTyParamsImplicit => {
        params.extend(handle_fn_ty_params(pair, true));
      }
      Rule::FnTyParamsExplicit => {
        params.extend(handle_fn_ty_params(pair, false));
      }
      Rule::QualifiedExpr => {
        ret_ty = Some(handle_qualified_expr(pair));
      }
      _ => unreachable!(),
    }
  }

  let expr = Expr::mk_fn_ty(params, ret_ty.unwrap());
  return expr;
}

fn handle_fn_ty_params(pair: Pair<Rule>, implicit: bool) -> Vec<FnParam> {
  let pairs = pair.into_inner();
  let mut params = Vec::new();
  for pair in pairs {
    // `Ident?` and `QualifiedPath`
    let pairs = pair.into_inner();
    // Optional name of the parameter
    let mut name = None;
    // Optional type of the parameter
    let mut ty = None;
    // Traverse the `Ident?` and `QualifiedPath`
    for pair in pairs {
      match pair.as_rule() {
        Rule::Ident => name = Some(pair.as_str().to_string()),
        Rule::QualifiedExpr => ty = Some(handle_qualified_expr(pair)),
        _ => unreachable!(),
      }
    }
    params.push(FnParam::new(name, ty, implicit));
  }
  return params;
}

fn handle_tuple(pair: Pair<Rule>) -> Expr {
  Expr::mk_tuple(pair.into_inner().map(handle_expr).collect())
}

fn handle_fn_args(pair: Pair<Rule>) -> Vec<FnArg> {
  let pair = pair.into_inner().next().unwrap();
  let implicit = if let Rule::FnArgsImplicit = pair.as_rule() {
    true
  } else {
    false
  };

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
        Rule::Expr => expr = Some(handle_expr(pair)),
        _ => unreachable!(),
      }
    }
    args.push(FnArg::new(name, expr.unwrap(), implicit));
  }
  return args;
}

fn handle_pattern(pair: Pair<Rule>) -> Pat {
  let pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
  if pairs.len() == 1 {
    return handle_primary_pattern(pairs[0].clone());
  } else {
    return Pat::mk_or(pairs.into_iter().map(handle_primary_pattern).collect());
  }
}

fn handle_primary_pattern(pair: Pair<Rule>) -> Pat {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::RangePattern => handle_range_pattern(pair),
    Rule::PatternWithoutRange => handle_pattern_without_range(pair),
    _ => unreachable!(),
  }
}

fn handle_range_pattern(pair: Pair<Rule>) -> Pat {
  let span = pair.as_span();
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
      let lhs = handle_range_pattern_bound(pairs.next().unwrap());
      let rhs = handle_range_pattern_bound(pairs.next().unwrap());
      Pat::mk_range(kind, lhs, rhs)
    }
    Rule::RangePatternFrom => {
      let bound = handle_range_pattern_bound(pair.into_inner().next().unwrap());
      Pat::mk_range(kind, bound, None)
    }
    Rule::RangePatternTo | Rule::RangePatternToInclusive => {
      let bound = handle_range_pattern_bound(pair.into_inner().next().unwrap());
      Pat::mk_range(kind, None, bound)
    }
    _ => unreachable!(),
  };
  pat.set_span(span);
  return pat;
}

fn handle_range_pattern_bound(pair: Pair<Rule>) -> Option<RangePatBound> {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Literal => Some(RangePatBound::Lit(handle_literal(pair))),
    Rule::QualifiedPath => {
      Some(RangePatBound::QualifiedPath(handle_qualified_path(pair)))
    }
    _ => unreachable!(),
  }
}

fn handle_pattern_without_range(pair: Pair<Rule>) -> Pat {
  let span = pair.as_span();
  let pair = pair.into_inner().next().unwrap();

  let mut pat = match pair.as_rule() {
    Rule::LiteralPattern => handle_literal_pattern(pair),
    Rule::IdentPattern => handle_ident_pattern(pair),
    Rule::WildcardPattern => Pat::mk_wildcard(),
    Rule::RestPattern => Pat::mk_rest(),
    Rule::RecordPattern => handle_record_pattern(pair),
    Rule::TuplePattern => handle_tuple_pattern(pair),
    Rule::QualifiedPattern => Pat::mk_qualified(handle_qualified_path(
      pair.into_inner().next().unwrap(),
    )),
    Rule::Pattern => handle_pattern(pair),
    Rule::ConstructorPattern => handle_constructor_pattern(pair),
    _ => unreachable!(),
  };

  pat.set_span(span);
  return pat;
}

fn handle_constructor_pattern(pair: Pair<Rule>) -> Pat {
  let mut pairs = pair.into_inner();
  let path = handle_qualified_path(pairs.next().unwrap());

  let mut args = Vec::new();
  for pair in pairs {
    match pair.as_rule() {
      Rule::ConstructorPatternArgsImplicit => {
        for pair in pair.into_inner() {
          args.push(handle_constructor_pattern_arg(pair, true));
        }
      }
      Rule::ConstructorPatternArgsExplicit => {
        for pair in pair.into_inner() {
          args.push(handle_constructor_pattern_arg(pair, false));
        }
      }
      _ => unreachable!(),
    }
  }
  return Pat::mk_constructor(path, args);
}

fn handle_constructor_pattern_arg(
  pair: Pair<Rule>,
  implicit: bool,
) -> ConstructorPatArg {
  let mut name = None;
  let mut pat = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::Ident => name = Some(pair.as_str().to_string()),
      Rule::Pattern => pat = Some(handle_pattern(pair)),
      _ => unreachable!(),
    }
  }

  return ConstructorPatArg::new(name, pat.unwrap(), implicit);
}

fn handle_tuple_pattern(pair: Pair<Rule>) -> Pat {
  Pat::mk_tuple(pair.into_inner().map(handle_pattern).collect())
}

fn handle_record_pattern(pair: Pair<Rule>) -> Pat {
  let mut pairs = pair.into_inner();
  let mut elems = Vec::new();

  let path = handle_qualified_path(pairs.next().unwrap());
  for pair in pairs {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
      Rule::RestPattern => elems.push(RecordPatElem::Rest),
      Rule::RecordPatternField => {
        let mut pairs = pair.into_inner();
        let name = pairs.next().unwrap().as_str().to_string();
        let pat = handle_pattern(pairs.next().unwrap());
        elems.push(RecordPatElem::Field(name, pat));
      }
      _ => unreachable!(),
    }
  }

  return Pat::mk_record(path, elems);
}

fn handle_literal_pattern(pair: Pair<Rule>) -> Pat {
  Pat::mk_lit(handle_literal(pair.into_inner().next().unwrap()))
}

fn handle_ident_pattern(pair: Pair<Rule>) -> Pat {
  let mut name = String::new();
  let mut spec = IdentPatSpec::None;
  let pairs = pair.into_inner();
  for pair in pairs {
    match pair.as_rule() {
      Rule::IdentPatternSpec => spec = hanlde_ident_pattern_spec(pair),
      Rule::Ident => name = pair.as_str().to_string(),
      _ => unreachable!(),
    }
  }

  return Pat::mk_ident(spec, name);
}

fn hanlde_ident_pattern_spec(pair: Pair<Rule>) -> IdentPatSpec {
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

fn handle_stmt(pair: Pair<Rule>) -> Stmt {
  let span = pair.as_span();
  let pair = pair.into_inner().next().unwrap();
  let mut stmt = match pair.as_rule() {
    Rule::Let => handle_let(pair),
    Rule::Item => Stmt::mk_item(handle_item(pair)),
    Rule::Expr => Stmt::mk_expr(handle_expr(pair)),
    _ => unreachable!(),
  };
  stmt.set_span(span);
  return stmt;
}

fn handle_let(pair: Pair<Rule>) -> Stmt {
  let mut pairs = pair.into_inner();
  let pat = handle_pattern_without_range(pairs.next().unwrap());
  let mut init = None;
  let mut ty = None;
  for pair in pairs {
    match pair.as_rule() {
      Rule::QualifiedExpr => {
        ty = Some(handle_qualified_expr(pair));
      }
      Rule::Expr => {
        init = Some(handle_expr(pair));
      }
      _ => unreachable!(),
    }
  }

  return Stmt::mk_let(Let::new(pat, ty, init));
}

fn handle_def(pair: Pair<Rule>) -> Item {
  let mut builtin = false;
  let mut name = None;
  let mut func = None;
  let mut expr = None;

  for pair in pair.into_inner() {
    match pair.as_rule() {
      Rule::DefSpec => builtin = true,
      Rule::Ident => name = Some(pair.as_str().to_string()),
      Rule::FnLhs => func = Some(handle_fn_lhs(pair)),
      Rule::Expr => expr = Some(handle_expr(pair)),
      _ => unreachable!(),
    }
  }

  let mut func = func.unwrap();
  func.name = Some(name.unwrap());

  func.expr = expr.map(Box::new);

  return Item::mk_def(Def::new(builtin, func));
}

fn handle_item(pair: Pair<Rule>) -> Item {
  let span = pair.as_span();
  let pair = pair.into_inner().next().unwrap();
  let mut item = match pair.as_rule() {
    Rule::Use => handle_use(pair),
    Rule::Import => handle_import(pair),
    Rule::Def => handle_def(pair),
    Rule::Const => handle_const(pair),
    Rule::Type => handle_type(pair),
    Rule::Impl => hanlde_impl(pair),
    Rule::Module => handle_module(pair),
    _ => unreachable!(),
  };
  item.set_span(span);
  return item;
}

fn handle_use(pair: Pair<Rule>) -> Item {
  Item::mk_use(handle_use_tree(pair.into_inner().next().unwrap()))
}

fn handle_use_tree(pair: Pair<Rule>) -> UseTree {
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
      let children = pairs.map(handle_use_tree).collect();
      UseTree::new(name, None, false, children)
    }
    Rule::UseTreeSimple => {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let children = pairs.map(handle_use_tree).collect();
      UseTree::new(name, None, false, children)
    }
    _ => unreachable!(),
  }
}

fn handle_import(pair: Pair<Rule>) -> Item {
  Item::mk_import(pair.into_inner().next().unwrap().as_str().to_string())
}

fn handle_const(pair: Pair<Rule>) -> Item {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let ty = handle_qualified_expr(pairs.next().unwrap());
  let init = handle_expr(pairs.next().unwrap());
  return Item::mk_const(Const::new(name, ty, init));
}

fn handle_type(pair: Pair<Rule>) -> Item {
  let pairs = pair.into_inner();
  let mut builtin = false;
  let mut name = String::new();
  let mut ty = None;
  let mut body = None;

  for pair in pairs {
    match pair.as_rule() {
      Rule::TypeSpec => builtin = true,
      Rule::Ident => name = pair.as_str().to_string(),
      Rule::FnLhs => ty = Some(handle_fn_lhs(pair)),
      Rule::TypeBody => body = Some(handle_type_body(pair)),
      _ => unreachable!(),
    }
  }

  return Item::mk_type(Type::new(builtin, name, ty, body));
}

fn handle_type_body(pair: Pair<Rule>) -> TypeBody {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::Record => handle_record(pair),
    Rule::Expr => TypeBody::Expr(handle_expr(pair)),
    Rule::Interface => handle_interface(pair),
    Rule::Constructors => handle_constructors(pair),
    _ => unreachable!(),
  }
}

fn handle_constructors(pair: Pair<Rule>) -> TypeBody {
  let pairs = pair.into_inner();
  let constructors = pairs
    .map(|pair| {
      let mut pairs = pair.into_inner();
      let name = pairs.next().unwrap().as_str().to_string();
      let maybe_pair = pairs.next();
      let func = if let Some(pair) = maybe_pair {
        Some(handle_fn_lhs(pair))
      } else {
        None
      };
      (name, func)
    })
    .collect();
  return TypeBody::Constructors(constructors);
}

fn handle_interface(pair: Pair<Rule>) -> TypeBody {
  let pairs = pair.into_inner();
  let items = pairs.map(handle_def).collect();
  return TypeBody::Interface(items);
}

fn handle_record(pair: Pair<Rule>) -> TypeBody {
  let pairs = pair.into_inner();
  let mut fields = Vec::new();

  for pair in pairs {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_string();
    let expr = handle_expr(pairs.next().unwrap());
    fields.push((name, expr));
  }
  return TypeBody::Record(fields);
}

fn hanlde_impl(pair: Pair<Rule>) -> Item {
  let mut pairs = pair.into_inner();
  let expr = handle_atomic_expr(pairs.next().unwrap());
  let items = pairs.map(handle_def).collect();
  return Item::mk_impl(expr, items);
}

fn handle_module(pair: Pair<Rule>) -> Item {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let items = pairs.map(handle_item).collect();
  return Item::mk_module(Module::new(name, items));
}
