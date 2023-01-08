use crate::define::ast::*;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "front/grammar.pest"]
struct OriaParser;

pub fn generate_parse_tree(source: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    OriaParser::parse(Rule::compunit, source)
}

pub fn handle_compunit(source: &str) -> Result<CompUnit, Error<Rule>> {
    let pairs = OriaParser::parse(Rule::compunit, source)?
        .next()
        .unwrap()
        .into_inner();

    let mut items = Vec::new();
    for pair in pairs {
        match pair.as_rule() {
            Rule::item => items.push(handle_item(pair)),
            Rule::EOI => {}
            _ => {
                println!("Unexpected rule: {:?}", pair.as_rule());
            }
        }
    }
    Ok(CompUnit::new(items))
}

pub fn handle_expr(pair: Pair<Rule>) -> Expr {
    let rule = pair.into_inner().next().unwrap();
    match rule.as_rule() {
        Rule::op_expr => handle_op_expr(rule.into_inner()),
        _ => unreachable!(),
    }
}

pub fn handle_op_expr(pairs: Pairs<Rule>) -> Expr {
    use pest::pratt_parser::Assoc::*;
    use pest::pratt_parser::Op;
    use pest::pratt_parser::PrattParser;

    let parser = PrattParser::new()
        .op(Op::infix(Rule::or, Left))
        .op(Op::infix(Rule::and, Left))
        .op(Op::infix(Rule::eq, Left)
            | Op::infix(Rule::ne, Left)
            | Op::infix(Rule::lt, Left)
            | Op::infix(Rule::le, Left)
            | Op::infix(Rule::gt, Left)
            | Op::infix(Rule::ge, Left))
        .op(Op::infix(Rule::pipe, Left))
        .op(Op::infix(Rule::bit_or, Left))
        .op(Op::infix(Rule::bit_xor, Left))
        .op(Op::infix(Rule::bit_and, Left))
        .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
        .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left) | Op::infix(Rule::rem, Left))
        .op(Op::infix(Rule::frac, Left))
        .op(Op::infix(Rule::shl, Right) | Op::infix(Rule::shr, Right))
        .op(Op::prefix(Rule::pos)
            | Op::prefix(Rule::neg)
            | Op::prefix(Rule::not)
            | Op::prefix(Rule::bit_not)
            | Op::prefix(Rule::ref_op)
            | Op::prefix(Rule::deref_op))
        .op(Op::infix(Rule::pow, Right));

    parser
        .map_primary(handle_app_expr)
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::pos => Expr::mk_unary(UnaryOp::Pos, Box::new(rhs)),
            Rule::neg => Expr::mk_unary(UnaryOp::Neg, Box::new(rhs)),
            Rule::not => Expr::mk_unary(UnaryOp::Not, Box::new(rhs)),
            Rule::bit_not => Expr::mk_unary(UnaryOp::BitNot, Box::new(rhs)),
            Rule::ref_op => Expr::mk_unary(UnaryOp::Ref, Box::new(rhs)),
            Rule::deref_op => Expr::mk_unary(UnaryOp::Deref, Box::new(rhs)),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::or => Expr::mk_binary(BinOp::Or, Box::new(lhs), Box::new(rhs)),
            Rule::and => Expr::mk_binary(BinOp::And, Box::new(lhs), Box::new(rhs)),
            Rule::eq => Expr::mk_binary(BinOp::Eq, Box::new(lhs), Box::new(rhs)),
            Rule::ne => Expr::mk_binary(BinOp::Ne, Box::new(lhs), Box::new(rhs)),
            Rule::lt => Expr::mk_binary(BinOp::Lt, Box::new(lhs), Box::new(rhs)),
            Rule::le => Expr::mk_binary(BinOp::Le, Box::new(lhs), Box::new(rhs)),
            Rule::gt => Expr::mk_binary(BinOp::Gt, Box::new(lhs), Box::new(rhs)),
            Rule::ge => Expr::mk_binary(BinOp::Ge, Box::new(lhs), Box::new(rhs)),
            Rule::pipe => Expr::mk_binary(BinOp::Pipe, Box::new(lhs), Box::new(rhs)),
            Rule::bit_or => Expr::mk_binary(BinOp::BitOr, Box::new(lhs), Box::new(rhs)),
            Rule::bit_xor => Expr::mk_binary(BinOp::BitXor, Box::new(lhs), Box::new(rhs)),
            Rule::bit_and => Expr::mk_binary(BinOp::BitAnd, Box::new(lhs), Box::new(rhs)),
            Rule::add => Expr::mk_binary(BinOp::Add, Box::new(lhs), Box::new(rhs)),
            Rule::sub => Expr::mk_binary(BinOp::Sub, Box::new(lhs), Box::new(rhs)),
            Rule::mul => Expr::mk_binary(BinOp::Mul, Box::new(lhs), Box::new(rhs)),
            Rule::div => Expr::mk_binary(BinOp::Div, Box::new(lhs), Box::new(rhs)),
            Rule::rem => Expr::mk_binary(BinOp::Rem, Box::new(lhs), Box::new(rhs)),
            Rule::frac => Expr::mk_binary(BinOp::Frac, Box::new(lhs), Box::new(rhs)),
            Rule::shl => Expr::mk_binary(BinOp::Shl, Box::new(lhs), Box::new(rhs)),
            Rule::shr => Expr::mk_binary(BinOp::Shr, Box::new(lhs), Box::new(rhs)),
            Rule::pow => Expr::mk_binary(BinOp::Pow, Box::new(lhs), Box::new(rhs)),
            Rule::as_ => Expr::mk_binary(BinOp::As, Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(pairs)
}

pub fn handle_fn_ty(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let fn_ty_params_pair = pairs.next().unwrap();
    let params = handle_fn_ty_params(fn_ty_params_pair);
    let rhs_pair = pairs.next().unwrap();
    let rhs = handle_app_expr(rhs_pair);

    Expr::mk_fn_ty(params, Box::new(rhs))
}

pub fn handle_fn_ty_params(pair: Pair<Rule>) -> Vec<Field> {
    let mut pairs = pair.into_inner();
    let mut params = Vec::new();
    while let Some(p) = pairs.next() {
        match p.as_rule() {
            Rule::fn_ty_params_explicit => {
                params.extend(handle_fn_ty_params_explicit(p));
            }
            Rule::fn_ty_params_implicit => {
                params.extend(handle_fn_ty_params_implicit(p));
            }
            _ => unreachable!(),
        }
    }
    params
}

pub fn handle_fn_ty_params_explicit(pair: Pair<Rule>) -> Vec<Field> {
    pair.into_inner().map(handle_field).collect()
}

pub fn handle_fn_ty_params_implicit(pair: Pair<Rule>) -> Vec<Field> {
    pair.into_inner()
        .map(|p| {
            let mut field = handle_field(p);
            field.implicit = true;
            field
        })
        .collect()
}

pub fn handle_spec(pair: Pair<Rule>) -> Spec {
    let spec = pair.as_str();
    match spec {
        "mut" => Spec::Mut,
        "extern" => Spec::Extern,
        "builtin" => Spec::Builtin,
        "comptime" => Spec::Comptime,
        _ => unreachable!(),
    }
}

pub fn handle_field(pair: Pair<Rule>) -> Field {
    let pairs = pair.into_inner();

    let mut spec = Spec::None;
    let mut ident = None;
    let mut expr = None;
    let mut with = Vec::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::spec => spec = handle_spec(pair),
            Rule::ident => ident = Some(pair.as_str().to_string()),
            Rule::app_expr => expr = Some(handle_app_expr(pair)),
            Rule::with_clause => with = handle_with_clause(pair),
            _ => unreachable!(),
        }
    }

    Field::new(spec, ident, expr, with)
}

pub fn handle_with_clause(pair: Pair<Rule>) -> Vec<String> {
    pair.into_inner()
        .map(|pair| pair.as_str().to_string())
        .collect()
}

pub fn handle_app_expr(pair: Pair<Rule>) -> Expr {
    let pairs = pair.into_inner().collect::<Vec<_>>();
    let primary = handle_primary_expr(pairs[0].clone());
    if pairs.len() == 1 {
        primary
    } else {
        let mut expr = primary;
        for pair in pairs[1..].iter() {
            expr = Expr::mk_apply(Box::new(expr), handle_app_args(pair.clone()));
        }
        expr
    }
}

pub fn handle_app_args(pair: Pair<Rule>) -> Vec<FieldInit> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::app_args_explicit => handle_app_args_explicit(pair),
        Rule::app_args_implicit => handle_app_args_implicit(pair),
        _ => unreachable!(),
    }
}

pub fn handle_app_args_explicit(pair: Pair<Rule>) -> Vec<FieldInit> {
    let mut args = Vec::new();
    for pair in pair.into_inner() {
        args.push(handle_app_arg(pair));
    }
    args
}

pub fn handle_app_args_implicit(pair: Pair<Rule>) -> Vec<FieldInit> {
    let mut args = Vec::new();
    for pair in pair.into_inner() {
        let mut arg = handle_app_arg(pair);
        arg.implicit = true;
        args.push(arg);
    }
    args
}

pub fn handle_app_arg(pair: Pair<Rule>) -> FieldInit {
    let rule = pair.into_inner().next().unwrap();
    match rule.as_rule() {
        Rule::app_arg_named => {
            let pairs = rule.into_inner().collect::<Vec<_>>();
            let name = pairs[0].as_str().to_string();
            let expr = handle_app_expr(pairs[1].clone());
            FieldInit::new(Some(name), expr)
        }
        Rule::app_expr => FieldInit::new(None, handle_app_expr(rule)),
        _ => unreachable!(),
    }
}

pub fn handle_primary_expr(pair: Pair<Rule>) -> Expr {
    let pairs = pair.into_inner().collect::<Vec<_>>();
    if pairs.len() == 1 {
        match pairs[0].as_rule() {
            Rule::fn_expr => handle_fn_expr(pairs[0].clone()),
            Rule::atomic_expr => handle_atomic_expr(pairs[0].clone()),
            _ => unreachable!(),
        }
    } else {
        let mut expr = handle_atomic_expr(pairs[0].clone());
        for pair in pairs[1..].iter() {
            let rhs = handle_atomic_expr(pair.clone());
            expr = Expr::mk_dot(Box::new(expr), Box::new(rhs));
        }
        expr
    }
}

pub fn handle_fn_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let (params, ret) = handle_fn_lhs(pairs.next().unwrap());
    let body = handle_expr(pairs.next().unwrap());
    Expr::mk_fn(params, ret, Box::new(body))
}

pub fn handle_fn_lhs(pair: Pair<Rule>) -> (Vec<Field>, Option<Box<Expr>>) {
    let mut pairs = pair.into_inner();
    let fn_params_pair = pairs.next().unwrap();
    let params = handle_fn_params(fn_params_pair);

    let maybe_expr_pair = pairs.next();

    match maybe_expr_pair {
        Some(pair) => {
            let expr = handle_expr(pair);
            (params, Some(Box::new(expr)))
        }
        None => (params, None),
    }
}

pub fn handle_fn_params(pair: Pair<Rule>) -> Vec<Field> {
    let mut pairs = pair.into_inner();
    let mut params = Vec::new();
    while let Some(p) = pairs.next() {
        match p.as_rule() {
            Rule::fn_params_explicit => {
                params.extend(handle_fn_ty_params_explicit(p));
            }
            Rule::fn_params_implicit => {
                params.extend(handle_fn_ty_params_implicit(p));
            }
            _ => unreachable!(),
        }
    }
    params
}

pub fn handle_fn_params_explicit(pair: Pair<Rule>) -> Vec<Field> {
    pair.into_inner().map(handle_field).collect()
}

pub fn handle_fn_params_implicit(pair: Pair<Rule>) -> Vec<Field> {
    pair.into_inner()
        .map(|p| {
            let mut field = handle_field(p);
            field.implicit = true;
            field
        })
        .collect()
}

pub fn handle_atomic_expr(pair: Pair<Rule>) -> Expr {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::universe => handle_universe(pair),
        Rule::unit => Expr::mk_unit(),
        Rule::literal => Expr::mk_literal(handle_literal(pair)),
        Rule::match_expr => handle_match_expr(pair),
        Rule::loop_expr => handle_loop_expr(pair),
        Rule::while_expr => handle_while_expr(pair),
        Rule::if_expr => handle_if_expr(pair),
        Rule::record_init => handle_record_init(pair),
        Rule::chained => Expr::mk_chained(handle_chained(pair)),
        Rule::tuple => handle_tuple(pair),
        Rule::expr => handle_expr(pair),
        Rule::block => Expr::mk_block(handle_block(pair)),
        Rule::fn_ty => handle_fn_ty(pair),
        _ => unreachable!(),
    }
}

pub fn handle_universe(pair: Pair<Rule>) -> Expr {
    let maybe_level_pair = pair.into_inner().next();
    match maybe_level_pair {
        Some(pair) => Expr::mk_type(pair.as_str().parse().unwrap()),
        None => Expr::mk_type(0),
    }
}

pub fn handle_literal(pair: Pair<Rule>) -> Lit {
    let pair = pair.into_inner().next().unwrap();

    let kind = match pair.as_rule() {
        Rule::floating => LitKind::Floating,
        Rule::hex_integer | Rule::oct_integer | Rule::bin_integer | Rule::dec_integer => {
            LitKind::Integer
        }
        _ => unreachable!(),
    };

    let radix = match pair.as_rule() {
        Rule::floating => Radix::Dec,
        Rule::hex_integer => Radix::Hex,
        Rule::oct_integer => Radix::Oct,
        Rule::bin_integer => Radix::Bin,
        Rule::dec_integer => Radix::Dec,
        _ => unreachable!(),
    };

    let pairs = pair.into_inner();

    let mut suffix = None;
    let mut text = String::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::literal_suffix => suffix = Some(handle_literal_suffix(pair)),
            Rule::hex_prefix | Rule::oct_prefix | Rule::bin_prefix => {}
            _ => text.push_str(pair.as_str()),
        }
    }

    text = text.replace("_", "");

    Lit::new(kind, radix, text, suffix)
}

pub fn handle_literal_suffix(pair: Pair<Rule>) -> Suffix {
    match pair.as_str() {
        "u8" => Suffix::U(8),
        "u16" => Suffix::U(16),
        "u32" => Suffix::U(32),
        "u64" => Suffix::U(64),
        "i8" => Suffix::I(8),
        "i16" => Suffix::I(16),
        "i32" => Suffix::I(32),
        "i64" => Suffix::I(64),
        "f32" => Suffix::F(32),
        "f64" => Suffix::F(64),
        "usize" => Suffix::USize,
        "isize" => Suffix::ISize,
        _ => unreachable!(),
    }
}

pub fn handle_match_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let expr = handle_expr(pairs.next().unwrap());
    let mut arms = Vec::new();
    for pair in pairs {
        let mut arm_pair = pair.into_inner();
        let pattern = handle_pattern(arm_pair.next().unwrap());
        let expr = handle_expr(arm_pair.next().unwrap());
        arms.push((pattern, expr));
    }
    Expr::mk_match(Box::new(expr), arms)
}

pub fn handle_pattern(pair: Pair<Rule>) -> Pattern {
    let mut pairs = pair.into_inner().collect::<Vec<_>>();
    if pairs.len() == 1 {
        handle_pattern_elem(pairs.pop().unwrap())
    } else {
        let mut patterns = Vec::new();
        for pair in pairs {
            patterns.push(handle_pattern_elem(pair));
        }
        Pattern::Or(patterns)
    }
}

pub fn handle_pattern_elem(pair: Pair<Rule>) -> Pattern {
    let rule = pair.into_inner().next().unwrap();
    match rule.as_rule() {
        Rule::wildcard => Pattern::Wildcard,
        Rule::rest => Pattern::Rest,
        Rule::range_pattern => handle_range_pattern(rule),
        Rule::constructor_pattern => handle_constructor_pattern(rule),
        Rule::literal => Pattern::Literal(handle_literal(rule)),
        Rule::record_pattern => handle_record_pattern(rule),
        Rule::tuple_pattern => handle_tuple_pattern(rule),
        Rule::pattern => handle_pattern(rule),
        _ => unreachable!(),
    }
}

pub fn handle_range_pattern(pair: Pair<Rule>) -> Pattern {
    let rule = pair.into_inner().next().unwrap();
    let kind = match rule.as_rule() {
        Rule::range_pattern_inclusive => RangeKind::Inclusive,
        Rule::range_pattern_exclusive => RangeKind::Exclusive,
        _ => unreachable!(),
    };
    let mut bounds = rule.into_inner();

    let lhs_pair = bounds.next().unwrap().into_inner().next().unwrap();
    let rhs_pair = bounds.next().unwrap().into_inner().next().unwrap();

    let lhs = match lhs_pair.as_rule() {
        Rule::literal => RangeBound::Literal(handle_literal(lhs_pair)),
        Rule::chained => RangeBound::Chained(handle_chained(lhs_pair)),
        _ => unreachable!(),
    };
    let rhs = match rhs_pair.as_rule() {
        Rule::literal => RangeBound::Literal(handle_literal(rhs_pair)),
        Rule::chained => RangeBound::Chained(handle_chained(rhs_pair)),
        _ => unreachable!(),
    };

    Pattern::Range(kind, lhs, rhs)
}

pub fn handle_constructor_pattern(pair: Pair<Rule>) -> Pattern {
    let pairs = pair.into_inner().collect::<Vec<_>>();

    let chained = handle_chained(pairs[0].clone());

    let mut patterns = Vec::new();

    for pair in pairs.into_iter().skip(1) {
        patterns.push(handle_pattern(pair));
    }

    Pattern::Constructor(chained, patterns)
}

pub fn handle_record_pattern(pair: Pair<Rule>) -> Pattern {
    let pairs = pair.into_inner();
    let mut elems = Vec::new();
    for elem_pair in pairs {
        let pair = elem_pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::record_pattern_field => {
                let mut pairs = pair.into_inner();
                let ident = pairs.next().unwrap().as_str().to_string();
                let pattern = handle_pattern(pairs.next().unwrap());
                elems.push(RecordPatternElem::Field(ident, pattern));
            }
            Rule::rest => elems.push(RecordPatternElem::Rest),
            _ => unreachable!(),
        }
    }
    Pattern::Record(elems)
}

pub fn handle_tuple_pattern(pair: Pair<Rule>) -> Pattern {
    let pairs = pair.into_inner().collect::<Vec<_>>();
    let mut patterns = Vec::new();
    for pair in pairs {
        patterns.push(handle_pattern_elem(pair));
    }
    Pattern::Tuple(patterns)
}

pub fn handle_loop_expr(pair: Pair<Rule>) -> Expr {
    Expr::mk_loop(handle_block(pair.into_inner().next().unwrap()))
}

pub fn handle_while_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let expr = handle_expr(pairs.next().unwrap());
    let block = handle_block(pairs.next().unwrap());
    Expr::mk_while(Box::new(expr), block)
}

pub fn handle_if_expr(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let expr = handle_expr(pairs.next().unwrap());
    let block = handle_block(pairs.next().unwrap());
    let else_ = match pairs.next() {
        Some(pair) => Some(Box::new({
            match pair.as_rule() {
                Rule::block => Expr::mk_block(handle_block(pair)),
                Rule::if_expr => handle_if_expr(pair),
                _ => unreachable!(),
            }
        })),
        None => None,
    };
    Expr::mk_if(Box::new(expr), block, else_)
}

pub fn handle_record_init(pair: Pair<Rule>) -> Expr {
    let mut pairs = pair.into_inner();
    let chained = handle_chained(pairs.next().unwrap());
    let mut elems = Vec::new();
    for field_pair in pairs {
        let mut field_pairs = field_pair.into_inner();
        let ident = field_pairs.next().unwrap().as_str().to_string();
        let expr = handle_expr(field_pairs.next().unwrap());
        elems.push((ident, expr));
    }
    Expr::mk_record_init(chained, elems)
}

pub fn handle_chained(pair: Pair<Rule>) -> Chained {
    pair.into_inner()
        .map(|pair| pair.as_span().as_str().to_string())
        .collect()
}

pub fn handle_tuple(pair: Pair<Rule>) -> Expr {
    let exprs = pair.into_inner().map(handle_expr).collect();
    Expr::mk_tuple(exprs)
}

pub fn handle_field_explicit_init(pair: Pair<Rule>) -> FieldInit {
    let mut pairs = pair.into_inner();
    let ident = Some(pairs.next().unwrap().as_str().to_string());
    let expr = handle_expr(pairs.next().unwrap());
    FieldInit::new(ident, expr)
}

pub fn handle_block(pair: Pair<Rule>) -> Block {
    let pairs = pair.into_inner();
    let stmts = pairs.map(handle_stmt).collect();
    Block::new(stmts)
}

pub fn handle_stmt(pair: Pair<Rule>) -> Stmt {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::item => Stmt::Item(handle_item(pair)),
        Rule::assign => handle_assign(pair),
        Rule::expr => Stmt::Expr(handle_expr(pair)),
        _ => unreachable!(),
    }
}

pub fn handle_assign(pair: Pair<Rule>) -> Stmt {
    let mut pairs = pair.into_inner();
    let lhs = handle_expr(pairs.next().unwrap());
    let rhs = handle_expr(pairs.next().unwrap());
    Stmt::Assign(lhs, rhs)
}

pub fn handle_item(pair: Pair<Rule>) -> Item {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::use_decl => handle_use_decl(pair),
        Rule::type_decl => handle_type_decl(pair),
        Rule::let_decl => handle_let_decl(pair),
        Rule::module => handle_module(pair),
        Rule::import => handle_import(pair),
        Rule::trait_decl => handle_trait_decl(pair),
        Rule::instance => handle_instance(pair),
        _ => unreachable!(),
    }
}

pub fn handle_trait_decl(pair: Pair<Rule>) -> Item {
    let mut pairs = pair.into_inner();
    let ident = pairs.next().unwrap().as_str().to_string();
    let params = handle_fn_ty_params(pairs.next().unwrap());
    let items = pairs.map(handle_item).collect();
    Item::mk_trait(ident, params, items)
}

pub fn handle_instance_item(pair: Pair<Rule>) -> (String, Expr) {
    let mut pairs = pair.into_inner();
    let ident = pairs.next().unwrap().as_str().to_string();
    let expr = handle_expr(pairs.next().unwrap());
    (ident, expr)
}

pub fn handle_instance(pair: Pair<Rule>) -> Item {
    let mut pairs = pair.into_inner();
    let ident = pairs.next().unwrap().as_str().to_string();
    let params = handle_fn_params(pairs.next().unwrap());
    let items = pairs.map(handle_instance_item).collect();
    Item::mk_instance(ident, params, items)
}

pub fn handle_let_decl(pair: Pair<Rule>) -> Item {
    let pairs = pair.into_inner();
    let mut spec = Spec::None;
    let mut ident = String::new();
    let mut ty = None;
    let mut expr = None;

    for pair in pairs {
        match pair.as_rule() {
            Rule::spec => spec = handle_spec(pair),
            Rule::ident => ident = pair.as_str().to_string(),
            Rule::let_ty => {
                ty = Some(handle_expr(pair.into_inner().next().unwrap()));
            }
            Rule::let_init => {
                expr = Some(handle_expr(pair.into_inner().next().unwrap()));
            }
            _ => unreachable!(),
        }
    }
    Item::mk_let(spec, ident, ty, expr)
}

pub fn handle_use_decl(pair: Pair<Rule>) -> Item {
    let tree = handle_use_tree(pair.into_inner().next().unwrap());
    Item::mk_use(tree)
}

pub fn handle_use_tree(pair: Pair<Rule>) -> UseTree {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::chained => UseTree::new(handle_chained(pair), None, Vec::new(), false),
        Rule::use_as => handle_use_as(pair),
        Rule::use_all => handle_use_all(pair),
        Rule::use_multi => handle_use_multi(pair),
        _ => unreachable!(),
    }
}

pub fn handle_use_as(pair: Pair<Rule>) -> UseTree {
    let mut pairs = pair.into_inner();
    let chained = handle_chained(pairs.next().unwrap());
    let alias = pairs.next().unwrap().as_str().to_string();
    UseTree::new(chained, Some(alias), Vec::new(), false)
}

pub fn handle_use_all(pair: Pair<Rule>) -> UseTree {
    let chained = handle_chained(pair.into_inner().next().unwrap());
    UseTree::new(chained, None, Vec::new(), true)
}

pub fn handle_use_multi(pair: Pair<Rule>) -> UseTree {
    let mut pairs = pair.into_inner();
    let chained = handle_chained(pairs.next().unwrap());
    let children = pairs.map(handle_use_tree).collect();
    UseTree::new(chained, None, children, false)
}

pub fn handle_type_decl(pair: Pair<Rule>) -> Item {
    let mut pairs = pair.into_inner();
    let mut spec = Spec::None;
    let mut pair = pairs.next().unwrap();

    match pair.as_rule() {
        Rule::spec => {
            spec = handle_spec(pair);
            pair = pairs.next().unwrap();
        }
        _ => {}
    }

    let ident = pair.as_str().to_string();

    let mut params = Vec::new();
    let mut ty = None;
    let mut body = None;

    for pair in pairs {
        match pair.as_rule() {
            Rule::fn_ty_params => {
                params = handle_fn_ty_params(pair);
            }
            Rule::app_expr => {
                ty = Some(handle_app_expr(pair));
            }
            Rule::type_decl_body => {
                body = Some(handle_type_decl_body(pair));
            }
            _ => unreachable!(),
        }
    }

    Item::mk_type(spec, ident, params, ty, body)
}

pub fn handle_type_decl_body(pair: Pair<Rule>) -> TypeBody {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::record => TypeBody::Record(handle_record(pair)),
        Rule::constructors => TypeBody::Constructors(handle_constructors(pair)),
        _ => unreachable!(),
    }
}

pub fn handle_record(pair: Pair<Rule>) -> Vec<Field> {
    pair.into_inner().map(handle_field).collect()
}

pub fn handle_constructors(pair: Pair<Rule>) -> Vec<Constructor> {
    pair.into_inner().map(handle_constructor).collect()
}

pub fn handle_constructor(pair: Pair<Rule>) -> Constructor {
    let mut pairs = pair.into_inner();
    let ident = pairs.next().unwrap().as_str().to_string();
    let mut fields = Vec::new();
    let mut ty = None;

    for pair in pairs {
        match pair.as_rule() {
            Rule::fn_ty_params => fields = handle_fn_ty_params(pair),
            Rule::app_expr => {
                ty = Some(handle_app_expr(pair));
            }
            _ => unreachable!(),
        }
    }
    Constructor::new(ident, fields, ty)
}

pub fn handle_module(pair: Pair<Rule>) -> Item {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap().as_str().to_string();
    let items = pairs.map(handle_item).collect();
    Item::mk_module(name, items)
}

pub fn handle_import(pair: Pair<Rule>) -> Item {
    let ident = pair.into_inner().next().unwrap().as_str().to_string();
    Item::mk_import(ident)
}
