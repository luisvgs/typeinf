#[macro_use]
extern crate lalrpop_util;
use std::collections::HashMap;
lalrpop_mod!(pub grammar);


pub fn apply_substitution(subs: &Substitution, t: &Type) -> Type {
    match t {
	Type::TNamed(_) => t.clone(),
	Type::TVar(ref v) => subs.0.get(v).cloned().unwrap_or(t.clone()),
	Type::TFun(f) => {
	    return Type::TFun(TFun{
		from: Box::new(apply_substitution(subs, &*f.from)),
		to: Box::new(apply_substitution(subs, &*f.to)),
	    })
	}

    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Var(String),
    Int(i32),
    Fun(String, Box<Expression>),
    TCall(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Substitution(HashMap<TVar, Type>);


#[derive(Debug, Clone)]
pub struct Context {
    next: i32,
    env: Env
}

#[derive(Debug, Clone)]
pub struct Env {
    pub environment: HashMap<String, Type>,
}

pub fn add_to_ctx(ctx: Context, name: String, t: Type) -> Context {
    let mut new_env = Context {..ctx};
    if let Some(ty) = new_env.env.environment.get_mut(&name) {
	*ty = t;
    }
    new_env
}

pub fn new_tvar(mut ctx: Context) -> Type {
    let new_var_num = ctx.next;
    ctx.next+=1;
    Type::TVar(TVar(new_var_num as usize))
}

pub fn infer(e: Box<Expression>, ctx: &Context) -> (Type, Substitution) {
    let mut env = ctx.env.clone();
    match *e {
	Expression::Int(_) => (Type::TNamed(TNamed(String::from("Int"))), Substitution(HashMap::new())),
	Expression::Var(v)=> {
	    match ctx.env.environment.contains_key(&v) {
		// true => Type::TVar(TVar(v.to_owned())),
		true => {
		    if let Some(variable) = ctx.env.environment.get(&v) {
			(variable.clone(), Substitution(HashMap::new()))
		    }
		    else { unimplemented!() }
		},
		false => unimplemented!(),
	    }
	},
	Expression::Fun(name, body) => {
	    let new_type = new_tvar(ctx.clone());
	    let new_ctx = add_to_ctx(ctx.clone(), name, new_type.clone());
	    let (body_type, subs) = infer(body, &new_ctx);

	    let inferred_type = Type::TFun(TFun {
		from: Box::new(apply_substitution(&subs, &new_type)),
		to: Box::new(body_type),
	    });

	    return (inferred_type, subs.clone())

	}
	_ => unimplemented!()
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct TFun {
    pub from: Box<Type>,
    pub to: Box<Type>,
}

#[derive(Copy, Debug, PartialEq, Hash, Eq, Clone)]
pub struct TVar(pub usize);

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct TNamed(pub String);

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum Type {
    TNamed(TNamed),
    TVar(TVar),
    TFun(TFun),
}

pub fn to_node(p: Box<Expression>) -> i32 {
    match *p {
	Expression::Int(x) => x,
	_ => unimplemented!()
    }
}

fn main() {
    let source = "1";
    let parser = grammar::ExpressionParser::new().parse(source).ok().unwrap();
    let node = to_node(parser);

    let mut init_env = Env{ environment : HashMap::new() };
    // init_env.environment.insert("+".to_string(), Type::TFun(TFun{
	// from: Box::new(Type::TNamed(TNamed(String::from("Int")))),
	// to: Box::new(Type::TNamed(TNamed(String::from("Int")))),
    // }));

    init_env.environment.insert("+".to_string(), Type::TNamed(TNamed(String::from("Int"))));
    let res = infer(Box::new(Expression::Fun(String::from("+"), Box::new(Expression::Int(4)))), &Context{
	next:0,
	env: init_env,
    });

    println!("{:?}", res);
}

#[test]
fn testing() {
    assert!(grammar::ExpressionParser::new().parse("1").is_ok());
}
