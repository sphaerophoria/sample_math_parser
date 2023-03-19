use std::{cmp::Ordering, collections::HashMap, error::Error, fmt, str::FromStr};

#[derive(Debug)]
enum Expression {
    Operation(Box<Operation>),
    Variable(usize),
    Number(f32),
}

#[derive(Debug)]
enum Operation {
    Add(Expression, Expression),
    Mul(Expression, Expression),
    Div(Expression, Expression),
    Sub(Expression, Expression),
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Token {
    //NOTE sorted by priority for easy comparison later
    Variable(String),
    Number(f32),
    Mul,
    Div,
    Sub,
    Add,
}

impl FromStr for Token {
    type Err = std::num::ParseFloatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Token::Add),
            "-" => Ok(Token::Sub),
            "*" => Ok(Token::Mul),
            "/" => Ok(Token::Div),
            _ => {
                if s.chars().next().unwrap().is_alphabetic() {
                    Ok(Token::Variable(s.to_string()))
                } else {
                    s.parse().map(Token::Number)
                }
            }
        }
    }
}

fn lex(input: &str) -> impl Iterator<Item = Result<Token, std::num::ParseFloatError>> + '_ {
    input
        .split_whitespace()
        .flat_map(split_str)
        .map(Token::from_str)
}

fn split_str(mut input: &str) -> impl Iterator<Item = &str> {
    let mut substrs = vec![];
    loop {
        if let Some(mut pos) = input.find(splits_input) {
            pos = pos.max(1);
            substrs.push(&input[0..pos]);
            input = &input[pos..];
        } else {
            if !input.is_empty() {
                substrs.push(input);
            }
            break;
        }
    }

    substrs.into_iter()
}

fn splits_input(c: char) -> bool {
    match c {
        '+' | '-' | '*' | '/' => true,
        _ => c.is_whitespace(),
    }
}

#[derive(Debug)]
enum ParseError {
    LexerError(std::num::ParseFloatError),
    NotEnoughTokens,
    NonNumberToken,
    NonOpToken,
    UndefinedVaraible(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::LexerError(_) => write!(f, "Failed to lex"),
            ParseError::NotEnoughTokens => write!(f, "Not enough tokens"),
            ParseError::NonNumberToken => write!(f, "Did not find expected number token"),
            ParseError::NonOpToken => write!(f, "Did not find expected op token"),
            ParseError::UndefinedVaraible(var) => write!(f, "Undefined variable {var}"),
        }
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ParseError::LexerError(e) => Some(e),
            ParseError::NotEnoughTokens
            | ParseError::NonNumberToken
            | ParseError::NonOpToken
            | ParseError::UndefinedVaraible(_) => None,
        }
    }
}

fn get_number(
    token: &Token,
    var_lookup: &HashMap<String, usize>,
) -> Result<Expression, ParseError> {
    match token {
        Token::Number(n) => Ok(Expression::Number(*n)),
        Token::Variable(s) => Ok(Expression::Variable(
            *var_lookup
                .get(s)
                .ok_or(ParseError::UndefinedVaraible(s.clone()))?,
        )),
        _ => Err(ParseError::NonNumberToken),
    }
}

fn make_op(op: &Token, lhs: Expression, rhs: Expression) -> Result<Operation, ParseError> {
    let ret = match op {
        Token::Add => Operation::Add(lhs, rhs),
        Token::Mul => Operation::Mul(lhs, rhs),
        Token::Div => Operation::Div(lhs, rhs),
        Token::Sub => Operation::Sub(lhs, rhs),
        Token::Variable(_) | Token::Number(_) => return Err(ParseError::NonOpToken),
    };

    Ok(ret)
}

fn parse_tokens(
    lhs: Expression,
    tokens: &[Token],
    var_lookup: &HashMap<String, usize>,
) -> Result<Operation, ParseError> {
    match tokens.len().cmp(&2) {
        Ordering::Less => Err(ParseError::NotEnoughTokens),
        Ordering::Equal => {
            let rhs = get_number(&tokens[1], var_lookup)?;
            make_op(&tokens[0], lhs, rhs)
        }
        Ordering::Greater => {
            // Determine if number goes to this op or the next one...
            let next_op = &tokens[2];
            let this_op = &tokens[0];
            let num = get_number(&tokens[1], var_lookup)?;

            if next_op < this_op {
                // next op has higher priority, so number is the lhs of the next operation
                let rhs = parse_tokens(num, &tokens[2..], var_lookup)?;
                make_op(this_op, lhs, Expression::Operation(Box::new(rhs)))
            } else {
                let lhs = make_op(this_op, lhs, num)?;
                parse_tokens(
                    Expression::Operation(Box::new(lhs)),
                    &tokens[2..],
                    var_lookup,
                )
            }
        }
    }
}

fn resolve_expression(expression: &Expression, vars: &[f32]) -> f32 {
    match expression {
        Expression::Operation(i) => execute_operation(i, vars),
        Expression::Number(n) => *n,
        Expression::Variable(i) => vars[*i],
    }
}

fn execute_operation(operation: &Operation, vars: &[f32]) -> f32 {
    match operation {
        Operation::Add(a, b) => resolve_expression(a, vars) + resolve_expression(b, vars),
        Operation::Mul(a, b) => resolve_expression(a, vars) * resolve_expression(b, vars),
        Operation::Div(a, b) => resolve_expression(a, vars) / resolve_expression(b, vars),
        Operation::Sub(a, b) => resolve_expression(a, vars) - resolve_expression(b, vars),
    }
}

fn parse(input: &str, var_lookup: &HashMap<String, usize>) -> Result<Operation, ParseError> {
    let tokens = lex(input)
        .collect::<Result<Vec<_>, _>>()
        .map_err(ParseError::LexerError)?;

    parse_tokens(
        get_number(&tokens[0], var_lookup)?,
        &tokens[1..],
        var_lookup,
    )
}

fn prepare_var_lookup(var_lookup: &HashMap<String, f32>) -> (HashMap<String, usize>, Vec<f32>) {
    var_lookup
        .iter()
        .enumerate()
        .map(|(i, (k, v))| ((k.clone(), i), v))
        .unzip()
}

#[cfg(test)]
mod test {

    use super::*;

    fn parse_and_execute(
        input: &str,
        var_lookup: &HashMap<String, f32>,
    ) -> Result<f32, ParseError> {
        let (key_lookup, args) = prepare_var_lookup(var_lookup);
        let operation = parse(input, &key_lookup)?;

        Ok(execute_operation(&operation, &args))
    }

    #[test]
    fn simple_addition() {
        assert!(f32::abs(parse_and_execute("1 + 2", &HashMap::new()).unwrap() - 3.0) < 0.0001);
    }

    #[test]
    fn simuple_multiplication() {
        assert!(f32::abs(parse_and_execute("2 * 3", &HashMap::new()).unwrap() - 6.0) < 0.0001);
    }

    #[test]
    fn add_then_mul() {
        assert!(f32::abs(parse_and_execute("1 + 2 * 3", &HashMap::new()).unwrap() - 7.0) < 0.0001);
    }

    #[test]
    fn mul_then_add() {
        assert!(f32::abs(parse_and_execute("1 * 2 + 3", &HashMap::new()).unwrap() - 5.0) < 0.0001);
    }

    #[test]
    fn add_two_muls() {
        assert!(
            f32::abs(parse_and_execute("1 * 2 + 3 * 4", &HashMap::new()).unwrap() - 14.0) < 0.0001
        );
    }

    #[test]
    fn add_sub_mul_div() {
        assert!(
            f32::abs(parse_and_execute("1 / 2 + 3 * 4 - 1", &HashMap::new()).unwrap() - 11.5)
                < 0.0001
        );
    }

    #[test]
    fn add_sub() {
        assert!(f32::abs(parse_and_execute("1 - 2 + 3", &HashMap::new()).unwrap() - 2.0) < 0.0001);
    }

    #[test]
    fn nospace() {
        assert!(f32::abs(parse_and_execute("1/2+3*4-1", &HashMap::new()).unwrap() - 11.5) < 0.0001);
    }

    #[test]
    fn apply_math_to_multiple_variables() {
        let statement = "x * x - y - y";
        let variables = [("x".to_string(), 3.0), ("y".to_string(), 5.0)]
            .into_iter()
            .collect();

        assert!(f32::abs(parse_and_execute(statement, &variables).unwrap() + 1.0) < 0.0001);
    }
}

fn main() {
    let mut args = std::env::args();
    // Skip fn name
    args.next();

    let statement = args.next().expect("First argument should be statement");
    let mut vars = HashMap::new();
    for key in args {
        vars.insert(key.to_string(), 0f32);
    }

    let (var_lookup, mut args) = prepare_var_lookup(&vars);

    let operation = parse(&statement, &var_lookup);
    let operation = match operation {
        Ok(v) => v,
        Err(e) => {
            println!("Failed to execute statement: {e}");

            let mut source = e.source();
            if source.is_none() {
                return;
            }

            println!("Caused by: ");
            while let Some(v) = source {
                println!("{v}");
                source = v.source();
            }
            return;
        }
    };

    let mut total = 0f32;
    for i in 0..1000000 {
        for v in &mut args {
            *v = i as f32;
        }

        total += execute_operation(&operation, &args);
    }

    println!("total: {total}");
}
