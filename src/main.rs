use std::{collections::HashMap, error::Error, fmt, str::FromStr};

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
    OpenParen,
    ClosedParen,
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
            "(" => Ok(Token::OpenParen),
            ")" => Ok(Token::ClosedParen),
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
        '+' | '-' | '*' | '/' | '(' | ')' => true,
        _ => c.is_whitespace(),
    }
}

#[derive(Debug)]
enum ParseError {
    LexerError(std::num::ParseFloatError),
    NotEnoughTokens,
    NonNumberToken,
    NonOpToken,
    UnmatchedParen,
    UndefinedVaraible(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::LexerError(_) => write!(f, "Failed to lex"),
            ParseError::NotEnoughTokens => write!(f, "Not enough tokens"),
            ParseError::NonNumberToken => write!(f, "Did not find expected number token"),
            ParseError::NonOpToken => write!(f, "Did not find expected op token"),
            ParseError::UnmatchedParen => write!(f, "Unmatched open bracket"),
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
            | ParseError::UnmatchedParen
            | ParseError::UndefinedVaraible(_) => None,
        }
    }
}

fn get_op_input<'a>(
    tokens: &'a [Token],
    var_lookup: &HashMap<String, usize>,
) -> Result<(Expression, &'a [Token]), ParseError> {
    match &tokens[0] {
        Token::Number(n) => Ok((Expression::Number(*n), &tokens[1..])),
        Token::Variable(s) => Ok((
            Expression::Variable(
                *var_lookup
                    .get(s)
                    .ok_or(ParseError::UndefinedVaraible(s.clone()))?,
            ),
            &tokens[1..],
        )),
        Token::OpenParen => {
            let mut depth = 1;
            let mut token_idx = 1;
            while depth > 0 {
                if token_idx >= tokens.len() {
                    return Err(ParseError::UnmatchedParen);
                }

                let token = &tokens[token_idx];
                match token {
                    Token::OpenParen => depth += 1,
                    Token::ClosedParen => depth -= 1,
                    _ => (),
                }

                if depth == 0 {
                    break;
                }

                token_idx += 1;
            }

            Ok((
                parse_tokens(&tokens[1..token_idx], var_lookup)?,
                &tokens[token_idx + 1..],
            ))
        }
        _ => Err(ParseError::NonNumberToken),
    }
}

fn make_op(op: &Token, lhs: Expression, rhs: Expression) -> Result<Operation, ParseError> {
    let ret = match op {
        Token::Add => Operation::Add(lhs, rhs),
        Token::Mul => Operation::Mul(lhs, rhs),
        Token::Div => Operation::Div(lhs, rhs),
        Token::Sub => Operation::Sub(lhs, rhs),
        Token::OpenParen | Token::ClosedParen | Token::Variable(_) | Token::Number(_) => {
            return Err(ParseError::NonOpToken)
        }
    };

    Ok(ret)
}

fn parse_operation(
    lhs: Expression,
    tokens: &[Token],
    var_lookup: &HashMap<String, usize>,
) -> Result<Operation, ParseError> {
    if tokens.len() < 2 {
        return Err(ParseError::NotEnoughTokens);
    }

    let this_op = &tokens[0];
    let (rhs, tokens) = get_op_input(&tokens[1..], var_lookup)?;

    if tokens.is_empty() {
        return make_op(this_op, lhs, rhs);
    }

    let next_op = &tokens[0];

    if next_op < this_op {
        // next op has higher priority, so rhs is the lhs of the next operation
        let rhs = parse_operation(rhs, tokens, var_lookup)?;
        make_op(this_op, lhs, Expression::Operation(Box::new(rhs)))
    } else {
        let lhs = make_op(this_op, lhs, rhs)?;
        parse_operation(Expression::Operation(Box::new(lhs)), tokens, var_lookup)
    }
}

fn parse_tokens(
    tokens: &[Token],
    var_lookup: &HashMap<String, usize>,
) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::NotEnoughTokens);
    }

    let (lhs, tokens) = get_op_input(tokens, var_lookup)?;

    if tokens.is_empty() {
        Ok(lhs)
    } else {
        Ok(Expression::Operation(Box::new(parse_operation(
            lhs, tokens, var_lookup,
        )?)))
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

fn parse(input: &str, var_lookup: &HashMap<String, usize>) -> Result<Expression, ParseError> {
    let tokens = lex(input)
        .collect::<Result<Vec<_>, _>>()
        .map_err(ParseError::LexerError)?;

    parse_tokens(&tokens, var_lookup)
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

    macro_rules! assert_matches {
        ($a:expr, $b:pat_param) => {
            match $a {
                $b => (),
                _ => panic!(concat!("{:?} does not match", stringify!($b)), $a),
            }

        }

    }

    macro_rules! assert_close {
        ($a:expr, $b:expr) => {
            assert_close!($a, $b, 0.0001)
        };
        ($a:expr, $b:expr, $eps:expr) => {
            let diff = f32::abs($a - $b);
            if f32::abs($a - $b) > $eps {
                panic!(
                    "{} and {} are {} apart, which is more than {}",
                    $a, $b, diff, $eps
                );
            }
        };
    }

    fn parse_and_execute(
        input: &str,
        var_lookup: &HashMap<String, f32>,
    ) -> Result<f32, ParseError> {
        let (key_lookup, args) = prepare_var_lookup(var_lookup);
        let expression = parse(input, &key_lookup)?;

        let ret = match expression {
            Expression::Operation(op) => execute_operation(&op, &args),
            Expression::Number(n) => n,
            Expression::Variable(n) => args[n],
        };

        Ok(ret)
    }

    #[test]
    fn simple_addition() {
        assert_close!(parse_and_execute("1 + 2", &HashMap::new()).unwrap(), 3.0);
    }

    #[test]
    fn simple_multiplication() {
        assert_close!(parse_and_execute("2 * 3", &HashMap::new()).unwrap(), 6.0);
    }

    #[test]
    fn add_then_mul() {
        assert_close!(
            parse_and_execute("1 + 2 * 3", &HashMap::new()).unwrap(),
            7.0
        );
    }

    #[test]
    fn mul_then_add() {
        assert_close!(
            parse_and_execute("1 * 2 + 3", &HashMap::new()).unwrap(),
            5.0
        );
    }

    #[test]
    fn add_two_muls() {
        assert_close!(
            parse_and_execute("1 * 2 + 3 * 4", &HashMap::new()).unwrap(),
            14.0
        );
    }

    #[test]
    fn add_sub_mul_div() {
        assert_close!(
            parse_and_execute("1 / 2 + 3 * 4 - 1", &HashMap::new()).unwrap(),
            11.5
        );
    }

    #[test]
    fn add_sub() {
        assert_close!(
            parse_and_execute("1 - 2 + 3", &HashMap::new()).unwrap(),
            2.0
        );
    }

    #[test]
    fn nospace() {
        assert_close!(
            parse_and_execute("1/2+3*4-1", &HashMap::new()).unwrap(),
            11.5
        );
    }

    #[test]
    fn apply_math_to_multiple_variables() {
        let statement = "x * x - y - y";
        let variables = [("x".to_string(), 3.0), ("y".to_string(), 5.0)]
            .into_iter()
            .collect();

        assert!(f32::abs(parse_and_execute(statement, &variables).unwrap() + 1.0) < 0.0001);
    }

    #[test]
    fn test_simple_paren() {
        let statement = "(3 * 4) + (5 * 6)";

        assert_close!(parse_and_execute(statement, &HashMap::new()).unwrap(), 42.0);
    }

    #[test]
    fn test_paren_single_number() {
        let statement = "(3) + (5 * 6)";

        assert_close!(parse_and_execute(statement, &HashMap::new()).unwrap(), 33.0);
    }

    #[test]
    fn test_double_paren_single_number() {
        let statement = "((3)) + (5 * 6)";

        assert_close!(parse_and_execute(statement, &HashMap::new()).unwrap(), 33.0);
    }

    #[test]
    fn test_double_paren() {
        let statement = "((3 * 4)) + (5 * 6)";

        assert_close!(parse_and_execute(statement, &HashMap::new()).unwrap(), 42.0);
    }

    #[test]
    fn test_unmatched_paren() {
        let statement = "((3 * 4)) + (5 * 6";
        match parse_and_execute(statement, &HashMap::new()) {
            Ok(_) => panic!("Unexpected success"),
            Err(e) => {
                assert_matches!(e, ParseError::UnmatchedParen);
            }
        }
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

    let (var_lookup, args) = prepare_var_lookup(&vars);

    let expression = parse(&statement, &var_lookup);
    let expression = match expression {
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

    match expression {
        Expression::Operation(op) => {
            println!("{}", execute_operation(&op, &args));
        }
        Expression::Variable(v) => {
            println!("{}", args[v]);
        }
        Expression::Number(n) => {
            println!("{n}");
        }
    }
}
