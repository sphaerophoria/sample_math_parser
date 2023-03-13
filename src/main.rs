use std::{cmp::Ordering, collections::HashMap, error::Error, fmt};

#[derive(Debug)]
enum Expression {
    Operation(Box<Operation>),
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

struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { input }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, std::num::ParseFloatError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut it = self.input.chars().enumerate().peekable();

        while let Some((_, _)) = it.next_if(|(_, c)| c.is_whitespace()) {}

        let mut s = String::new();
        while let Some((_, c)) = it.next_if(|(_, c)| !c.is_whitespace()) {
            s.push(c);
        }

        if s.is_empty() {
            return None;
        }

        let next_start = it.next().map(|(i, _)| i).unwrap_or(self.input.len());
        self.input = &self.input[next_start..];

        match s.as_str() {
            "+" => Some(Ok(Token::Add)),
            "-" => Some(Ok(Token::Sub)),
            "*" => Some(Ok(Token::Mul)),
            "/" => Some(Ok(Token::Div)),
            _ => {
                if s.chars().next().unwrap().is_alphabetic() {
                    Some(Ok(Token::Variable(s)))
                } else {
                    Some(s.parse().map(Token::Number))
                }
            }
        }
    }
}

fn lex(input: &str) -> Result<Vec<Token>, std::num::ParseFloatError> {
    Lexer::new(input).collect()
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

fn get_number(token: &Token, var_lookup: &HashMap<String, f32>) -> Result<Expression, ParseError> {
    match token {
        Token::Number(n) => Ok(Expression::Number(*n)),
        Token::Variable(s) => Ok(Expression::Number(
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

fn generate_operation(
    lhs: Expression,
    tokens: &[Token],
    var_lookup: &HashMap<String, f32>,
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
                let rhs = generate_operation(num, &tokens[2..], var_lookup)?;
                make_op(this_op, lhs, Expression::Operation(Box::new(rhs)))
            } else {
                let lhs = make_op(this_op, lhs, num)?;
                generate_operation(
                    Expression::Operation(Box::new(lhs)),
                    &tokens[2..],
                    var_lookup,
                )
            }
        }
    }
}

fn resolve_expression(expression: &Expression) -> f32 {
    match expression {
        Expression::Operation(i) => execute_operation(i),
        Expression::Number(n) => *n,
    }
}

fn execute_operation(operation: &Operation) -> f32 {
    match operation {
        Operation::Add(a, b) => resolve_expression(a) + resolve_expression(b),
        Operation::Mul(a, b) => resolve_expression(a) * resolve_expression(b),
        Operation::Div(a, b) => resolve_expression(a) / resolve_expression(b),
        Operation::Sub(a, b) => resolve_expression(a) - resolve_expression(b),
    }
}

fn parse_tokens_and_exeucte(
    tokens: &[Token],
    var_lookup: &HashMap<String, f32>,
) -> Result<f32, ParseError> {
    let operation = generate_operation(
        get_number(&tokens[0], var_lookup)?,
        &tokens[1..],
        var_lookup,
    )?;

    Ok(execute_operation(&operation))
}

fn parse_and_execute(input: &str, var_lookup: &HashMap<String, f32>) -> Result<f32, ParseError> {
    let tokens = lex(input).map_err(ParseError::LexerError)?;

    parse_tokens_and_exeucte(&tokens, var_lookup)
}

#[cfg(test)]
mod test {
    use super::*;

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
    while let (Some(key), Some(val)) = (args.next(), args.next()) {
        vars.insert(
            key.to_string(),
            val.parse().expect("Failed to parse variable value"),
        );
    }
    let res = parse_and_execute(&statement, &vars);

    match res {
        Ok(v) => println!("{v}"),
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
        }
    }
}
