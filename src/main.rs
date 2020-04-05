#![type_length_limit = "1638329"]

use std::fmt;
use std::fmt::Write;
use std::fs;
use std::fs::File;
use std::io;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;

pub type Match = Result<Matcher, Matcher>;
pub type Capture = Result<(Matcher, Matcher), Matcher>;

#[derive(Clone)]
pub struct Matcher {
  data: Rc<Box<[u8]>>,
  start: usize,
  pos: usize,
}

impl Matcher {
  pub fn new(data: impl Into<Box<[u8]>>) -> Self {
    let data = Rc::new(data.into());
    Self {
      data,
      start: 0,
      pos: 0,
    }
  }

  #[inline(always)]
  fn advance(self, count: usize) -> Self {
    Self {
      pos: self.pos + count,
      ..self
    }
  }

  #[inline(always)]
  pub fn split(self) -> (Self, Self) {
    (
      self.clone(),
      Self {
        start: self.pos,
        ..self
      },
    )
  }

  #[inline(always)]
  pub fn skip(self) -> Self {
    Self {
      start: self.pos,
      ..self
    }
  }

  #[inline(always)]
  pub fn revert(self) -> Self {
    Self {
      pos: self.start,
      ..self
    }
  }

  #[inline(always)]
  pub fn byte_is(self, byte: u8) -> Match {
    match self.data.get(self.pos) {
      Some(&b) if b == byte => Ok(self.advance(1)),
      _ => Err(self),
    }
  }
  #[inline(always)]
  pub fn byte_is_any(self, bytes: &[u8]) -> Match {
    match self.data.get(self.pos) {
      Some(b) if bytes.contains(b) => Ok(self.advance(1)),
      _ => Err(self),
    }
  }

  #[inline(always)]
  pub fn byte_is_not_any(self, bytes: &[u8]) -> Match {
    match self.data.get(self.pos) {
      Some(b) if bytes.contains(b) => Err(self),
      Some(_) => Ok(self.advance(1)),
      None => Err(self),
    }
  }

  #[inline(always)]
  pub fn byte_is_in_range(self, min: u8, max: u8) -> Match {
    match self.data.get(self.pos) {
      Some(&b) if b >= min && b <= max => Ok(self.advance(1)),
      _ => Err(self),
    }
  }

  #[inline(always)]
  pub fn byte_is_alphanum(self) -> Match {
    self
      .byte_is_in_range(b'a', b'z')
      .or_else(|s| s.byte_is_in_range(b'A', b'Z'))
      .or_else(|s| s.byte_is_in_range(b'0', b'9'))
  }

  #[inline(always)]
  pub fn byte_sequence(self, bytes: impl AsRef<[u8]>) -> Match {
    let mut result = self.clone();
    for &b in bytes.as_ref().iter() {
      result = match result.byte_is_any(&[b]) {
        Ok(r) => r,
        Err(_) => return Err(self),
      }
    }
    Ok(result)
  }

  #[inline(always)]
  pub fn line_break(self) -> Match {
    self.byte_sequence(b"\r\n").or_else(|m| m.byte_is(b'\n'))
  }

  #[inline(always)]
  pub fn at_end(self) -> Match {
    match self.data.get(self.pos) {
      Some(_) => Err(self),
      None => Ok(self),
    }
  }

  #[inline(always)]
  pub fn zero_or_more(mut self, f: impl Fn(Self) -> Match) -> Self {
    loop {
      self = match f(self) {
        Ok(hit) => hit,
        Err(miss) => break miss,
      }
    }
  }

  #[inline(always)]
  pub fn one_or_more(self, f: impl Fn(Self) -> Match) -> Match {
    f(self).map(|hit| hit.zero_or_more(f))
  }

  #[inline(always)]
  pub fn select<R>(
    self,
  ) -> Select<R, impl FnOnce(Match) -> Result<(R, Matcher), Matcher>> {
    Select::<R, _> {
      matcher: self,
      eval_fn: |m| Err(m.unwrap_or_else(|m| m)),
    }
  }
}

impl Deref for Matcher {
  type Target = [u8];
  fn deref(&self) -> &Self::Target {
    &self.data[self.start..self.pos]
  }
}

impl fmt::Debug for Matcher {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_char('"')?;
    f.write_str(&String::from_utf8_lossy(&*self))?;
    f.write_char('"')?;
    Ok(())
  }
}

pub struct Select<R, FnEval>
where
  FnEval: FnOnce(Match) -> Result<(R, Matcher), Matcher>,
{
  matcher: Matcher,
  eval_fn: FnEval,
}

impl<R, FnEval> Select<R, FnEval>
where
  FnEval: FnOnce(Match) -> Result<(R, Matcher), Matcher>,
{
  pub fn or(
    self,
    match_fn: impl FnOnce(Matcher) -> Match,
    map_fn: impl FnOnce(&Matcher) -> R,
  ) -> Select<R, impl FnOnce(Match) -> Result<(R, Matcher), Matcher>> {
    let Select {
      matcher,
      eval_fn: next_eval_fn,
    } = self;
    let matcher_ = matcher.clone();
    let eval_fn = |best_match_so_far: Match| match match_fn(matcher_) {
      Ok(this_match)
        if this_match.len()
          >= best_match_so_far.as_ref().unwrap_or(&this_match).len() =>
      {
        next_eval_fn(Ok(this_match)).or_else(|this_match| {
          let map_result = map_fn(&this_match);
          Ok((map_result, this_match))
        })
      }
      _ => next_eval_fn(best_match_so_far),
    };
    Select { matcher, eval_fn }
  }

  pub fn evaluate(self) -> Result<(R, Matcher), Matcher> {
    (self.eval_fn)(Err(self.matcher))
  }
}

struct Lexer {
  filename: PathBuf,
  head: Matcher,
}

#[derive(Debug)]
enum Token {
  Comment,
  NewLine,
  Indent,
  Build,
  Pool,
  Rule,
  Default,
  Equals,
  Colon,
  Pipe2,
  Pipe,
  Include,
  SubNinja,
  Ident(Matcher),
  EOF,
}

#[derive(Debug)]
enum EvalStringPart {
  Literal(Matcher),
  Special(Matcher),
}

#[derive(Debug)]
struct EvalString(Vec<EvalStringPart>);

impl EvalString {
  pub fn new() -> Self {
    Self(Vec::new())
  }

  pub fn push(&mut self, part: EvalStringPart) {
    // TODO: merge adjacent literals.
    self.0.push(part)
  }
}

impl Lexer {
  pub fn new(filename: impl Into<PathBuf>) -> io::Result<Self> {
    let filename = filename.into();
    let data = fs::read(&filename)?;
    let head = Matcher::new(data);
    Ok(Self { filename, head })
  }

  fn match_comment(m: Matcher) -> Match {
    // [ ]*"#"[^\000\n]*"\n"
    let m = m.zero_or_more(|m| m.byte_is(b' '));
    let m = m.byte_is(b'#')?;
    let m = m.zero_or_more(|m| m.byte_is_not_any(b"\0\n"));
    let m = m.byte_is(b'\n')?;
    Ok(m)
  }

  fn match_var_name(m: Matcher) -> Match {
    // [a-zA-Z0-9_.-]+
    m.one_or_more(|m| m.byte_is_alphanum().or_else(|m| m.byte_is_any(b"_.-")))
  }

  fn match_simple_var_name(m: Matcher) -> Match {
    //  [a-zA-Z0-9_-]+
    m.one_or_more(|m| m.byte_is_alphanum().or_else(|m| m.byte_is_any(b"_-")))
  }

  pub fn eat_whitespace(m: Matcher) -> Matcher {
    m.zero_or_more(|m| {
      m.byte_is(b' ')
        .or_else(|m| m.byte_sequence(b"$\r\n"))
        .or_else(|m| m.byte_sequence(b"$\n"))
    })
  }

  #[inline(never)]
  pub fn read_token(&mut self) -> Token {
    loop {
      let r = self
        .head
        .clone()
        .select()
        .or(Self::match_comment, |_| Token::Comment)
        .or(
          |m| m.zero_or_more(|m| m.byte_is(b' ')).line_break(),
          |_| Token::NewLine,
        )
        .or(|m| m.one_or_more(|m| m.byte_is(b' ')), |_| Token::Indent)
        .or(|m| m.byte_sequence(b"build"), |_| Token::Build)
        .or(|m| m.byte_sequence(b"pool"), |_| Token::Pool)
        .or(|m| m.byte_sequence(b"rule"), |_| Token::Rule)
        .or(|m| m.byte_sequence(b"default"), |_| Token::Default)
        .or(|m| m.byte_sequence(b"="), |_| Token::Equals)
        .or(|m| m.byte_sequence(b":"), |_| Token::Colon)
        .or(|m| m.byte_sequence(b"||"), |_| Token::Pipe2)
        .or(|m| m.byte_sequence(b"|"), |_| Token::Pipe)
        .or(|m| m.byte_sequence(b"include"), |_| Token::Include)
        .or(|m| m.byte_sequence(b"subninja"), |_| Token::SubNinja)
        .or(Self::match_var_name, |m| Token::Ident(m.clone()))
        .or(|m| m.at_end(), |_| Token::EOF)
        .evaluate();
      match r {
        Ok((Token::Comment, matcher)) => {
          self.head = matcher.skip();
        }
        Ok((token @ Token::NewLine, matcher)) => {
          self.head = matcher.skip();
          break token;
        }
        Ok((token, matcher)) => {
          self.head = Self::eat_whitespace(matcher).skip();
          break token;
        }
        Err(matcher) => {
          panic!("Parse error: {}", String::from_utf8_lossy(&matcher))
        }
      }
    }
  }

  pub fn read_ident(&mut self) -> Option<Matcher> {
    match Self::match_var_name(self.head.clone()).map(|m| m.split()) {
      Ok((ident, head)) => {
        self.head = Self::eat_whitespace(head);
        Some(ident)
      }
      Err(_) => None,
    }
  }

  fn match_simple_literals(m: Matcher) -> Match {
    // [^$ :\r\n|\000]+
    m.one_or_more(|m| m.byte_is_not_any(b"$ :\r\n|\0"))
  }
  fn match_escaped_literal(m: Matcher) -> Match {
    let m = m.byte_is(b'$')?.skip();
    m.byte_is_any(b"$ :")
  }

  fn match_escaped_newline(m: Matcher) -> Match {
    let m = m.byte_is(b'$')?;
    let m = m.line_break()?;
    let m = m.zero_or_more(|m| m.byte_is(b' '));
    Ok(m.skip())
  }

  fn match_braceless_substitution(m: Matcher) -> Match {
    let m = m.byte_is(b'$')?;
    Self::match_simple_var_name(m)
  }
  fn match_braced_substitution(m: Matcher) -> Match {
    let m = m.byte_sequence(b"${")?;
    let m = Self::match_var_name(m)?;
    m.byte_is(b'}')
  }

  fn match_end_of_path(m: Matcher) -> Match {
    m.byte_is_any(b" :|")
  }

  #[inline(never)]
  fn read_eval_string(&mut self, is_path: bool) -> EvalString {
    use EvalStringPart::*;
    let mut eval_string = EvalString::new();
    loop {
      let r = self
        .head
        .clone()
        .select()
        .or(Self::match_simple_literals, |m| Some(Literal(m.clone())))
        .or(Self::match_escaped_literal, |m| Some(Literal(m.clone())))
        .or(Self::match_escaped_newline, |m| Some(Literal(m.clone())))
        .or(Self::match_braced_substitution, |m| {
          Some(Special(m.clone()))
        })
        .or(Self::match_braceless_substitution, |m| {
          Some(Special(m.clone()))
        })
        .or(Self::match_end_of_path, |m| {
          if is_path {
            None
          } else {
            Some(Literal(m.clone()))
          }
        })
        .or(|m| m.line_break(), |_| None)
        .evaluate();
      match r {
        Ok((Some(part), matcher)) => {
          eval_string.push(part);
          self.head = matcher.skip();
        }
        Ok((None, matcher)) => {
          let mut matcher = matcher; //.revert();
          if is_path {
            matcher = Self::eat_whitespace(matcher.revert())
          }
          self.head = matcher.skip();
          break;
        }
        Err(matcher) => {
          panic!("Parse error: {}", String::from_utf8_lossy(&matcher))
        }
      };
    }
    eval_string
  }

  pub fn read_path(&mut self) -> EvalString {
    self.read_eval_string(true)
  }

  pub fn read_var_value(&mut self) -> EvalString {
    self.read_eval_string(false)
  }
}
/*
bool Lexer::ReadEvalString(EvalString* eval, bool path, string* err) {
  const char* p = ofs_;
  const char* q;
  const char* start;
  for (;;) {
    start = p;
    /*!re2c
    [^$ :\r\n|\000]+ {
      eval->AddText(StringPiece(start, p - start));
      continue;
    }
    "\r\n" {
      if (path)
        p = start;
      break;
    }
    [ :|\n] {
      if (path) {
        p = start;
        break;
      } else {
        if (*start == '\n')
          break;
        eval->AddText(StringPiece(start, 1));
        continue;
      }
    }
    "$$" {
      eval->AddText(StringPiece("$", 1));
      continue;
    }
    "$ " {
      eval->AddText(StringPiece(" ", 1));
      continue;
    }
    "$\r\n"[ ]* {
      continue;
    }
    "$\n"[ ]* {
      continue;
    }
    "${"varname"}" {
      eval->AddSpecial(StringPiece(start + 2, p - start - 3));
      continue;
    }
    "$"simple_varname {
      eval->AddSpecial(StringPiece(start + 1, p - start - 1));
      continue;
    }
    "$:" {
      eval->AddText(StringPiece(":", 1));
      continue;
    }
    "$". {
      last_token_ = start;
      return Error("bad $-escape (literal $ must be written as $$)", err);
    }
    nul {
      last_token_ = start;
      return Error("unexpected EOF", err);
    }
    [^] {
      last_token_ = start;
      return Error(DescribeLastError(), err);
    }
    */
  }
  last_token_ = start;
  ofs_ = p;
  if (path)
    EatWhitespace();
  // Non-path strings end in newlines, so there's no whitespace to eat.
  return true;
}
  */

/*
nul = "\000";
 simple_varname = [a-zA-Z0-9_-]+;
 varname = [a-zA-Z0-9_.-]+;
 [ ]*"#"[^\000\n]*"\n" { continue; }
 [ ]*"\r\n" { token = NEWLINE;  break; }
 [ ]*"\n"   { token = NEWLINE;  break; }
 [ ]+       { token = INDENT;   break; }
 "build"    { token = BUILD;    break; }
 "pool"     { token = POOL;     break; }
 "rule"     { token = RULE;     break; }
 "default"  { token = DEFAULT;  break; }
 "="        { token = EQUALS;   break; }
 ":"        { token = COLON;    break; }
 "||"       { token = PIPE2;    break; }
 "|"        { token = PIPE;     break; }
 "include"  { token = INCLUDE;  break; }
 "subninja" { token = SUBNINJA; break; }
 varname    { token = IDENT;    break; }
 nul        { token = TEOF;     break; }
 [^]        { token = ERROR;    break; }
 */

pub fn main() {
  let mut l =
    Lexer::new("D:\\v8rs\\target\\debug\\gn_out\\build.ninja").unwrap();
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_var_value());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_var_value());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_var_value());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_path());
  println!("{:?}", l.read_token());
  println!("{:?}", l.read_path());
}
