use std::collections::HashMap;
use std::fmt;

//macro for new lambda
#[macro_export]
macro_rules! lambda {
    ($x:expr) => (
        Lambda::new($x, vec![])
    );
    ($x:expr, $($y:expr), +) => (
        Lambda::new($x, vec![$($y), +])
    );

}

//Lambda data type
#[derive(Debug, PartialEq, Clone)]
pub enum Lambda {
    Func((Box<Lambda>, Box<Lambda>)),
    Variable(String),
    Reducible((Box<Lambda>, Box<Lambda>)),
    AlphaMark(Box<Lambda>),
    //tokens
    StVec(Vec<String>),
    Brack(Vec<Lambda>),
    TFunc(Vec<String>),
    Container(Box<Lambda>),
    AttPl(()),
}

impl Lambda {
    //Alphabet for variable naming
    const ALPH: &str = "xyzwabcdefghijklmnopqrstuv";
    //new lambda from formatted string
    pub fn new(s: &str, f: Vec<Lambda>) -> Lambda {
        let chars = s
            .chars()
            .collect::<Vec<char>>()
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        let bracks = Self::find_bracks(chars, false);
        let tokens = Self::parse_bracks(bracks, &f, 0).0;
        Self::parse_tokens(tokens)
    }
    //order characters by brackets
    fn find_bracks(chars: Vec<String>, alph: bool) -> Lambda {
        //find brackets
        let mut starts: Vec<usize> = Vec::new();
        let mut ends: Vec<usize> = Vec::new();
        let mut alphas: Vec<usize> = Vec::new();
        let mut count = 0;
        for (i, char) in chars.iter().enumerate() {
            match (char.as_str(), count) {
                (")", 1) => {
                    ends.push(i);
                    count -= 1;
                }
                (")", 0) => panic!("Unclosed bracket"),
                (")", _) => count -= 1,
                ("(", 0) => {
                    if i != 0 && chars[i - 1] == "&" {
                        alphas.push(i)
                    }
                    starts.push(i);
                    count += 1;
                }
                ("(", _) => count += 1,
                _ => {}
            }
        }
        if starts.len() != ends.len() {
            panic!("Unclosed bracket");
        }
        //split the string into bracket tokens and mark them for alpha reduction if needed
        if starts.is_empty() && alph {
            return Self::AlphaMark(Box::new(Self::Brack(vec![Self::StVec(chars)])));
        } else if starts.is_empty() {
            return Self::Brack(vec![Self::StVec(chars)]);
        }
        let mut bracks: Vec<Lambda> = Vec::new();
        if !chars[..starts[0]].to_vec().is_empty() {
            bracks.push(Self::StVec(chars[..starts[0]].to_vec()));
        }
        for i in 0..starts.len() {
            if alphas.contains(&starts[i]) {
                bracks.push(Self::find_bracks(
                    chars[starts[i] + 1..ends[i]].to_vec(),
                    true,
                ));
            } else {
                bracks.push(Self::find_bracks(
                    chars[starts[i] + 1..ends[i]].to_vec(),
                    false,
                ));
            }
            if i + 1 != starts.len() {
                bracks.push(Self::StVec(chars[ends[i] + 1..starts[i + 1]].to_vec()));
            } else if !chars[ends[i] + 1..].to_vec().is_empty() {
                bracks.push(Self::StVec(chars[ends[i] + 1..].to_vec()));
            }
        }
        if alph {
            return Self::AlphaMark(Box::new(Self::Brack(bracks)));
        }
        Self::Brack(bracks)
    }
    //parse the brackets and characters into brackets and tokens
    fn parse_bracks(brs: Lambda, vec: &Vec<Lambda>, mut vec_num: usize) -> (Lambda, usize) {
        if let Self::Brack(br) = brs {
            let mut parse_vec: Vec<Lambda> = Vec::new();
            for b in br {
                match &b {
                    Self::Brack(_) => {
                        let t: Lambda;
                        (t, vec_num) = Self::parse_bracks(b, vec, vec_num);
                        if let Self::Brack(v) = &t
                            && v.len() == 1
                        {
                            parse_vec.push(v[0].clone());
                        } else {
                            parse_vec.push(t);
                        }
                    }
                    Self::StVec(v) => {
                        let t: Vec<Lambda>;
                        (t, vec_num) = Self::parse_stvec(v.clone(), vec, vec_num);
                        parse_vec.extend_from_slice(&t[..]);
                    }
                    Self::AlphaMark(l) => {
                        let temp: Lambda;
                        (temp, vec_num) = Self::parse_bracks(*l.clone(), vec, vec_num);
                        parse_vec.push(Self::AlphaMark(Box::new(temp)));
                    }
                    _ => panic!("Syntax error"),
                }
            }
            return (Self::Brack(parse_vec), vec_num);
        }
        panic!("Syntax error")
    }
    //turn the characters into tokens
    fn parse_stvec(strs: Vec<String>, vec: &[Lambda], mut vec_num: usize) -> (Vec<Lambda>, usize) {
        let mut token_vec: Vec<Lambda> = Vec::new();
        let mut pass_num = 0;
        for i in 0..strs.len() {
            if pass_num > 0 {
                pass_num -= 1;
            } else if strs[i] == "%" {
                (token_vec, pass_num) = Self::parse_func_char(strs.clone(), token_vec, i);
            } else if strs[i] == "{" && strs[i + 1] == "}" {
                token_vec.push(Self::Container(Box::new(vec[vec_num].clone())));
                vec_num += 1;
                pass_num += 1;
            } else {
                for j in i..strs.len() {
                    match strs[j].as_str() {
                        " " => {
                            token_vec.push(Self::AttPl(()));
                        }
                        "&" => {}
                        "{" => {}
                        "}" => {}
                        _ => {
                            (token_vec, pass_num) = Self::find_vars(&strs, token_vec, j);
                        }
                    }
                }
                pass_num += 1;
            }
        }
        (token_vec, vec_num)
    }
    //find variable tokens
    fn find_vars(strs: &[String], mut token_vec: Vec<Lambda>, i: usize) -> (Vec<Lambda>, i32) {
        let mut pass_num = 0;
        if !Self::ALPH.contains(&strs[i]) {
            panic!("Syntax error");
        }
        let mut var: String = "".to_string();
        for k in i..strs.len() {
            if !Self::ALPH.contains(&strs[k]) {
                token_vec.push(Self::Variable(var));
                pass_num += 1;
                break;
            }
            var.push_str(&strs[k]);
            if k == strs.len() - 1 {
                token_vec.push(Self::Variable(var));
                pass_num += 1;
                break;
            }
            pass_num += 1;
        }
        (token_vec, pass_num)
    }
    //parse the function syntax
    fn parse_func_char(
        strs: Vec<String>,
        mut token_vec: Vec<Lambda>,
        i: usize,
    ) -> (Vec<Lambda>, i32) {
        let mut pass_num: i32 = 0;
        let mut val_vec: Vec<String> = Vec::new();
        let mut var: String = "".to_string();
        for st in strs[i + 1..].iter() {
            match st.as_str() {
                "." => {
                    val_vec.push(var);
                    token_vec.push(Self::TFunc(val_vec));
                    pass_num += 1;
                    break;
                }
                "|" => {
                    val_vec.push(var);
                    var = "".to_string();
                }
                _ => {
                    if Self::ALPH.contains(st) {
                        var.push_str(st);
                    } else {
                        panic!("Syntax error");
                    }
                }
            }
            pass_num += 1;
        }
        (token_vec, pass_num)
    }
    //turn brackets and tokens into a lambda
    fn parse_tokens(token: Lambda) -> Lambda {
        match token {
            Self::Brack(v) => Self::parse_token_vec(v),
            Self::Variable(_) => token,
            Self::AlphaMark(l) => Self::AlphaMark(Box::new(Self::parse_tokens(*l))),
            Self::Reducible((a, b)) => Self::parse_tokens(*a).attach(Self::parse_tokens(*b)),
            Self::Func((a, b)) => Self::Func((a, Box::new(Self::parse_tokens(*b)))),
            Self::Container(l) => *l,
            _ => panic!("syntax error"),
        }
    }
    //turn a vec of tokens into a lambda
    fn parse_token_vec(mut vec: Vec<Lambda>) -> Lambda {
        let temp_vec = vec.clone();
        for (i, l) in temp_vec.iter().enumerate() {
            if let Self::AttPl(_) = *l {
                vec = [
                    &vec[..i - 1],
                    &vec![
                        Self::parse_tokens(vec[i - 1].clone())
                            .attach(Self::parse_tokens(vec[i + 1].clone())),
                    ][..],
                    &vec[i + 2..],
                ]
                .concat();
            }
        }
        match vec[0].clone() {
            Self::TFunc(v) => Self::parse_func_token(v, vec),
            Self::Brack(_) => Self::parse_tokens(vec[0].clone()),
            Self::Reducible(_) => Self::parse_tokens(vec[0].clone()),
            Self::Variable(_) => vec[0].clone(),
            _ => panic!("Syntax error"),
        }
    }
    //turn the shorthand function token into lambda functions
    fn parse_func_token(vec: Vec<String>, tokens: Vec<Lambda>) -> Lambda {
        if !vec.is_empty() {
            return Self::func(
                vec[0].as_str(),
                Self::parse_func_token(vec[1..vec.len()].to_vec(), tokens),
            );
        }
        if tokens.is_empty() {
            panic!("Syntax error");
        }
        Self::parse_token_vec(tokens[1..tokens.len()].to_vec())
    }
    //make new function variant with a string and a Lambda
    pub fn func(a: &str, b: Lambda) -> Lambda {
        Self::Func((Box::new(Self::var(a)), Box::new(b)))
    }
    //make new variable variant with a string
    pub fn var(inp: &str) -> Lambda {
        Self::Variable(inp.to_string())
    }
    //make new reductible variant by attaching an input Lambda to a Lambda
    pub fn attach(self, a: Lambda) -> Lambda {
        Self::Reducible((Box::new(self), Box::new(a)))
    }
    //mark a lambda for alpha reduction
    pub fn alpha(self) -> Lambda {
        Self::AlphaMark(Box::new(self))
    }
    //beta reduce a reducible
    pub fn reduce(self) -> Lambda {
        if let Self::Reducible((a, b)) = self.clone() {
            //if reducible has a function reduce the function, else if it has a reducible, reduce the inside reducible
            if let Self::Func((c, d)) = &*a {
                return Self::recursive_reduce(*d.clone(), *c.clone(), *b);
            } else if let Self::Reducible(_) = &*a {
                return a.reduce().attach(*b);
            }
            panic!("Cannot reduce");
        }
        panic!("Cannot reduce");
    }
    //alpha reduce a lambda
    pub fn alpha_reduce(self) -> Lambda {
        let (m, _, _) = Self::set_map(self.clone(), vec![HashMap::new()], 0, 0, 0);
        let (out, _) = Self::recursive_alpha(self, m, 0, 0);
        out
    }
    //recursive function to substitute every instance of the given variable
    fn recursive_reduce(b: Lambda, a: Lambda, sub: Lambda) -> Lambda {
        match b {
            //if it is a function variant, check for the variable being substituted or reduce an inside function
            Self::Func((c, d)) => {
                if a != *d {
                    return Self::Func((c, Box::new(Self::recursive_reduce(*d, a, sub))));
                }
                Self::Func((c, Box::new(sub)))
            }
            //if it is a reducible, reduce both the function and the input expression
            Self::Reducible((c, d)) => Self::recursive_reduce(*c, a.clone(), sub.clone())
                .attach(Self::recursive_reduce(*d, a, sub)),
            //if it is just a variable, substitute if it is the variable being substituted
            Self::Variable(_) => {
                if a == b {
                    return sub;
                }
                b
            }
            Self::AlphaMark(_) => b,
            _ => panic!("Cannot reduce {:?}", b),
        }
    }
    //function to evaluate lambda
    pub fn evaluate(self) -> Lambda {
        self.alpha_reduce().recursive_evaluate().alpha_reduce()
    }
    //function to recursively reduce every reducible
    fn recursive_evaluate(self) -> Lambda {
        if let Self::Reducible(_) = self {
            return self.reduce().recursive_evaluate();
        }
        self
    }
    //function to assign a vector of hashmaps to a lambda
    fn set_map(
        l: Lambda,
        mut m: Vec<HashMap<String, usize>>,
        mut i: usize,
        al: usize,
        mut al_in: usize,
    ) -> (Vec<HashMap<String, usize>>, usize, usize) {
        match l {
            Self::Variable(a) => {
                if !m[al].contains_key(a.as_str()) {
                    m[al].insert(a, i);
                    (m, i + 1, al_in)
                } else {
                    (m, i, al_in)
                }
            }
            Self::Func((a, b)) => {
                if let Self::Variable(c) = *a
                    && !m[al].contains_key(c.as_str())
                {
                    m[al].insert(c, i);
                }
                Self::set_map(*b, m, i + 1, al, al_in)
            }
            Self::Reducible((a, b)) => {
                (m, i, al_in) = Self::set_map(*a, m, i, al, al_in);
                Self::set_map(*b, m, i, al, al_in)
            }
            Self::AlphaMark(a) => {
                al_in += 1;
                m.push(HashMap::new());
                Self::set_map(*a, m, i, al_in, al_in)
            }
            _ => panic!("Cannot map lambda"),
        }
    }
    //function to get a lambda variable name from an integer
    fn get_name(mut n: usize) -> String {
        let mut out = "".to_string();
        let chars = Self::ALPH.chars();
        let num = chars.clone().count();
        n += 1;
        while n != 0 {
            let mut temp = chars.clone().nth((n - 1) % num).unwrap().to_string();
            temp.push_str(&out);
            out = temp;
            n = ((n - 1) - ((n - 1) % num)) / num;
        }
        out
    }
    //recursive function to remove any colliding variable names
    fn recursive_alpha(
        l: Lambda,
        m: Vec<HashMap<String, usize>>,
        al: usize,
        mut al_in: usize,
    ) -> (Lambda, usize) {
        match l {
            Self::Variable(a) => match m[al].get(&a) {
                Some(b) => (Self::Variable(Self::get_name(*b)), al_in),
                _ => panic!("Cannot alpha reduce"),
            },
            Self::Func((a, b)) => {
                let c: Lambda;
                let d: Lambda;
                (c, al_in) = Self::recursive_alpha(*a, m.clone(), al, al_in);
                (d, al_in) = Self::recursive_alpha(*b, m, al, al_in);
                (Self::Func((Box::new(c), Box::new(d))), al_in)
            }
            Self::Reducible((a, b)) => {
                let c: Lambda;
                let d: Lambda;
                (c, al_in) = Self::recursive_alpha(*a, m.clone(), al, al_in);
                (d, al_in) = Self::recursive_alpha(*b, m, al, al_in);
                (c.attach(d), al_in)
            }
            Self::AlphaMark(a) => {
                al_in += 1;
                Self::recursive_alpha(*a, m, al_in, al_in)
            }
            _ => panic!("Cannot alpha reduce"),
        }
    }
    //function to calculate a string to represent the lambda
    fn display(l: Lambda) -> String {
        match l {
            Self::Variable(a) => a,
            Self::Func((a, mut b)) => {
                let d = b.clone();
                let mut s1 = Self::display(*a);
                let mut s2 = "".to_string();
                while let Self::Func((ref a, ref c)) = *b {
                    s1.push('|');
                    s1.push_str(&Self::display(*a.clone()));
                    if let Self::Func(_) = **c {
                    } else {
                        s2 = Self::display(*c.clone());
                    }
                    b = c.clone();
                }
                if s2.is_empty() {
                    s2 = Self::display(*d)
                }
                format!("(λ{}.{})", &s1, &s2)
            }
            Self::Reducible((a, b)) => {
                let s1 = Self::display(*a);
                let s2 = Self::display(*b);
                format!("({} {})", &s1, &s2)
            }
            Self::AlphaMark(a) => {
                let s1 = Self::display(*a);
                format!("&{}", &s1)
            }
            _ => panic!("Cannot display"),
        }
    }
}

//implement display for the lambda data type
impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Lambda::display(self.clone()))
    }
}
