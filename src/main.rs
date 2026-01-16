use dyn_fmt::AsStrFormatExt;
use std::collections::HashMap;
use std::fmt;
use unicode_segmentation::UnicodeSegmentation;

//Alphabet for variable naming
static ALPH: &str = "xyzwabcdefghijklmnopqrstuv";

//Lambda data type
#[derive(Debug, PartialEq, Clone)]
pub enum Lambda {
    Func((Box<Lambda>, Box<Lambda>)),
    Variable(String),
    Reducible((Box<Lambda>, Box<Lambda>)),
    AlphaMark(Box<Lambda>),
    StVec(Vec<String>),
    Brack(Vec<Lambda>),
    TFunc(Vec<String>),
    AttPl(()),
}

impl Lambda {
    //new lambda from string
    pub fn new(s: &str) -> Lambda {
        let mut chars: Vec<String> = Vec::new();
        let graphemes = UnicodeSegmentation::graphemes(s, true);
        for g in graphemes {
            chars.push(g.to_string())
        }
        let bracks = Self::find_bracks(chars.clone(), false);
        let tokens = Self::parse_bracks(bracks.clone());
        Self::parse_tokens(tokens)
    }
    //new lambda from formatted string
    pub fn newf(s: &str, f: Vec<Lambda>) -> Lambda {
        let a = f.iter().map(|x| format!("{}", x)).collect::<Vec<String>>();
        Self::new(&s.format(&a))
    }
    //order characters by brackets
    fn find_bracks(chars: Vec<String>, alph: bool) -> Lambda {
        //find brackets
        let mut starts: Vec<usize> = Vec::new();
        let mut ends: Vec<usize> = Vec::new();
        let mut alphas: Vec<usize> = Vec::new();
        let mut count = 0;
        for (i, char) in chars.iter().enumerate() {
            if char == ")" && count == 1 {
                ends.push(i);
                count -= 1;
            } else if char == ")" && count == 0 {
                panic!("Unclosed bracket");
            } else if char == ")" {
                count -= 1;
            }
            if char == "(" && count == 0 {
                if i != 0 && chars[i - 1] == "&" {
                    alphas.push(i)
                }
                starts.push(i);
                count += 1;
            } else if char == "(" {
                count += 1;
            }
        }
        if starts.len() != ends.len() {
            panic!("Unclosed bracket");
        }
        //split the string into bracket tokens
        if !starts.is_empty() {
            let mut bracks: Vec<Lambda> = Vec::new();
            if !chars[..starts[0]].to_vec().is_empty() {
                bracks.push(Lambda::StVec(chars[..starts[0]].to_vec()));
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
                    bracks.push(Lambda::StVec(chars[ends[i] + 1..starts[i + 1]].to_vec()));
                } else if !chars[ends[i] + 1..].to_vec().is_empty() {
                    bracks.push(Lambda::StVec(chars[ends[i] + 1..].to_vec()));
                }
            }

            //mark brackets for alpha reduction
            if alph {
                Lambda::AlphaMark(Box::new(Lambda::Brack(bracks)))
            } else {
                Lambda::Brack(bracks)
            }
        } else if alph {
            Lambda::AlphaMark(Box::new(Lambda::Brack(vec![Lambda::StVec(chars)])))
        } else {
            Lambda::Brack(vec![Lambda::StVec(chars)])
        }
    }
    //parse the brackets and characters into brackets and tokens
    fn parse_bracks(brs: Lambda) -> Lambda {
        if let Lambda::Brack(br) = brs {
            let mut parse_vec: Vec<Lambda> = Vec::new();
            for b in br {
                if let Lambda::Brack(_) = &b {
                    let t = Self::parse_bracks(b);
                    if let Lambda::Brack(v) = &t
                        && v.len() == 1
                    {
                        parse_vec.push(v[0].clone());
                    } else {
                        parse_vec.push(t);
                    }
                } else if let Lambda::StVec(v) = &b {
                    let t = Self::parse_stvec(v.clone());
                    parse_vec.extend_from_slice(&t[..]);
                } else if let Lambda::AlphaMark(l) = &b {
                    parse_vec.push(Lambda::AlphaMark(Box::new(Self::parse_bracks(*l.clone()))));
                } else {
                    panic!("Syntax error")
                }
            }
            Lambda::Brack(parse_vec)
        } else {
            panic!("Syntax error")
        }
    }
    //turn the characters into tokens
    fn parse_stvec(strs: Vec<String>) -> Vec<Lambda> {
        let mut token_vec: Vec<Lambda> = Vec::new();
        let mut pass_num = 0;
        for i in 0..strs.len() {
            if pass_num > 0 {
                pass_num -= 1;
            } else {
                match strs[i].as_str() {
                    "%" => {
                        let mut val_vec: Vec<String> = Vec::new();
                        let mut var: String = "".to_string();
                        for st in strs[i + 1..].iter() {
                            match st.as_str() {
                                "." => {
                                    val_vec.push(var);
                                    token_vec.push(Lambda::TFunc(val_vec));
                                    pass_num += 1;
                                    break;
                                }
                                "|" => {
                                    val_vec.push(var);
                                    var = "".to_string();
                                }
                                _ => {
                                    if ALPH.contains(st) {
                                        var.push_str(st);
                                    } else {
                                        panic!("Invalid syntax");
                                    }
                                }
                            }
                            pass_num += 1;
                        }
                    }
                    _ => {
                        for j in i..strs.len() {
                            match strs[j].as_str() {
                                " " => {
                                    token_vec.push(Lambda::AttPl(()));
                                }
                                "&" => {}
                                _ => {
                                    if ALPH.contains(&strs[j]) {
                                        let mut var: String = "".to_string();
                                        for k in j..strs.len() {
                                            if ALPH.contains(&strs[k]) {
                                                var.push_str(&strs[k]);
                                                if k == strs.len() - 1 {
                                                    token_vec.push(Lambda::Variable(var));
                                                    pass_num += 1;
                                                    break;
                                                }
                                            } else if k == strs.len() {
                                                println!("{}", var.clone());
                                            } else {
                                                token_vec.push(Lambda::Variable(var));
                                                pass_num += 1;
                                                break;
                                            }
                                            pass_num += 1;
                                        }
                                    } else {
                                        panic!("Invalid syntax");
                                    }
                                }
                            }
                        }
                        pass_num += 1;
                    }
                }
            }
        }
        token_vec
    }
    //turn brackets and tokens into a lambda
    fn parse_tokens(token: Lambda) -> Lambda {
        if let Lambda::Brack(v) = token {
            Self::parse_token_vec(v)
        } else if let Lambda::Variable(_) = token {
            token
        } else if let Lambda::AlphaMark(l) = token {
            Lambda::AlphaMark(Box::new(Self::parse_tokens(*l)))
        } else if let Lambda::Reducible((a, b)) = token {
            Self::parse_tokens(*a).attach(Self::parse_tokens(*b))
        } else if let Lambda::Func((a, b)) = token {
            Lambda::Func((a, Box::new(Self::parse_tokens(*b))))
        } else {
            panic!("syntax error")
        }
    }
    //turn a vec of tokens into a lambda
    fn parse_token_vec(mut vec: Vec<Lambda>) -> Lambda {
        let temp_vec = vec.clone();
        for (i, l) in temp_vec.iter().enumerate() {
            if let Lambda::AttPl(_) = *l {
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
        if let Lambda::TFunc(v) = vec[0].clone() {
            Self::parse_func_token(v, vec)
        } else if let Lambda::Brack(_) = vec[0].clone() {
            Self::parse_tokens(vec[0].clone())
        } else if let Lambda::Reducible(_) = vec[0].clone() {
            Self::parse_tokens(vec[0].clone())
        } else if let Lambda::Variable(_) = &vec[0].clone() {
            vec[0].clone()
        } else {
            panic!("{:?}", vec);
        }
    }
    //turn the shorthand function token into lambda functions
    fn parse_func_token(vec: Vec<String>, tokens: Vec<Lambda>) -> Lambda {
        if vec.is_empty() {
            if !tokens.is_empty() {
                Self::parse_token_vec(tokens[1..tokens.len()].to_vec())
            } else {
                panic!("Syntax error");
            }
        } else {
            Lambda::func(
                vec[0].as_str(),
                Self::parse_func_token(vec[1..vec.len()].to_vec(), tokens),
            )
        }
    }
    //make new function variant with a string and a Lambda
    pub fn func(a: &str, b: Lambda) -> Lambda {
        Lambda::Func((Box::new(Lambda::var(a)), Box::new(b)))
    }
    //make new variable variant with a string
    pub fn var(inp: &str) -> Lambda {
        Lambda::Variable(inp.to_string())
    }
    //make new reductible variant by attaching an input Lambda to a Lambda
    pub fn attach(self, a: Lambda) -> Lambda {
        Lambda::Reducible((Box::new(self), Box::new(a)))
    }
    //mark a lambda for alpha reduction
    pub fn alpha(self) -> Lambda {
        Lambda::AlphaMark(Box::new(self))
    }
    //beta reduce a reducible
    pub fn reduce(self) -> Lambda {
        if let Lambda::Reducible((a, b)) = self.clone() {
            //if reducible has a function reduce the function, else if it has a reducible, reduce the inside reducible
            if let Lambda::Func((c, d)) = &*a {
                Lambda::recursive_reduce(*d.clone(), *c.clone(), *b)
            } else if let Lambda::Reducible(_) = &*a {
                a.reduce().attach(*b)
            } else {
                panic!("Cannot reduce");
            }
        } else {
            panic!("Cannot reduce");
        }
    }
    //alpha reduce a lambda
    pub fn alpha_reduce(self) -> Lambda {
        let (m, _, _) = Self::set_map(self.clone(), vec![HashMap::new()], 0, 0, 0);
        let (out, _) = Self::recursive_alpha(self, m, 0, 0);
        out
    }
    //recursive function to substitute every instance of the given variable
    fn recursive_reduce(b: Lambda, a: Lambda, sub: Lambda) -> Lambda {
        //if it is a function variant, check for the variable being substituted or reduce an inside function
        if let Lambda::Func((c, d)) = b {
            if a == *d {
                Lambda::Func((c, Box::new(sub)))
            } else {
                Lambda::Func((c, Box::new(Lambda::recursive_reduce(*d, a, sub))))
            }
        //if it is a reducible, reduce both the function and the input expression
        } else if let Lambda::Reducible((c, d)) = b {
            Lambda::recursive_reduce(*c, a.clone(), sub.clone())
                .attach(Lambda::recursive_reduce(*d, a, sub))
        //if it is just a variable, substitute if it is the variable being substituted
        } else if let Lambda::Variable(_) = &b {
            if a == b { sub } else { b }
        } else if let Lambda::AlphaMark(_) = b {
            b
        } else {
            panic!("Cannot reduce");
        }
    }
    //function to evaluate lambda
    pub fn evaluate(self) -> Lambda {
        self.alpha_reduce().recursive_evaluate().alpha_reduce()
    }
    //function to recursively reduce every reducible
    fn recursive_evaluate(self) -> Lambda {
        if let Lambda::Reducible(_) = self {
            self.reduce().recursive_evaluate()
        } else {
            self
        }
    }
    //function to assign a vector of hashmaps to a lambda
    pub fn set_map(
        l: Lambda,
        mut m: Vec<HashMap<String, usize>>,
        mut i: usize,
        al: usize,
        mut al_in: usize,
    ) -> (Vec<HashMap<String, usize>>, usize, usize) {
        if let Lambda::Variable(a) = l {
            if !m[al].contains_key(a.as_str()) {
                m[al].insert(a, i);
                (m, i + 1, al_in)
            } else {
                (m, i, al_in)
            }
        } else if let Lambda::Func((a, b)) = l {
            if let Lambda::Variable(c) = *a
                && !m[al].contains_key(c.as_str())
            {
                m[al].insert(c, i);
            }
            Self::set_map(*b, m, i + 1, al, al_in)
        } else if let Lambda::Reducible((a, b)) = l {
            (m, i, al_in) = Self::set_map(*a, m, i, al, al_in);
            Self::set_map(*b, m, i, al, al_in)
        } else if let Lambda::AlphaMark(a) = l {
            al_in += 1;
            m.push(HashMap::new());
            Self::set_map(*a, m, i, al_in, al_in)
        } else {
            panic!("Cannot map lambda");
        }
    }
    //function to get a lambda variable name from an integer
    pub fn get_name(mut n: usize) -> String {
        let mut out = "".to_string();
        let chars = UnicodeSegmentation::graphemes(ALPH, true);
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
        if let Lambda::Variable(a) = l {
            match m[al].get(&a) {
                Some(b) => (Lambda::Variable(Self::get_name(*b)), al_in),
                _ => panic!("Cannot alpha reduce"),
            }
        } else if let Lambda::Func((a, b)) = l {
            let c: Lambda;
            let d: Lambda;
            (c, al_in) = Self::recursive_alpha(*a, m.clone(), al, al_in);
            (d, al_in) = Self::recursive_alpha(*b, m, al, al_in);
            (Lambda::Func((Box::new(c), Box::new(d))), al_in)
        } else if let Lambda::Reducible((a, b)) = l {
            let c: Lambda;
            let d: Lambda;
            (c, al_in) = Self::recursive_alpha(*a, m.clone(), al, al_in);
            (d, al_in) = Self::recursive_alpha(*b, m, al, al_in);
            (c.attach(d), al_in)
        } else if let Lambda::AlphaMark(a) = l {
            al_in += 1;
            Self::recursive_alpha(*a, m, al_in, al_in)
        } else {
            panic!("Cannot alpha reduce");
        }
    }
    //function to calculate a string to represent the lambda
    fn display(l: Lambda) -> String {
        if let Lambda::Variable(a) = l {
            a
        } else if let Lambda::Func((a, mut b)) = l {
            let d = b.clone();
            let mut s1 = Self::display(*a);
            let mut s2 = "".to_string();
            while let Lambda::Func((ref a, ref c)) = *b {
                s1.push('|');
                s1.push_str(&Self::display(*a.clone()));
                if let Lambda::Func(_) = **c {
                } else {
                    s2 = Self::display(*c.clone());
                }
                b = c.clone();
            }
            if s2.is_empty() {
                s2 = Self::display(*d)
            }
            let mut s3 = "(%".to_string();
            s3.push_str(&s1);
            s3.push('.');
            s3.push_str(&s2);
            s3.push(')');
            s3
        } else if let Lambda::Reducible((a, b)) = l {
            let s1 = Self::display(*a);
            let s2 = Self::display(*b);
            let mut s3 = "(".to_string();
            s3.push_str(&s1);
            s3.push(' ');
            s3.push_str(&s2);
            s3.push(')');
            s3
        } else if let Lambda::AlphaMark(a) = l {
            let s1 = Self::display(*a);
            let mut s2 = "&".to_string();
            s2.push_str(&s1);
            s2
        } else {
            panic!("Cannot display");
        }
    }
}

//implement display for the lambda data type
impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Lambda::display(self.clone()))
    }
}

//code to evaluate a(t, t)
fn main() {
    let t = Lambda::new("%x|y.x"); //true
    let f = Lambda::new("%x|y.y"); //false
    let a = Lambda::newf("%x|y.(x y) &{}", vec![f.clone()]); //and
    let res = Lambda::newf("({} &{}) &{}", vec![a, t.clone(), t]); //and(true, true)
    println!("{}", res.evaluate());
}
//outputs (%x|y.x) which is equivalent to true
