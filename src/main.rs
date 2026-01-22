use easy_lambda_calculus::*;

//code to evaluate a(t, t)
fn main() {
    let t = lambda!("%x|y.x"); //true
    let f = lambda!("%x|y.y"); //false
    let a = lambda!("%x|y.(x y) &{}", f); //and
    let res = lambda!("({} &{}) &{}", a, t.clone(), t); //and(true, true)
    println!("{}", res.evaluate());
}
//outputs (%x|y.x) which is equivalent to true
