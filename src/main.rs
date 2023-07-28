use std::fmt;
use std::collections::HashMap;
use std::io::{self, BufRead};

#[derive(Clone)]
pub struct Matrix<T>{
    data : Vec<T>,
    N : usize,
    M : usize
}

impl Matrix<f32>{
    pub fn add(self,other : &Matrix<f32>) -> Matrix<f32>{
        assert_eq!(self.N,other.N );
        assert_eq!(self.M,other.M );
        let mut copy = self;
        for (ptrc,ptro) in copy.data.iter_mut().zip(other.data.iter()) {
            *ptrc += *ptro;
        }
        copy
    }
    pub fn sub(self,other : &Matrix<f32>) -> Matrix<f32>{
        assert_eq!(self.N,other.N );
        assert_eq!(self.M,other.M );
        let mut copy = self;
        for (ptrc,ptro) in copy.data.iter_mut().zip(other.data.iter()) {
            *ptrc -= *ptro;
        }
        copy
    }
    pub fn mul(self,other : &Matrix<f32>) -> Matrix<f32>{
        assert_eq!(self.M,other.N);
        let mut result = Matrix{
                    data: vec![0.0;self.N*other.M],
                    N : self.N,
                    M : other.M      
                    };
        for i in 0..self.N {
            for j in 0..other.M {
                let mut sum = 0.0;
                for k in 0..self.M{
                    sum += self.data[i*self.M+k] * other.data[k*other.M+j];
                }
                result.data[i*result.M+j] = sum;
            }
        }
        result
    }
}

impl fmt::Display for Matrix<f32> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut dispaly_data = "|".to_owned();
        for i in 0..self.data.len() {
            dispaly_data += &self.data[i].to_string();
            if (i+1)%self.M == 0 { dispaly_data += "|\n|"; }
            else {dispaly_data += " ";}
        }
        dispaly_data.pop();
        write!(f, "{}", dispaly_data)
    }
}

fn create_matrix(string:&str) -> Matrix<f32>{
    let mut tmp: Vec<f32> = Vec::new();
    let mut numtmp = "".to_owned();
    let mut n : usize= 1;
    for i in string.chars(){
        match i {
            '0'..='9' | '.' | '-'=> numtmp.push(i),
            ' ' | ';' | ']' => {
                if !numtmp.is_empty(){
                    tmp.push(numtmp.parse::<f32>().unwrap());
                    numtmp.clear();
                }
                if i == ';' {n+=1;}
            }
            _=>{}
        }
    }
    let m : usize= tmp.len()/n;
    return Matrix{
        data : tmp,
        N : n,
        M : m
    }
}
#[derive(Clone)]
pub enum VariableType{
    Matrix(Matrix<f32>),
    Scalar(f32),
    String(String)
}

pub struct Interpreter{
    variable_vector : HashMap<String,VariableType>
}

#[derive(PartialEq,Clone)]
pub enum Token{
    Var{name: String},
    MatrixDec{data: String},
    ScalarDec{data: String},
    StringDec{data: String},
    Print,
    Dec,
    Add,
    Sub,
    Eq,
    Lq,
    Bq,
    Sort,
    Mul,
    Div,
    Mod,
    Map,
    None
}


#[derive(Clone)]
pub struct ParserTree{
    Type : Token,
    Left : Option<Box<ParserTree>>,
    Right : Option<Box<ParserTree>>
}

pub enum EvalReturn{
    Value(VariableType),
    Error(String),
    None
}

type binop = fn(VariableType,&VariableType) -> EvalReturn;

impl Interpreter{
    fn tokenizer(string:&str) -> Vec<Token>{
        let mut token_list:Vec<Token> = Vec::new();
        let mut i =  string.chars();
        while let Some(c) = i.next(){
            match c{
                'a'..='z' | 'A'..='Z' => {
                    let mut tmp_name = "".to_owned();
                    tmp_name.push(c);
                    while let Some(_c) = i.next(){
                        match _c{
                            ' ' => break,
                            _ => tmp_name.push(_c)
                        }
                    }
                    token_list.push(
                        Token::Var{
                            name : tmp_name
                        }
                    );
                }
                '\"' => {
                    let mut tmp_name = "".to_owned();
                    while let Some(_c) = i.next(){
                        match _c{
                            '\"' => break,
                            _ => tmp_name.push(_c)
                        }
                    }
                    token_list.push(
                        Token::StringDec{
                            data : tmp_name
                        }
                    );
                }
                ':' => token_list.push(Token::Dec),
                '=' => token_list.push(Token::Eq),
                '+' => token_list.push(Token::Add),
                '-' => token_list.push(Token::Sub),
                '#' => token_list.push(Token::Map),
                '>' => token_list.push(Token::Bq),
                '<' => token_list.push(Token::Lq),
                '$' => token_list.push(Token::Sort),
                '%'  => token_list.push(Token::Mod),
                '*'  => token_list.push(Token::Mul),
                '/'  => token_list.push(Token::Div),
                '[' =>{
                    let mut tmp_mdd = "".to_owned();
                    tmp_mdd.push(c);
                    while let Some(c1) = i.next(){
                        match c1{
                            ']' => {
                                tmp_mdd.push(c1);
                                break
                            }
                            _ => tmp_mdd.push(c1)
                        }
                    }
                    token_list.push(
                        Token::MatrixDec{
                            data : tmp_mdd
                        }
                    );
                }
                '0'..='9' | '.'  => {
                    let mut tmp_v = "".to_owned();
                    tmp_v.push(c);
                    while let Some(_c) = i.next(){
                        match _c{
                            '0'..='9' | '.'  => tmp_v.push(_c),
                            _ => break
                        }
                    }
                    token_list.push(
                        Token::ScalarDec{
                            data : tmp_v
                        }
                    );
                }
                '\'' => token_list.push(Token::Print),
                _ => {}
            }
        }
        token_list.push(Token::None);
        token_list
    }
    fn parser(token_list : Vec<Token>) -> ParserTree {
        let mut tree = ParserTree{
                        Type: Token::None,
                        Left: None,
                        Right: None
        };
        let token = &token_list[0];
        match token{
            Token::Var{name} => tree.Type = Token::Var{name : name.to_string()},
            Token::Dec|Token::Map|Token::Mod|Token::Div|Token::Sort|Token::Mul|Token::Add|Token::Sub => {
                        tree.Type = token.clone();
                        tree.Left = Some(Box::new(Self::parser(token_list[1..].to_vec())));
                        tree.Right = Some(Box::new(Self::parser(token_list[2..].to_vec())))
            } 
            Token::Eq | Token::Bq | Token::Lq=> {
                tree.Type = token.clone();
                tree.Left = Some(Box::new(Self::parser(token_list[1..].to_vec())));
                if token_list.len() > 2 {tree.Right = Some(Box::new(Self::parser(token_list[2..].to_vec())))}
            }
            Token::Print => {
                tree.Type = Token::Print;
                tree.Left = Some(Box::new(Self::parser(token_list[1..].to_vec())));
            }
            Token::MatrixDec{data: _} | Token::ScalarDec{data: _} | Token::StringDec{data: _} => tree.Type = token.clone(),
            _=> {}
        }
        tree
    }
    fn print(value:&VariableType)->(){
        match value{
            VariableType::Matrix(value) => println!("{value}"),
            VariableType::Scalar(value) => println!("{value}"),
            VariableType::String(value) => println!("{value}")
        }
    }
    fn add(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(value) => {
                if let VariableType::Matrix(value2) = value2{
                    EvalReturn::Value(VariableType::Matrix(value.add(&value2)))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar(value + value2))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"+\" operator".to_string())
        }
    }
    fn mul(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(value) => {
                if let VariableType::Matrix(value2) = value2{
                    EvalReturn::Value(VariableType::Matrix(value.mul(&value2)))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar(value * value2))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"*\" operator".to_string())
        }
    }
    
    fn sub(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(value) => {
                if let VariableType::Matrix(value2) = value2{
                    EvalReturn::Value(VariableType::Matrix(value.sub(&value2)))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar(value - value2))
                }
                else {EvalReturn::Error("Inncorect data type".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"-\" operator".to_string())
        }
    }
    fn div(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(_value) => {
                EvalReturn::Error("Matrix doesnt implement \"\\\" operator".to_string())
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar(value / value2))
                }
                else {EvalReturn::Error("Inncorect data type during \"\\\"".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"\\\" operator".to_string())
        }
    }
    fn eq(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(_value) => {
                EvalReturn::Error("Matrix doesnt implement \"=\" operator".to_string())
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar((value == *value2) as i32 as f32))
                }
                else {EvalReturn::Error("Inncorect data type during \"=\"".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"=\" operator".to_string())
        }
    }
    fn bq(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(_value) => {
                EvalReturn::Error("Matrix doesnt implement \"=\" operator".to_string())
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar((value > *value2) as i32 as f32))
                }
                else {EvalReturn::Error("Inncorect data type during \"=\"".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"=\" operator".to_string())
        }
    }
    fn lq(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(_value) => {
                EvalReturn::Error("Matrix doesnt implement \"=\" operator".to_string())
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar((value < *value2) as i32 as f32))
                }
                else {EvalReturn::Error("Inncorect data type during \"=\"".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"=\" operator".to_string())
        }
    }
    fn modl(value1:VariableType,value2:&VariableType) -> EvalReturn{
        match value1 {
            VariableType::Matrix(_value) => {
                EvalReturn::Error("Matrix doesnt implement \"%\" operator".to_string())
            },
            VariableType::Scalar(value) => {
                if let VariableType::Scalar(value2) = value2{
                    EvalReturn::Value(VariableType::Scalar(value % value2))
                }
                else {EvalReturn::Error("Inncorect data type during \"%\"".to_string())}
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"%\" operator".to_string())
        }
    }
    fn map(value:VariableType,function:binop,arg:&VariableType) -> EvalReturn{
        if let VariableType::Matrix(matrix) = value{
            let mut copy:Matrix<f32> = matrix.clone();
            for ptrc in copy.data.iter_mut() {
                if let EvalReturn::Value(VariableType::Scalar(value)) = function(VariableType::Scalar(*ptrc),&arg){
                    *ptrc = value;
                }
                else {return EvalReturn::Error("function in \"#\" return non-scalar data type".to_owned())}
            }
            EvalReturn::Value(VariableType::Matrix(copy))
        }
        else {EvalReturn::Error("trying using \"#\" on no-matrix data type".to_owned())}
    }
    fn sort(value:VariableType,b:bool)->EvalReturn{
        match value {
            VariableType::Matrix(value) => {
                let mut clone = value;
                clone.data.sort_by(|a, b| a.partial_cmp(b).unwrap());
                if b { clone.data.reverse(); }
                EvalReturn::Value(VariableType::Matrix(clone))

            },
            VariableType::Scalar(_value) => {
                EvalReturn::Error("Scalar doesnt implement \"=\" operator".to_owned())
            }
            VariableType::String(_value) => EvalReturn::Error("String doesnt implement \"=\" operator".to_owned())
        }
    }
    fn eval(&mut self,tree: ParserTree) ->EvalReturn{
        match tree.Type {
            Token::Dec => {
                if let Token::Var{name} = tree.Left.unwrap().Type{
                    if let EvalReturn::Value(value) = self.eval(*tree.Right.unwrap()){
                        Self::print(&value);
                        self.variable_vector.entry(name.to_string()).and_modify(|k| *k = value.clone()).or_insert(value);
                        EvalReturn::None
                    }
                    else{EvalReturn::Error("Second arg of \":\" is not a Value".to_owned())}
                }
                else {EvalReturn::Error("First arg of \":\" is not a name of variable".to_owned())}
            }
            Token::Eq => {
                if let EvalReturn::Value(value1) = self.eval(*tree.Left.unwrap()){
                    if let EvalReturn::Value(value2) = self.eval(*tree.Right.unwrap()){
                        Self::eq(value1,&value2)
                    }
                    else{EvalReturn::Error("Second arg of \"=\" is not a Value".to_owned())}
                }
                else {EvalReturn::Error("First arg of \"=\" is not a Value".to_owned())}
            }
            Token::Sort => {
                if let EvalReturn::Value(value1) = self.eval(*tree.Left.unwrap()){
                    let ParserTree{Type:function,Left:_,Right:_} = *tree.Right.unwrap();
                    match function{
                        Token::Bq | Token::None => {
                            Self::sort(value1,false)
                        }
                        Token::Lq => {
                            Self::sort(value1,true)
                        }
                        _=>{EvalReturn::Error("Wrong arg for \"$\"".to_owned())}
                    }
                }
                else {EvalReturn::Error("First arg of \"=\" is not a Value".to_owned())}
            }
            Token::Bq => {
                if let EvalReturn::Value(value1) = self.eval(*tree.Left.unwrap()){
                    if let EvalReturn::Value(value2) = self.eval(*tree.Right.unwrap()){
                        Self::bq(value1,&value2)
                    }
                    else{EvalReturn::Error("Second arg of \"=\" is not a Value".to_owned())}
                }
                else {EvalReturn::Error("First arg of \"=\" is not a Value".to_owned())}
            }
            Token::Lq => {
                if let EvalReturn::Value(value1) = self.eval(*tree.Left.unwrap()){
                    if let EvalReturn::Value(value2) = self.eval(*tree.Right.unwrap()){
                        Self::lq(value1,&value2)
                    }
                    else{EvalReturn::Error("Second arg of \"=\" is not a Value".to_owned())}
                }
                else {EvalReturn::Error("First arg of \"=\" is not a Value".to_owned())}
            }
            Token::Map =>{
                if let EvalReturn::Value(value1) = self.eval(*tree.Left.unwrap()){
                    let ParserTree{Type:function,Left:arg,Right:_} = *tree.Right.unwrap();
                    match function{
                        Token::Eq =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::eq,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Bq =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::bq,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Lq =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::lq,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Sub =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::sub,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Add =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::add,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Div =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::div,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Mul =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::mul,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        Token::Mod =>{
                            if let EvalReturn::Value(arg) = self.eval(*arg.unwrap()){
                                Self::map(value1,Self::modl,&arg)
                            }
                            else{EvalReturn::Error("Arg of \"=\" is not a Value".to_owned())}
                        }
                        _=>{EvalReturn::Error("Not match function for \"#\"".to_owned())}
                    }
                }
                else {EvalReturn::Error("First arg of \"#\" is not a Value".to_owned())}
            }
            Token::Print => {
                if let EvalReturn::Value(value) = self.eval(*tree.Left.unwrap()){
                    Self::print(&value);
                    EvalReturn::None
                }
                else {EvalReturn::Error("Cannot print value".to_owned())}
            }
            Token::Add =>{
                if let (EvalReturn::Value(value1),EvalReturn::Value(value2)) = (self.eval(*tree.Left.unwrap()),self.eval(*tree.Right.unwrap())){
                    Self::add(value1,&value2)
                }
                else {EvalReturn::Error("\"+\" args are not values".to_owned())}
            }
            Token::Mul =>{
                if let (EvalReturn::Value(value1),EvalReturn::Value(value2)) = (self.eval(*tree.Left.unwrap()),self.eval(*tree.Right.unwrap())){
                    Self::mul(value1,&value2)
                }
                else {EvalReturn::Error("\"*\" args are not values".to_owned())}
            }
            Token::Div =>{
                if let (EvalReturn::Value(value1),EvalReturn::Value(value2)) = (self.eval(*tree.Left.unwrap()),self.eval(*tree.Right.unwrap())){
                    Self::div(value1,&value2)
                }
                else {EvalReturn::Error("\"*\" args are not values".to_owned())}
            }
            Token::Mod =>{
                if let (EvalReturn::Value(value1),EvalReturn::Value(value2)) = (self.eval(*tree.Left.unwrap()),self.eval(*tree.Right.unwrap())){
                    Self::modl(value1,&value2)
                }
                else {EvalReturn::Error("\"*\" args are not values".to_owned())}
            }
            Token::Sub =>{
                if let (EvalReturn::Value(value1),EvalReturn::Value(value2)) = (self.eval(*tree.Left.unwrap()),self.eval(*tree.Right.unwrap())){
                    Self::sub(value1,&value2)
                }
                else {EvalReturn::Error("\"-\" args are not values".to_owned())}
            }
            Token::Var{name} =>{
                if let Some(var) = self.variable_vector.get(&name){
                    match var {
                        VariableType::Matrix(value) => EvalReturn::Value(VariableType::Matrix(value.clone())),
                        VariableType::Scalar(value) => EvalReturn::Value(VariableType::Scalar(*value)),
                        VariableType::String(value) => EvalReturn::Value(VariableType::String(value.clone()))
                    }
                }
                else {EvalReturn::Error("Variable is not initialized".to_owned())}
            }
            Token::MatrixDec{data} =>{
                EvalReturn::Value(VariableType::Matrix(create_matrix(&data)))
            }
            Token::ScalarDec{data} =>{
                EvalReturn::Value(VariableType::Scalar(data.parse::<f32>().unwrap()))
            }
            Token::StringDec{data} =>{
                EvalReturn::Value(VariableType::String(data))
            }
            _=>EvalReturn::None
        }
    }
    pub fn shell(&mut self,line: &str){
        let token_list = Self::tokenizer(line);
        let tree = Self::parser(token_list);
        if let EvalReturn::Error(string) = self.eval(tree){
            println!("ERROR: {string}");
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let mut interp = Interpreter{variable_vector: HashMap::new()};
    for line in stdin.lock().lines() {
        match line {
            Err(_) => break, 
            Ok(s) => {
                if s == "exit"{
                    break;
                }
                else {interp.shell(&s);}
            }
        }

    }
}