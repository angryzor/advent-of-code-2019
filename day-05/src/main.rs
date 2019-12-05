use std::fs::File;
use std::io::{BufRead,Read,Write};

struct InstructionResult {
    jump: Option<usize>,
    new_params: Vec<i32>,
}

#[derive(Copy, Clone)]
enum ParameterMode {
    Position,
    Immediate,
}

type Operation<'a> = Box<dyn FnMut(&[i32]) -> InstructionResult + 'a>;

struct Instruction<'a> {
    parameter_count: usize,
    operation: Operation<'a>,
}

struct Opcode<'a> {
    instruction: Instruction<'a>,
    parameter_modes: Vec<ParameterMode>,
}

fn arithmetic_instruction<'a, F: FnMut(i32, i32) -> i32 + 'a>(mut func: F) -> Instruction<'a> {
    Instruction {
        parameter_count: 3,
        operation: Box::new(move |params| InstructionResult {
            jump: None,
            new_params: vec![params[0], params[1], func(params[0], params[1])]
        }),
    }
}

fn jmp_instruction<'a, F: FnMut(i32) -> bool + 'a>(mut func: F) -> Instruction<'a> {
    Instruction {
        parameter_count: 2,
        operation: Box::new(move |params| InstructionResult {
            jump: if func(params[0]) { Some(params[1] as usize) } else { None },
            new_params: params.to_vec(),
        }),
    }
}

fn cmp_instruction<'a, F: FnMut(i32, i32) -> bool + 'a>(mut func: F) -> Instruction<'a> {
    Instruction {
        parameter_count: 3,
        operation: Box::new(move |params| InstructionResult {
            jump: None,
            new_params: vec![params[0], params[1], if func(params[0], params[1]) { 1 } else { 0 }],
        }),
    }
}

fn read_int(_params: &[i32]) -> InstructionResult {
    print!("Please enter input: ");
    std::io::stdout().flush().ok().expect("Could not flush stdout");

    let line = std::io::stdin().lock().lines().next().unwrap().unwrap();

    InstructionResult {
        jump: None,
        new_params: vec![line.parse().unwrap()],
    }
}

fn print_int(params: &[i32]) -> InstructionResult {
    print!("<{}>", params[0]);
    std::io::stdout().flush().ok().expect("Could not flush stdout");

    InstructionResult {
        jump: None,
        new_params: params.to_vec(),
    }
}

fn decode_instruction<'a>(raw_opcode: i32) -> Instruction<'a> {
    match raw_opcode {
        1 => arithmetic_instruction(|a,b| a + b),
        2 => arithmetic_instruction(|a,b| a * b),
        3 => Instruction { parameter_count: 1, operation: Box::new(read_int) },
        4 => Instruction { parameter_count: 1, operation: Box::new(print_int) },
        5 => jmp_instruction(|a| a != 0),
        6 => jmp_instruction(|a| a == 0),
        7 => cmp_instruction(|a,b| a < b),
        8 => cmp_instruction(|a,b| a == b),
        _ => panic!()
    }
}

fn decode_parameter_mode(raw_mode: i32) -> ParameterMode {
    match raw_mode {
        0 => ParameterMode::Position,
        1 => ParameterMode::Immediate,
        _ => panic!(),
    }
}

fn decode_opcode<'a>(raw_code: i32) -> Opcode<'a> {
    let mut raw_param_modes = raw_code / 100;
    let mut parameter_modes: Vec<ParameterMode> = vec![];

    while raw_param_modes != 0 {
        parameter_modes.push(decode_parameter_mode(raw_param_modes % 10));
        raw_param_modes = raw_param_modes / 10
    }

    return Opcode {
        instruction: decode_instruction(raw_code % 100),
        parameter_modes,
    }
}

fn run_opcode(program: &mut [i32], pc: usize) -> usize {
    let raw_opcode = program[pc];
    let opcode = decode_opcode(raw_opcode);

    let param_modes: Vec<ParameterMode> = (0..opcode.instruction.parameter_count).map(|i| *opcode.parameter_modes.get(i).unwrap_or(&ParameterMode::Position)).collect();
    let param_addresses: Vec<usize> = param_modes.iter().enumerate().map(|(i, mode)| {
        match mode {
            ParameterMode::Position => program[pc+i+1] as usize,
            ParameterMode::Immediate => pc+i+1,
        }
    }).collect();

    let params: Vec<i32> = param_addresses.iter().map(|&address| program[address]).collect();

    let mut operation = opcode.instruction.operation;

    let result = operation(&params);

    for (&address, &new_param) in param_addresses.iter().zip(result.new_params.iter()) {
        program[address] = new_param;
    }

    match result.jump {
        None => pc + opcode.instruction.parameter_count + 1,
        Some(x) => x
    }
}


fn simulate(program: &mut [i32]) {
    let mut pc = 0;

    loop {
        match program[pc] {
            99 => return,
            _ => pc = run_opcode(program, pc)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simulate() {
        let mut case1 = vec![1,0,0,0,99];
        let mut case2 = vec![2,3,0,3,99];
        let mut case3 = vec![2,4,4,5,99,0];
        let mut case4 = vec![1,1,1,4,99,5,6,0,99];

        simulate(&mut case1);
        simulate(&mut case2);
        simulate(&mut case3);
        simulate(&mut case4);

        assert_eq!(case1, [2,0,0,0,99]);
        assert_eq!(case2, [2,3,0,6,99]);
        assert_eq!(case3, [2,4,4,5,99,9801]);
        assert_eq!(case4, [30,1,1,4,2,5,6,0,99]);
    }
}

fn run(program: &mut [i32], noun: i32, verb: i32) -> i32 {
    program[1] = noun;
    program[2] = verb;

    simulate(program);

    program[0]
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buffer = String::new();
    let mut file = File::open("input.txt")?;

    file.read_to_string(&mut buffer)?;

    let program = buffer.trim().split(",").map(|s| s.parse()).collect::<Result<Vec<_>, _>>()?;

    simulate(&mut program.clone());

    Ok(())
}
