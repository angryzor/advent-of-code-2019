use std::fs::File;
use std::io::{BufRead,Read,Write};

#[derive(Copy, Clone)]
enum ParameterMode {
    Position,
    Immediate,
}

type Parameter = usize;

#[derive(Copy, Clone)]
enum Instruction {
    Add(Parameter, Parameter, Parameter),
    Multiply(Parameter, Parameter, Parameter),
    Input(Parameter),
    Output(Parameter),
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Parameter),
    Equal(Parameter, Parameter, Parameter),
    Halt,
}

fn get_param_count(instr: Instruction) -> usize {
    match instr {
        Instruction::Add(_, _, _) => 3,
        Instruction::Multiply(_, _, _) => 3,
        Instruction::Input(_) => 1,
        Instruction::Output(_) => 1,
        Instruction::JumpIfTrue(_, _) => 2,
        Instruction::JumpIfFalse(_, _) => 2,
        Instruction::LessThan(_, _, _) => 3,
        Instruction::Equal(_, _, _) => 3,
        Instruction::Halt => 0,
    }
}

fn read_int() -> i32 {
    print!("Please enter input: ");
    std::io::stdout().flush().ok().expect("Could not flush stdout");

    let line = std::io::stdin().lock().lines().next().unwrap().unwrap();

    line.parse().unwrap()
}

fn print_int(value: i32) {
    print!("<{}>", value);
    std::io::stdout().flush().ok().expect("Could not flush stdout");
}

fn decode_parameter_mode(raw_mode: i32) -> ParameterMode {
    match raw_mode {
        0 => ParameterMode::Position,
        1 => ParameterMode::Immediate,
        _ => panic!(),
    }
}

fn decode_parameter_modes(raw_opcode: i32) -> Vec<ParameterMode> {
    let mut raw_param_modes = raw_opcode / 100;
    let mut parameter_modes: Vec<ParameterMode> = vec![];

    while raw_param_modes != 0 {
        parameter_modes.push(decode_parameter_mode(raw_param_modes % 10));
        raw_param_modes = raw_param_modes / 10
    }

    parameter_modes
}

fn decode_instruction(program: &[i32], pc: usize) -> Instruction {
    let opcode = program[pc];
    let parameter_modes = decode_parameter_modes(opcode);

    let param = |offset: usize| -> Parameter {
        let mode = parameter_modes.get(offset).unwrap_or(&ParameterMode::Position);

        match mode {
            ParameterMode::Position => program[pc+offset+1] as usize,
            ParameterMode::Immediate => pc+offset+1,
        }
    };

    match opcode % 100 {
        1 => Instruction::Add(param(0), param(1), param(2)),
        2 => Instruction::Multiply(param(0), param(1), param(2)),
        3 => Instruction::Input(param(0)),
        4 => Instruction::Output(param(0)),
        5 => Instruction::JumpIfTrue(param(0), param(1)),
        6 => Instruction::JumpIfFalse(param(0), param(1)),
        7 => Instruction::LessThan(param(0), param(1), param(2)),
        8 => Instruction::Equal(param(0), param(1), param(2)),
        99 => Instruction::Halt,
        _ => panic!(),
    }
}

fn execute_instruction<I: FnMut() -> i32, O: FnMut(i32) -> ()>(program: &mut [i32], pc: &mut usize, instr: Instruction, input: &mut I, output: &mut O) {
    *pc += 1 + get_param_count(instr);

    match instr {
        Instruction::Add(op1, op2, dest) => { program[dest] = program[op1] + program[op2]; },
        Instruction::Multiply(op1, op2, dest) => { program[dest] = program[op1] * program[op2]; },
        Instruction::Input(dest) => { program[dest] = input(); },
        Instruction::Output(value) => { output(program[value]) },
        Instruction::JumpIfTrue(value, target) => { if program[value] != 0 { *pc = program[target] as usize; } },
        Instruction::JumpIfFalse(value, target) => { if program[value] == 0 { *pc = program[target] as usize; } },
        Instruction::LessThan(op1, op2, dest) => { program[dest] = if program[op1] < program[op2] { 1 } else { 0 } },
        Instruction::Equal(op1, op2, dest) => { program[dest] = if program[op1] == program[op2] { 1 } else { 0 } },
        Instruction::Halt => panic!(),
    }
}

fn simulate<I: FnMut() -> i32, O: FnMut(i32) -> ()>(program: &mut [i32], mut input: I, mut output: O) {
    let mut pc = 0;

    loop {
        let instr = decode_instruction(program, pc);

        match instr {
            Instruction::Halt => return,
            _ => {
                execute_instruction(program, &mut pc, instr, &mut input, &mut output);
            }
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buffer = String::new();
    let mut file = File::open("input.txt")?;

    file.read_to_string(&mut buffer)?;

    let program = buffer.trim().split(",").map(|s| s.parse()).collect::<Result<Vec<_>, _>>()?;

    simulate(&mut program.clone(), read_int, print_int);

    Ok(())
}
