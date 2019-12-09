use std::fs::File;
use std::io::Read;
use std::sync::mpsc::channel;
use std::thread;
use std::cmp::max;
use itertools::Itertools;

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buffer = String::new();
    let mut file = File::open("input.txt")?;

    file.read_to_string(&mut buffer)?;

    let program = buffer.trim().split(",").map(|s| s.parse()).collect::<Result<Vec<_>, _>>()?;
    // let messages: RefCell<VecDeque<i32>> = RefCell::new(VecDeque::new());

    // let mut max_value: i32 = 0;

    // for phase_setting in (0..5).permutations(5) {
    //     messages.borrow_mut().push_back(0);

    //     for i in 0..5 {
    //         messages.borrow_mut().push_front(phase_setting[i]);
    //         simulate(
    //             &mut program.clone(),
    //             | | { println!("reading for {}", i); messages.borrow_mut().pop_front().unwrap() },
    //             |x| messages.borrow_mut().push_back(x)
    //         );
    //     }

    //     max_value = max(messages.borrow_mut().pop_front().unwrap(), max_value);
    // }

    // println!("{}", max_value);


    // let mut channels: Vec<(Sender<i32>, Receiver<i32>)> = (0..=5).map(|_| channel()).collect();

    let mut max_value: i32 = 0;

    for phase_setting in (5..=9).permutations(5) {
        let (first_sender, first_receiver) = channel();
        let mut current_receiver = first_receiver;

        let mut children = vec![];

        for i in 0..5 {
            let mut prog = program.clone();
            let (new_sender, new_receiver) = channel();
            let input = current_receiver;
            let output = new_sender;
            current_receiver = new_receiver;

            let my_first = first_sender.clone();

            if i != 4 {
                output.send(phase_setting[i + 1]).unwrap();
            }

            children.push(thread::spawn(move|| {
                simulate(
                    &mut prog,
                    || input.recv().unwrap(),
                    |x| {
                        output.send(x).unwrap();

                        if i == 4 {
                            match my_first.send(x) {
                                Ok(_) => (),
                                Err(what) => (), // Probably finished processing.
                            }
                        }
                    }
                );
            }));
        }

        first_sender.send(phase_setting[0]).unwrap();
        first_sender.send(0).unwrap();

        for child in children {
            child.join().unwrap();
        }

        let mut end_value = 0;

        for value in current_receiver.iter() {
            end_value = value;
        }

        max_value = max(end_value, max_value);
    }

    println!("{}", max_value);

    Ok(())
}
