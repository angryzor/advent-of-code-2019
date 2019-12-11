use std::fs::File;
use std::io::{Read, Write, BufRead};
use std::sync::mpsc::{Sender, Receiver, channel};
use std::thread;

#[derive(Copy, Clone)]
enum ParameterMode {
    Position,
    Immediate,
    Relative,
}

type Parameter = usize;

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Add(Parameter, Parameter, Parameter),
    Multiply(Parameter, Parameter, Parameter),
    Input(Parameter),
    Output(Parameter),
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Parameter),
    Equal(Parameter, Parameter, Parameter),
    AdjustRelativeBase(Parameter),
    Halt,
}

#[derive(Debug)]
struct Context {
    pc: usize,
    relative_base: i64,
}

fn read_int() -> i64 {
    print!("Please enter input: ");
    std::io::stdout().flush().ok().expect("Could not flush stdout");

    let line = std::io::stdin().lock().lines().next().unwrap().unwrap();

    line.parse().unwrap()
}

fn print_int(value: i64) {
    print!("<{}>", value);
    std::io::stdout().flush().ok().expect("Could not flush stdout");
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
        Instruction::AdjustRelativeBase(_) => 1,
        Instruction::Halt => 0,
    }
}

fn decode_parameter_mode(raw_mode: i64) -> ParameterMode {
    match raw_mode {
        0 => ParameterMode::Position,
        1 => ParameterMode::Immediate,
        2 => ParameterMode::Relative,
        _ => panic!(),
    }
}

fn decode_parameter_modes(raw_opcode: i64) -> Vec<ParameterMode> {
    let mut raw_param_modes = raw_opcode / 100;
    let mut parameter_modes: Vec<ParameterMode> = vec![];

    while raw_param_modes != 0 {
        parameter_modes.push(decode_parameter_mode(raw_param_modes % 10));
        raw_param_modes = raw_param_modes / 10
    }

    parameter_modes
}

fn decode_instruction(program: &[i64], ctx: &Context) -> Instruction {
    let opcode = program[ctx.pc];
    let parameter_modes = decode_parameter_modes(opcode);

    let param = |offset: usize| -> Parameter {
        let mode = parameter_modes.get(offset).unwrap_or(&ParameterMode::Position);

        match mode {
            ParameterMode::Position => program[ctx.pc+offset+1] as usize,
            ParameterMode::Immediate => ctx.pc+offset+1,
            ParameterMode::Relative => (ctx.relative_base + program[ctx.pc+offset+1]) as usize,
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
        9 => Instruction::AdjustRelativeBase(param(0)),
        99 => Instruction::Halt,
        _ => panic!(),
    }
}

fn execute_instruction<I: FnMut() -> i64, O: FnMut(i64) -> ()>(program: &mut [i64], ctx: &mut Context, instr: Instruction, input: &mut I, output: &mut O) {
    ctx.pc += 1 + get_param_count(instr);

    match instr {
        Instruction::Add(op1, op2, dest) => { program[dest] = program[op1] + program[op2]; },
        Instruction::Multiply(op1, op2, dest) => { program[dest] = program[op1] * program[op2]; },
        Instruction::Input(dest) => { program[dest] = input(); },
        Instruction::Output(value) => { output(program[value]) },
        Instruction::JumpIfTrue(value, target) => { if program[value] != 0 { ctx.pc = program[target] as usize; } },
        Instruction::JumpIfFalse(value, target) => { if program[value] == 0 { ctx.pc = program[target] as usize; } },
        Instruction::LessThan(op1, op2, dest) => { program[dest] = if program[op1] < program[op2] { 1 } else { 0 } },
        Instruction::Equal(op1, op2, dest) => { program[dest] = if program[op1] == program[op2] { 1 } else { 0 } },
        Instruction::AdjustRelativeBase(offset) => { ctx.relative_base += program[offset]; },
        Instruction::Halt => panic!(),
    }
}

fn simulate<I: FnMut() -> i64, O: FnMut(i64) -> ()>(program: &mut [i64], mut input: I, mut output: O) {
    let mut ctx = Context {
        pc: 0,
        relative_base: 0,
    };

    loop {
        let instr = decode_instruction(program, &ctx);

        match instr {
            Instruction::Halt => return,
            _ => {
                execute_instruction(program, &mut ctx, instr, &mut input, &mut output);
            }
        }
    }
}


#[derive(Debug, Copy, Clone)]
enum PanelColor { Black, White }

#[derive(Debug, Copy, Clone)]
struct Panel {
    color: PanelColor,
    touched: bool,
}

type Field = [[Panel; 100]; 100];

#[derive(Debug)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Debug)]
enum Orientation { NORTH, EAST, SOUTH, WEST }

#[derive(Debug)]
struct Turtle {
    position: Position,
    orientation: Orientation,
}

fn get_panel_color(field: &Field, position: &Position) -> PanelColor {
    field[position.y][position.x].color
}

fn set_panel_color(field: &mut Field, position: &Position, color: PanelColor) {
    field[position.y][position.x] = Panel { color, touched: true };
}

fn rotate_cw(orientation: Orientation) -> Orientation {
    match orientation {
        Orientation::NORTH => Orientation::EAST,
        Orientation::EAST => Orientation::SOUTH,
        Orientation::SOUTH => Orientation::WEST,
        Orientation::WEST => Orientation::NORTH,
    }
}

fn rotate_ccw(orientation: Orientation) -> Orientation {
    match orientation {
        Orientation::NORTH => Orientation::WEST,
        Orientation::WEST => Orientation::SOUTH,
        Orientation::SOUTH => Orientation::EAST,
        Orientation::EAST => Orientation::NORTH,
    }
}

fn parse_panel_color(num: i64) -> PanelColor {
    match num {
        0 => PanelColor::Black,
        1 => PanelColor::White,
        _ => panic!()
    }
}

fn dump_panel_color(color: PanelColor) -> i64 {
    match color {
        PanelColor::Black => 0,
        PanelColor::White => 1,
    }
}

fn run_turtle(receiver: Receiver<i64>, sender: Sender<i64>) -> Field {
    let mut field: Field = [[Panel { color: PanelColor::Black, touched: false }; 100]; 100];
    let mut turtle = Turtle {
        position: Position { x: 50, y: 50 },
        orientation: Orientation::NORTH,
    };

    set_panel_color(&mut field, &turtle.position, PanelColor::White);

    loop {
        match sender.send(dump_panel_color(get_panel_color(&field, &turtle.position))) {
            Ok(_) => (),
            Err(_) => return field,
        }

        let paint_color: i64 = receiver.recv().unwrap();
        let move_dir: i64 = receiver.recv().unwrap();

        set_panel_color(&mut field, &turtle.position, parse_panel_color(paint_color));

        if move_dir == 0 {
            turtle.orientation = rotate_ccw(turtle.orientation);
        } else {
            turtle.orientation = rotate_cw(turtle.orientation);
        }

        match turtle.orientation {
            Orientation::NORTH => { turtle.position.y -= 1; },
            Orientation::EAST => { turtle.position.x += 1; },
            Orientation::SOUTH => { turtle.position.y += 1; },
            Orientation::WEST => { turtle.position.x -= 1; },
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buffer = String::new();
    let mut file = File::open("input.txt")?;

    file.read_to_string(&mut buffer)?;

    let mut program = buffer.trim().split(",").map(|s| s.parse()).collect::<Result<Vec<_>, _>>()?;
    program.resize(4096, 0);

    let (toTurtleSender, toTurtleReceiver) = channel();
    let (fromTurtleSender, fromTurtleReceiver) = channel();

    let turtleThread = thread::spawn(move|| {
        let field = run_turtle(toTurtleReceiver, fromTurtleSender);

        println!("{}", field.iter().map(|&row| row.iter().filter(|&panel| panel.touched).count()).sum::<usize>());

        for row in field.iter() {
            for panel in row.iter() {
                match panel.color {
                    PanelColor::White => { print!("X"); },
                    PanelColor::Black => { print!(" "); },
                }
            }
            println!("");
        }
    });
    let computerThread = thread::spawn(move|| {
        simulate(&mut program, || fromTurtleReceiver.recv().unwrap(), |x| toTurtleSender.send(x).unwrap());
    });

    turtleThread.join();
    computerThread.join();

    Ok(())
}
