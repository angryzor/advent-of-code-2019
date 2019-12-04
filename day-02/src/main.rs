use std::fs::File;
use std::io::Read;

fn op<F: FnMut(i32, i32) -> i32>(program: &mut [i32], pc: usize, mut func: F) {
    let input1 = program[pc+1] as usize;
    let input2 = program[pc+2] as usize;
    let output = program[pc+3] as usize;

    program[output] = func(program[input1], program[input2])
}

fn simulate(program: &mut [i32]) {
    let mut pc = 0;

    loop {
        match program[pc] {
            1 => op(program, pc, std::ops::Add::add),
            2 => op(program, pc, std::ops::Mul::mul),
            99 => return,
            _ => panic!()
        }

        pc += 4
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

    println!("Result of part 1: {}", run(&mut program.clone(), 12, 2));

    for noun in 0..=99 {
        for verb in 0..=99 {
            if run(&mut program.clone(), noun, verb) == 19690720 {
                println!("Result of part 2: {}", noun * 100 + verb);
                break;
            }
        }
    }

    Ok(())
}
