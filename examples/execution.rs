use corosensei::Execution;

struct Arg {
    exec: Execution<Option<Yield>>,
    i: i32,
}

struct Yield {
    exec: Execution<Arg>,
    i: i32,
}

impl Arg {
    fn new(exec: Execution<Option<Yield>>, input: i32) -> Self {
        Self { exec, i: input }
    }
}

fn main() {
    println!("[main] creating coroutine");

    let mut exec = Execution::new(|arg: Arg| {
        let ret = |_| None;
        let Arg { mut exec, i } = arg;
        println!("[coroutine] coroutine started with input {}", i);
        for mut i in 0..5 {
            println!("[coroutine] yielding {}", i);
            Arg { exec, i } = exec.switch(move |exec| Some(Yield { exec, i }));
            println!("[coroutine] got {} from parent", i)
        }
        println!("[coroutine] exiting coroutine");
        (exec, ret)
    });

    let mut counter = 100;
    loop {
        println!("[main] resuming coroutine with argument {}", counter);
        match exec.switch(move |exec| Arg::new(exec, counter)) {
            Some(Yield { exec: new, i }) => {
                exec = new;
                println!("[main] got {:?} from coroutine", i)
            }
            None => break,
        }

        counter += 1;
    }

    println!("[main] exiting");
}

#[test]
fn execution() {
    main()
}
