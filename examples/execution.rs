use corosensei::Fiber;

struct Arg {
    exec: Fiber<Option<Yield>>,
    i: i32,
}

struct Yield {
    exec: Fiber<Arg>,
    i: i32,
}

impl Arg {
    fn new(exec: Fiber<Option<Yield>>, input: i32) -> Self {
        Self { exec, i: input }
    }
}

fn main() {
    println!("[main] creating execution");

    let mut exec = Fiber::new(|Arg { mut exec, i }| {
        println!("[execution] execution started with input {}", i);
        for mut i in 0..5 {
            println!("[execution] yielding {}", i);
            Arg { exec, i } = exec.switch(move |exec| Some(Yield { exec, i }));
            println!("[execution] got {} from parent", i)
        }
        println!("[execution] exiting execution");
        (exec, |_| None)
    });

    let mut counter = 100;
    loop {
        println!("[main] resuming execution with argument {}", counter);
        match exec.switch(move |exec| Arg::new(exec, counter)) {
            Some(Yield { exec: new, i }) => {
                exec = new;
                println!("[main] got {:?} from execution", i)
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
