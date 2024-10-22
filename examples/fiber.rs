// This example is basically a reworked `example/basic.rs`

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
    println!("[main] creating fiber");

    let mut exec = Fiber::new(|Arg { mut exec, i }| {
        println!("[fiber] fiber started with input {}", i);
        for mut i in 0..5 {
            println!("[fiber] yielding {}", i);
            Arg { exec, i } = exec.switch(move |exec| Some(Yield { exec, i }));
            println!("[fiber] got {} from parent", i)
        }
        println!("[fiber] exiting fiber");
        (exec, |_| None)
    });

    let mut counter = 100;
    loop {
        println!("[main] resuming fiber with argument {}", counter);
        match exec.switch(move |exec| Arg::new(exec, counter)) {
            Some(Yield { exec: new, i }) => {
                exec = new;
                println!("[main] got {:?} from fiber", i)
            }
            None => break,
        }

        counter += 1;
    }

    println!("[main] exiting");
}

#[test]
fn fiber() {
    main()
}