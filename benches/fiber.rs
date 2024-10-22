use corosensei::stack::DefaultStack;
use corosensei::Fiber;
use criterion::measurement::Measurement;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fiber_switch<M: Measurement + 'static>(name: &str, c: &mut Criterion<M>) {
    struct IdFiber(Fiber<(IdFiber, usize)>);

    let mut identity = IdFiber(Fiber::new(
        |(mut yielder, mut input): (IdFiber, usize)| -> (Fiber<()>, fn(DefaultStack)) {
            loop {
                (yielder, input) = yielder.0.switch(move |fib| (IdFiber(fib), input))
            }
        },
    ));

    c.bench_function(name, |b| {
        b.iter(|| {
            identity.0.switch_in_place(
                |yielder| (IdFiber(yielder), black_box(0usize)),
                |(f, _)| (f.0, ()),
            );
        })
    });
}

fn fiber_call<M: Measurement + 'static>(name: &str, c: &mut Criterion<M>) {
    // Don't count time spent allocating a stack.
    let mut stack = Some(DefaultStack::default());

    c.bench_function(name, move |b| {
        b.iter(|| {
            let identity = Fiber::<(Fiber<(DefaultStack, usize)>, usize)>::with_stack(
                |(fiber, input)| (fiber, move |stack| (stack, input)),
                stack.take().unwrap(),
            );
            stack = Some(identity.switch(|fib| (fib, black_box(0usize))).0)
        })
    });
}

fn fiber_switch_time(c: &mut Criterion) {
    fiber_switch("fiber_switch_time", c);
}
fn fiber_call_time(c: &mut Criterion) {
    fiber_call("fiber_call_time", c);
}

criterion_group!(
    name = time;
    config = Criterion::default();
    targets = fiber_switch_time, fiber_call_time
);

cfg_if::cfg_if! {
    if #[cfg(any(target_arch = "x86", target_arch = "x86_64"))] {
        use criterion_cycles_per_byte::CyclesPerByte;

        fn fiber_switch_cycles(c: &mut Criterion<CyclesPerByte>) {
            fiber_switch("fiber_switch_cycles", c);
        }
        fn fiber_call_cycles(c: &mut Criterion<CyclesPerByte>) {
            fiber_call("fiber_call_cycles", c);
        }

        criterion_group!(
            name = cycles;
            config = Criterion::default().with_measurement(CyclesPerByte);
            targets = fiber_switch_cycles, fiber_call_cycles
        );

        criterion_main!(cycles, time);
    } else {
        criterion_main!(time);
    }
}
