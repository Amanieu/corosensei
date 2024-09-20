use core::marker::PhantomData;

pub trait Session {
    type Argument;
    type Successor;

    fn chain<T>(self) -> Chain<T, Self>
    where
        Self: Sized,
    {
        Chain {
            _marker: PhantomData,
            successor: self,
        }
    }
}

pub struct Chain<Arg, Sess> {
    _marker: PhantomData<fn(Arg) -> Arg>,
    successor: Sess,
}

impl<Arg, Sess: Clone> Clone for Chain<Arg, Sess> {
    fn clone(&self) -> Self {
        Chain {
            _marker: PhantomData,
            successor: self.successor.clone(),
        }
    }
}

impl<Arg, Sess: Copy> Copy for Chain<Arg, Sess> {}
