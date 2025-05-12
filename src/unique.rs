use std::sync::atomic::Ordering;
use std::sync::atomic::AtomicU64;

///for aliasing we need a unique id
#[derive(Debug,Clone,Copy, PartialEq, Eq, Hash,PartialOrd,Ord)]
pub struct Unique(u64);

static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(0xff);


impl Unique {

    pub const FALSE : Unique =Unique(0);
    pub const TRUE : Unique =Unique(1);
    pub const TYPE : Unique =Unique(2);

    pub const NUM : Unique =Unique(4);
    pub const BOOL : Unique =Unique(8);

    // pub const NAT : Unique =Unique(0xf1);
    // pub const INT: Unique =Unique(0xf2);
    // pub const FRAC: Unique =Unique(0xf4);
    // pub const FLOAT: Unique =Unique(0xf5);


    pub fn new() -> Self {
        let id = UNIQUE_COUNTER.fetch_add(1, Ordering::Relaxed);
        //if some idiot runs a loop that makes lots of these multithreaded this would crash them all
        assert!(id < u64::MAX / 2, "Unique ID counter has overflowed...");
        Unique(id)
    }

    #[inline]
    pub fn new_bool(b:bool) -> Self{
        if b {Self::TRUE} else {Self::FALSE}
    }

    #[inline]
    pub fn bool_value(&self) -> Option<bool>{
        match *self{
            Self::FALSE=>Some(false),
            Self::TRUE=>Some(true),
            _=>None
        }
    }

    #[inline]
    pub fn id(&self) -> u64 {
        self.0
    }
    #[inline]
    pub fn get_counter() -> u64{
        UNIQUE_COUNTER.load(Ordering::Relaxed)
    }

    //this specifcly takes u32 to avoid wrap around risks
    //if for some reason u need more than a full u32 something went horibly wrong
    #[allow(clippy::unnecessary_fallible_conversions)]
    pub fn ensure_size(size: u32) {
        let size = size.try_into().unwrap();
        let id = UNIQUE_COUNTER.load(Ordering::Relaxed);
        if id >= size {
            return;
        }

        let id = UNIQUE_COUNTER.fetch_add(size - id, Ordering::Relaxed);
        assert!(id < u64::MAX / 2, "Unique ID counter has wrapped around!");
    }
}

impl Default for Unique {
    fn default() -> Self {
        Self::new()
    }
}
