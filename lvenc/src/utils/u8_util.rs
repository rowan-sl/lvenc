use std::cmp::{max, min};

pub trait U8AbsDist {
    fn abs_dist(&self, other: u8) -> i16;
}

impl U8AbsDist for u8 {
    fn abs_dist(&self, other: u8) -> i16 {
        (max(*self, other) as i16 - min(*self, other) as i16).abs()
    }
}

#[test]
fn test_u8_abs_dist() {
    let a = 10u8;
    let b = 15u8;
    assert_eq!(a.abs_dist(b), b.abs_dist(a));
    assert_eq!(5, a.abs_dist(b));
}
