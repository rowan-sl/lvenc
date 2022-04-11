mod u8_util;
pub use u8_util::*;
use image::Rgb;
use bitvec::prelude::*;


/// returns if the difference between two `Rgb` pixels is 'small' and not all non-zero
pub fn pix_small_difference(px1: Rgb<u8>, px2: Rgb<u8>) -> bool {
    // 1 + 2 + 4 + 8 = 15
    const ACCEPTABLE: i16 = 15; // 4 bit + one for signedness
    px1.0[0].abs_dist(px2.0[0]) <= ACCEPTABLE
    && px1.0[1].abs_dist(px2.0[1]) <= ACCEPTABLE
    && px1.0[2].abs_dist(px2.0[2]) <= ACCEPTABLE
    && px1 != px2
}

#[test]
fn test_pix_small_diff() {
    let px1 = Rgb([10, 21, 100]);
    let px2 = Rgb([25, 20, 100]);
    assert!(pix_small_difference(px1, px2));
    let px1 = Rgb([10, 21, 255]);
    let px2 = Rgb([10, 21, 255]);
    assert!(!pix_small_difference(px1, px2));
    let px1 = Rgb([10, 21, 116]);
    let px2 = Rgb([25, 20, 100]);
    assert!(!pix_small_difference(px1, px2));
}

/// get the bit at the provided index
///
/// bit 0 is least significant, bit 7 is most significant
pub const fn bit(byte: u8, bit: u8) -> bool {
    debug_assert!(bit <= 7);
    byte >> bit & 1 == 1
}

#[test]
fn test_get_bit() {
    let byte = 1u8;
    let bit_val = bit(byte, 0);
    assert_eq!(true, bit_val);
}

/// sets the bit at the provided index
///
/// bit 0 is least significant, bit 7 is most significant
pub fn set_bit(byte: &mut u8, bit_id: u8, bit_v: bool) {
    debug_assert!(bit_id <= 7);
    // dbg!(&byte);
    *byte &= (1 >> bit_id) ^ u8::MAX;
    // dbg!(&byte);
    if bit_v {
        *byte |= 1 << bit_id;
    }
    // dbg!(&byte);
}

#[test]
fn test_set_bit() {
    let mut byte = 1u8;
    set_bit(&mut byte, 0, false);
    assert_eq!(0, byte);
}

pub fn collect_byte(iter: &mut bitvec::boxed::IntoIter<u8, Lsb0>) -> Option<u8> {
    if iter.len() < 8 {
        return None;
    }
    let mut bits = vec![];
    for _ in 0..8 {
        bits.push(iter.next().unwrap());
    }
    let mut res = 0u8;
    for (i, b) in bits.into_iter().enumerate() {
        set_bit(&mut res, i as u8, b);
    }
    Some(res)
}

#[test]
fn test_collect_byte() {
    let mut vec = BitVec::<u8, Lsb0>::new();
    vec.extend_from_raw_slice(&[249u8]);
    vec.extend_from_raw_slice(&[126u8]);
    dbg!(&vec);
    let mut iter = vec.into_iter();
    assert_eq!(Some(249), collect_byte(&mut iter));
    assert_eq!(Some(126), collect_byte(&mut iter));
}

pub fn collect_i4(iter: &mut bitvec::boxed::IntoIter<u8, Lsb0>) -> Option<i8> {
    if iter.len() < 5 {
        return None;
    }
    let mut bits = vec![];
    let is_neg = iter.next().unwrap();
    for _ in 0..4 {
        bits.push(iter.next().unwrap());
    }
    let mut res = 0u8;
    for (i, b) in bits.into_iter().enumerate() {
        set_bit(&mut res, i as u8, b);
    }
    let mut signed_res = i8::try_from(/* TODO please god no please god why */if is_neg {16 - res} else {res}).unwrap();
    if is_neg {
        signed_res = -signed_res;
    }
    Some(signed_res)
}

pub fn serialize_i4(target: &mut BitVec<u8, Lsb0>, x: i8) {
    target.push(x.is_negative());
    for i in 0u8..4u8 {
        target.push(bit(x as u8, i));
    }
}

#[test]
fn test_i4_encoding() {
    let mut fail = false;
    for i in -15i8..=15i8 {
        println!("testing {i}");
        let mut bv = BitVec::<u8, Lsb0>::new();
        serialize_i4(&mut bv, i);
        // assert_eq!(Some(i), collect_i4(&mut bv.into_iter()));
        let collected = collect_i4(&mut bv.into_iter()).unwrap();
        if i != collected {
            fail = true;
            println!("Fail: {} != {}", i, collected);
        }
    }
    if fail {
        panic!("Failed");
    }
}