mod imageiter;
mod utils;

extern crate anyhow;
extern crate array2d;
extern crate bitvec;
extern crate image;
extern crate opencv;
extern crate thiserror;

use anyhow::Result;
use array2d::Array2D;
use bitvec::prelude::*;
use cv::prelude::*;
use cv::videoio::VideoCapture;
use image::{Rgb, RgbImage};
use opencv as cv;
use std::io::Write;
use std::path::PathBuf;
use std::time::Instant;
use std::fs::OpenOptions;//, hint::unreachable_unchecked};
use utils::mat_to_image;

trait U8Diff {
    fn diff(&self, other: u8) -> i16;
}

impl U8Diff for u8 {
    fn diff(&self, other: u8) -> i16 {
        (*self as i16) - (other as i16)
    }
}

/// returns if the difference between two `Rgb` pixels is 'small' and not all non-zero
pub fn pix_small_difference(px1: Rgb<u8>, px2: Rgb<u8>) -> bool {
    const ACCEPTABLE: i16 = 15; // 4 bit + one for signedness
    (px1.0[0].diff(px2.0[0]).abs() <= ACCEPTABLE
        && px1.0[1].diff(px2.0[1]).abs() <= ACCEPTABLE
        && px1.0[2].diff(px2.0[2]).abs() <= ACCEPTABLE)
        && (px1.0[0] != 0 || px1.0[0] != 0 || px1.0[0] != 0)
}

/// get the bit at the provided index
///
/// bit 0 is least significant, bit 7 is most significant
pub const fn bit(byte: u8, bit: u8) -> bool {
    debug_assert!(bit <= 7);
    byte >> bit & 1 == 1
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

fn collect_byte(iter: &mut bitvec::boxed::IntoIter<u8, Lsb0>) -> Option<u8> {
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

fn collect_i4(iter: &mut bitvec::boxed::IntoIter<u8, Lsb0>) -> Option<i8> {
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
    let mut signed_res = i8::try_from(res).unwrap();
    if is_neg {
        signed_res = -signed_res;
    }
    Some(signed_res)
}

fn serialize_i4(target: &mut BitVec<u8, Lsb0>, x: i8) {
    target.push(x.is_negative());
    for i in 0u8..4u8 {
        target.push(bit(x as u8, i));
    }
}


//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa
#[derive(Debug, Clone, PartialEq, Eq)]
enum IntermediatePixelNonPointerOrSameAsLast {
    Pixel([u8; 3]),
    /// even though it is a i8, treat this as a i4. for qualification, use the [`pix_small_difference`] function
    SmallChange {
        change: [i8; 3],
        new: [u8; 3],
    },
}

impl IntermediatePixelNonPointerOrSameAsLast {
    pub fn into_inner(self) -> [u8; 3] {
        match self {
            Self::Pixel(x) => x,
            Self::SmallChange { new, .. } => new,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IntermediatePixelNonPointer {
    /// same as last may not point to a pointer, as that would be b a d, nor can it point to another SameAsLast,
    /// but it kinda can since the SameAsLast is processed before the check happens
    SameAsLast(IntermediatePixelNonPointerOrSameAsLast),
    Other(IntermediatePixelNonPointerOrSameAsLast),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IntermediatePixel {
    Pointer {
        /// 1 is up, 0 is left
        direction: bool,
        /// a pointer can however point to a SameAsLast
        target: IntermediatePixelNonPointer,
    },
    Other(IntermediatePixelNonPointer),
}

/// 1 is up, 0 is left
#[derive(Debug, Clone, PartialEq, Eq)]
enum Pointer {
    Repeat(u8, bool),
    Series(Vec<bool>), // len MUST NOT exceed 255
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SingleRepType {
    SameAsLast,
    Pixel([u8; 3]),
    SmallChange([i8; 3]),
    /// 1 is up, 0 is left
    Pointer(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RepType {
    SameAsLast(u8),
    Pixels(Vec<[u8; 3]>), // len MUST NOT exceed 255
    SmallChange(Vec<[i8; 3]>),
    Pointer(Pointer),
}

impl RepType {
    pub fn serialize(self) -> BitVec<u8, Lsb0> {
        let mut res = BitVec::new();

        match self {
            Self::SameAsLast(num) => {
                // res_type
                res.push(false);
                res.push(false);
                // num of pixels that are the same as last
                res.extend_from_raw_slice(&[num]);
            }
            Self::Pixels(pixels) => {
                // res_type
                res.push(true);
                res.push(false);
                // number of pixels
                res.extend_from_raw_slice(&[u8::try_from(pixels.len()).expect("pixels len must be less than 255")]);
                // pixels
                res.extend_from_raw_slice(&pixels.into_iter().flatten().collect::<Vec<u8>>());
            }
            Self::SmallChange(pixels) => {
                // res_type
                res.push(false);
                res.push(true);
                // number of pixels
                res.extend_from_raw_slice(&[u8::try_from(pixels.len()).expect("pixels len must be less than 255")]);
                // pixels
                pixels.into_iter().flatten().for_each(|d_rgb| {
                    serialize_i4(&mut res, d_rgb);
                });
            }
            Self::Pointer(ptr) => {
                // res_type
                res.push(true);
                res.push(true);
                // type
                match ptr {
                    Pointer::Repeat(num, direction) => {
                        //type
                        res.push(false);
                        //num
                        res.extend_from_raw_slice(&[num]);
                        //direction
                        res.push(direction);
                    }
                    Pointer::Series(ptrs) => {
                        //type
                        res.push(true);
                        //num
                        res.extend_from_raw_slice(
                            &[u8::try_from(ptrs.len())
                                .expect("Pointer vec len must be less than 255")],
                        );
                        //poiners
                        res.extend(ptrs);
                    }
                }
            }
        }
        res
    }
}

///& old encoding
/// is rep
///     n -> is ptr
///         n -> encoded rgb [u8; 3]
///         y -> left or right
///             l -> 0
///             r -> 1
///     y -> max num reps?
///         n -> num reps (u8), item (see first branch)
///         y -> item (see first branch)
///
///& new encoding:
/// reps here are specified before a series of pixels,
/// and the number of folowing pixels are always specified (or max)
/// so it might say `the next 143 pixels are all repetitions of this one pixel` and then specify the pixel,
/// or it could say `these next 120 pixels are all the same as in the last image` and then specify nothing.
/// weather it is a rep or not is not specified, as the first data will be the repetition type and after the num of pixels specified, the type data will occur again
///
/// TODO add test cases for weather different things increase encoding size
/// TODO add fourth case to repr_type, as there is one more option available
///
/// ! note that for pointers, they are stored as a bool, with 1 being up and 0 being left
///
/// rep_type(u2):
///     same_as_last(num: u8,) /* same pixel as last image, if there was no last image, assume that it was entierly made of black pixels
///                                this is generic over every single other pixel type except iself, so when encoding it should be treated as so,
///                                added in as part of the last step, and compared to the previous image right before the last step. */
///     pixels(num: u8,) /* folowed by `num` pixels ([u8; 3]) */
///     small_change(num: u8,) /* folowed by `num` small scale pixel changes ([i4; 3] vs [u8; 3] or 15 bits vs 24 bits).
///                                 if this is applicable can be determined using the `pix_small_difference` function
///                                 i4 is represented as (sign + first 4 bits of i8 as u8*/
///     pointer(type: u1): /* pointer to another pixel, type determines the folowing variant */
///         repeat(num: u8, direction: u1,) /* `num` repetitions of pointer `direction` */
///         series(num: u8,) /* folowed by [u1; `num`] pointers */
///
/// Encoding steps:
///
/// start out with a RgbImage, and convert to a 2DArray of `IntermediatePixelNonPointerOrSameAsLast`
/// then, conver to `IntermediatePixelNonPointer`, and finally conver to `IntermediatePixel`
///
/// at this step, flatten the array into a vector, and convert the `IntermediatePixel`s into a vector of `RepType`
/// then, use RepType::serialize() to convert to bytes
///
#[allow(unused)]
const SPEC: () = ();

fn serialize_stage_0(
    last_img: &RgbImage,
    img: &RgbImage,
) -> Array2D<Option<IntermediatePixelNonPointerOrSameAsLast>> {
    let width = img.width();
    let height = img.height();

    let mut intermediate_arr: Array2D<Option<IntermediatePixelNonPointerOrSameAsLast>> =
        Array2D::filled_with(None, height as usize, width as usize);

    for (x, y, pix) in img.enumerate_pixels() {
        let last_pix = *last_img.get_pixel(x, y);

        intermediate_arr
            .set(
                y as usize,
                x as usize,
                Some({
                    if pix_small_difference(last_pix, *pix) {
                        // println!("{:?} {:?}", last_pix, pix);
                        IntermediatePixelNonPointerOrSameAsLast::SmallChange {
                            change: [
                                i8::try_from(last_pix.0[0] as i16 - pix.0[0] as i16).unwrap(),
                                i8::try_from(last_pix.0[1] as i16 - pix.0[1] as i16).unwrap(),
                                i8::try_from(last_pix.0[2] as i16 - pix.0[2] as i16).unwrap(),
                            ],
                            new: pix.0,
                        }
                    } else {
                        IntermediatePixelNonPointerOrSameAsLast::Pixel([
                            pix.0[0], pix.0[1], pix.0[2],
                        ])
                    }
                }),
            )
            .unwrap();
    }

    intermediate_arr
}

fn serialize_stage_1(
    last: Option<Array2D<Option<IntermediatePixelNonPointerOrSameAsLast>>>,
    current: Array2D<Option<IntermediatePixelNonPointerOrSameAsLast>>,
) -> Array2D<Option<IntermediatePixelNonPointer>> {
    let mut intermediate_arr: Array2D<Option<IntermediatePixelNonPointer>> =
        Array2D::filled_with(None, current.num_rows(), current.num_columns());

    for col in 0..current.num_columns() {
        for row in 0..current.num_rows() {
            let at_current = current.get(row, col).unwrap().to_owned().unwrap();
            if let Some(ref last_arr) = last {
                let at_last = last_arr.get(row, col).unwrap().to_owned().unwrap();
                if at_last == at_current {
                    intermediate_arr
                        .set(
                            row,
                            col,
                            Some(IntermediatePixelNonPointer::SameAsLast(at_last)),
                        )
                        .unwrap();
                    continue;
                }
            }
            intermediate_arr
                .set(
                    row,
                    col,
                    Some(IntermediatePixelNonPointer::Other(at_current)),
                )
                .unwrap();
        }
    }

    intermediate_arr
}

fn serialize_stage_2(
    current: Array2D<Option<IntermediatePixelNonPointer>>,
) -> Array2D<Option<IntermediatePixel>> {
    let mut intermediate_arr: Array2D<Option<IntermediatePixel>> =
        Array2D::filled_with(None, current.num_rows(), current.num_columns());

    for (y, rows) in current.rows_iter().enumerate() {
        'pxl_loop: for (x, pixel_wapper) in rows.enumerate() {
            let pixel = pixel_wapper.to_owned().unwrap();

            for (o_x, o_y, i) in [(x.saturating_sub(1), y, 0), (x, y.saturating_sub(1), 1)] {
                if o_x == x && o_y == y {
                    continue;
                }

                let at_other = current.get(o_y, o_x).unwrap().to_owned().unwrap();

                let inner_at_other = match at_other.clone() {
                    IntermediatePixelNonPointer::SameAsLast(x) => x,
                    IntermediatePixelNonPointer::Other(x) => x,
                };

                let inner_at_this = match pixel.clone() {
                    IntermediatePixelNonPointer::SameAsLast(x) => x,
                    IntermediatePixelNonPointer::Other(x) => x,
                };

                if inner_at_other.into_inner() == inner_at_this.into_inner() {
                    intermediate_arr
                        .set(
                            y,
                            x,
                            Some(IntermediatePixel::Pointer {
                                direction: i == 1,
                                target: at_other,
                            }),
                        )
                        .unwrap();
                    continue 'pxl_loop;
                }
            }

            intermediate_arr
                .set(y, x, Some(IntermediatePixel::Other(pixel)))
                .unwrap();
        }
    }

    intermediate_arr
}

fn display_stage_2(stage_2: &Array2D<Option<IntermediatePixel>>) -> RgbImage {
    RgbImage::from_fn(
        stage_2.num_columns() as u32,
        stage_2.num_rows() as u32,
        |col, row| match stage_2
            .get(row as usize, col as usize)
            .unwrap()
            .to_owned()
            .unwrap()
        {
            IntermediatePixel::Pointer { direction, .. } => {
                if direction {
                    Rgb([255, 0, 0])
                } else {
                    Rgb([0, 0, 255])
                }
            }
            IntermediatePixel::Other(other) => match other {
                IntermediatePixelNonPointer::SameAsLast(..) => Rgb([0, 255, 0]),
                IntermediatePixelNonPointer::Other(pixel) => Rgb(pixel.into_inner()),
            },
        },
    )
}

fn serialize_stage_3(
    stage_2: Array2D<Option<IntermediatePixel>>
) -> Vec<IntermediatePixel> {
    stage_2.elements_row_major_iter()
        .map(|i| {
            i.to_owned().unwrap()
        })
        .collect()
}

fn serialize_stage_4(
    stage_3: Vec<IntermediatePixel>
) -> Vec<RepType> {
    let mut iterator = stage_3.into_iter();
    let mut current: RepType = match iterator.next().unwrap() {
        IntermediatePixel::Other(other) => match other {
            IntermediatePixelNonPointer::SameAsLast(..) => {
                RepType::SameAsLast(1)
            }
            IntermediatePixelNonPointer::Other(pixel) => {
                match pixel {
                    IntermediatePixelNonPointerOrSameAsLast::Pixel(px) => {
                        RepType::Pixels(vec![px])
                    }
                    IntermediatePixelNonPointerOrSameAsLast::SmallChange { change, .. } => {
                        RepType::SmallChange(vec![change])
                    }
                }
            }
        }
        IntermediatePixel::Pointer{..} => unreachable!()
    };
    let mut res = vec![];

    for i in iterator {
        match i {
            IntermediatePixel::Pointer { direction, .. } => {
                if let RepType::Pointer(ref mut ptr) = current {
                    match ptr {
                        Pointer::Repeat(ref mut count, ref c_dir) => {
                            if *c_dir == direction {
                                if *count == u8::MAX {
                                    res.push(current);
                                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                                } else {
                                    *count += 1;
                                }
                            } else {
                                if *count < 10 {
                                    // less than some ammount of repetitions, so it is better just to switch over to a series variant instead
                                    let mut ptrs = vec![];
                                    for _  in 0..*count {
                                        ptrs.push(*c_dir);
                                    }
                                    ptrs.push(direction);
                                    current = RepType::Pointer(Pointer::Series(ptrs));
                                } else {
                                    // more than some ammount of reps, so it is better to make a new set of reps to go on with
                                    res.push(current);
                                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                                }
                            }
                        }
                        Pointer::Series(ref mut ptrs) => {
                            if ptrs.len() == 255 {
                                res.push(current);
                                current = RepType::Pointer(Pointer::Repeat(1, direction));
                            } else {
                                ptrs.push(direction);
                            }
                        }
                    }
                } else {
                    res.push(current);
                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                }
            }
            IntermediatePixel::Other(other) => match other {
                IntermediatePixelNonPointer::SameAsLast(..) => {
                    if let RepType::SameAsLast(ref mut count) = current {
                        if *count == 255 {
                            res.push(current);
                            current = RepType::SameAsLast(1);
                        } else {
                            *count += 1;
                        }
                    } else {
                        res.push(current);
                        current = RepType::SameAsLast(1);
                    }
                }
                IntermediatePixelNonPointer::Other(pixel_kind) => match pixel_kind {
                    IntermediatePixelNonPointerOrSameAsLast::Pixel(pixel) => {
                        if let RepType::Pixels(ref mut pixels) = current {
                            if pixels.len() == 255 {
                                res.push(current);
                                current = RepType::Pixels(vec![pixel]);
                            } else {
                                pixels.push(pixel);
                            }
                        } else {
                            res.push(current);
                            current = RepType::Pixels(vec![pixel]);
                        }
                    }
                    IntermediatePixelNonPointerOrSameAsLast::SmallChange { change, .. } => {
                        if let RepType::SmallChange(ref mut changes) = current {
                            if changes.len() == 255 {
                                res.push(current);
                                current = RepType::SmallChange(vec![change]);
                            } else {
                                changes.push(change);
                            }
                        } else {
                            res.push(current);
                            current = RepType::SmallChange(vec![change]);
                        }
                    }
                }
            }
        }
    }

    res.push(current);
    res
}

fn serialize_stage_5(
    stage_4: Vec<RepType>
) -> Vec<u8> {
    let mut res = BitVec::<u8, Lsb0>::new();
    for i in stage_4 {
        res.append(&mut i.serialize());
    }
    res.into_vec()
}


#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PacketMetadata {
    /// data vector can be ignored, should be empty
    Init {
        /// width, height
        frame_size: (u32, u32),
    },
    /// data vector includes some data, for the next frame.
    NewFrame(usize/* amnt of bytes for the next frame */),
    /// data vector includes some data
    FrameData,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Packet {
    metadata: PacketMetadata,
    data: Vec<u8>,
}

#[derive(Debug)]
pub struct Encoder {
    /// number of bytes to send in a packet. if zero, a entire image of bytes will be sent in one go
    bytes_per_packet: usize,
    last_frame: RgbImage,
    last_stage_0: Option<Array2D<Option<IntermediatePixelNonPointerOrSameAsLast>>>,
    pending_packets: Vec<Packet>,
}

impl Encoder {
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            bytes_per_packet: 0,
            last_frame: RgbImage::from_pixel(width, height, Rgb([0; 3])),
            last_stage_0: None,
            pending_packets: vec![Packet {metadata: PacketMetadata::Init { frame_size: (width, height) }, data: vec![]}]
        }
    }

    pub fn packets(&mut self) -> impl Iterator<Item = Packet> + '_ {
        self.pending_packets.drain(..)
    }

    pub fn encode_frame(&mut self, frame: RgbImage) {
        let pt0 = serialize_stage_0(
            &self.last_frame,
            &frame,
        );
        self.last_frame = frame;
        let pt1 = serialize_stage_1(self.last_stage_0.take(), pt0.clone());
        self.last_stage_0 = Some(pt0);
        let pt2 = serialize_stage_2(pt1);

        let pt3 = serialize_stage_3(pt2.clone());
        let pt4 = serialize_stage_4(pt3);
        let serialized = serialize_stage_5(pt4);

        if self.bytes_per_packet == 0 {
            self.pending_packets.push(Packet { metadata: PacketMetadata::NewFrame(serialized.len()), data: serialized });
        } else {
            let mut iter = serialized.chunks(self.bytes_per_packet);
            self.pending_packets.push(Packet { metadata: PacketMetadata::NewFrame(serialized.len()), data: iter.next().unwrap().to_vec() });
            for packet_data in iter {
                self.pending_packets.push(Packet { metadata: PacketMetadata::FrameData, data: packet_data.to_vec() });
            }
        }
    }
}

#[derive(Debug)]
pub struct Decoder {
    last_frame: Option<RgbImage>,
    frame_size: Option<(u32, u32)>,
    bytes_for_next_frame: Option<usize>,
    pending_frame_data: Vec<u8>,
    pending_frames: Vec<RgbImage>,
}

impl Decoder {
    pub fn new() -> Self {
        Self {
            last_frame: None,
            frame_size: None,
            bytes_for_next_frame: None,
            pending_frame_data: vec![],
            pending_frames: vec![],
        }
    }

    fn attempt_decode(&mut self) {
        let bytes_for_next_frame = self.bytes_for_next_frame.unwrap();
        if self.pending_frame_data.len() >= bytes_for_next_frame {
            let mut frame_data = self.pending_frame_data.drain(0..bytes_for_next_frame).collect::<BitVec<u8, Lsb0>>().into_iter();
            let mut decoded_reptypes: Vec<RepType> = vec![];

            while frame_data.len() != 0 {
                match (frame_data.next().unwrap(), frame_data.next().unwrap()) {
                    (false, false) => {
                        let num_of_same = collect_byte(&mut frame_data).unwrap();
                        decoded_reptypes.push(RepType::SameAsLast(num_of_same));
                    }
                    (true, false) => {
                        let mut pixels = vec![];
                        for _ in 0..collect_byte(&mut frame_data).unwrap() {
                            pixels.push([collect_byte(&mut frame_data).unwrap(), collect_byte(&mut frame_data).unwrap(), collect_byte(&mut frame_data).unwrap()]);
                        }
                        decoded_reptypes.push(RepType::Pixels(pixels));
                    }
                    (false, true) => {
                        let mut changes = vec![];
                        for _ in 0..collect_byte(&mut frame_data).unwrap() {
                            changes.push([collect_i4(&mut frame_data).unwrap(), collect_i4(&mut frame_data).unwrap(), collect_i4(&mut frame_data).unwrap()]);
                        }
                        decoded_reptypes.push(RepType::SmallChange(changes));
                    }
                    (true, true) => {
                        if frame_data.next().unwrap() {
                            // series of different ptrs
                            let mut ptrs = vec![];
                            for _ in 0..collect_byte(&mut frame_data).unwrap() {
                                ptrs.push(frame_data.next().unwrap());
                            }
                            decoded_reptypes.push(RepType::Pointer(Pointer::Series(ptrs)));
                        } else {
                            // repetition of one
                            let count = collect_byte(&mut frame_data).unwrap();
                            let direction = frame_data.next().unwrap();
                            decoded_reptypes.push(RepType::Pointer(Pointer::Repeat(count, direction)));
                        }
                    }
                }
            }

            let single_reptypes = Array2D::<SingleRepType>::from_iter_row_major(decoded_reptypes
                .into_iter()
                .map(|i| {
                    match i {
                        RepType::Pixels(pixels) => {
                            pixels.into_iter().map(|i| {
                                SingleRepType::Pixel(i)
                            }).collect::<Vec<_>>()
                        }
                        RepType::SmallChange(changes) => {
                            changes.into_iter().map(|i| {
                                SingleRepType::SmallChange(i)
                            }).collect::<Vec<_>>()
                        }
                        RepType::SameAsLast(num) => {
                            std::iter::repeat(SingleRepType::SameAsLast).take(num as usize).collect::<Vec<_>>()
                        }
                        RepType::Pointer(ptr) => {
                            match ptr {
                                Pointer::Repeat(num, direction) => {
                                    std::iter::repeat(SingleRepType::Pointer(direction)).take(num as usize).collect::<Vec<_>>()
                                }
                                Pointer::Series(ptrs) => {
                                    ptrs.into_iter().map(|i| {
                                        SingleRepType::Pointer(i)
                                    }).collect::<Vec<_>>()
                                }
                            }
                        }
                    }
                })
                .flatten(),
                self.frame_size.unwrap().1 as usize,
                self.frame_size.unwrap().0 as usize,
            );

            let mut frame = self.last_frame.take().unwrap();

            for (row, row_data) in single_reptypes.rows_iter().enumerate() {
                for (col, reptype) in row_data.enumerate() {
                    match *reptype {
                        SingleRepType::Pixel(pixel) => {
                            frame.get_pixel_mut(col as u32, row as u32).0 = pixel;
                        }
                        SingleRepType::Pointer(direction) => {
                            *frame.get_pixel_mut(col as u32, row as u32) = if direction {
                                *frame.get_pixel_mut(col as u32, row as u32 - 1)
                            } else {
                                *frame.get_pixel_mut(col as u32 - 1, row as u32)
                            };
                        }
                        SingleRepType::SmallChange(change) => {
                            let at = frame.get_pixel_mut(col as u32, row as u32);
                            at.0[0] = (at.0[0] as i8 + change[0]) as u8;
                            at.0[1] = (at.0[1] as i8 + change[1]) as u8;
                            at.0[2] = (at.0[2] as i8 + change[2]) as u8;
                        }
                        SingleRepType::SameAsLast => {/* hehe */}
                    }
                }
            }

            self.last_frame = Some(frame.clone());
            self.pending_frames.push(frame);
        }
    }

    pub fn feed_packet(&mut self, packet: Packet) {
        // TODO remove panics
        match packet.metadata {
            PacketMetadata::Init { frame_size } => {
                self.frame_size = Some(frame_size);
                self.last_frame = Some(RgbImage::from_pixel(frame_size.0, frame_size.1, Rgb([0; 3])));
            }
            PacketMetadata::NewFrame(num_bytes) => {
                if let None = self.bytes_for_next_frame {
                    self.bytes_for_next_frame = Some(num_bytes);
                    self.pending_frame_data.clear();
                } else {
                    panic!("new frame header sent, but the last frame has not been completed");
                }
                self.pending_frame_data.extend_from_slice(&packet.data);
                self.attempt_decode();
            }
            PacketMetadata::FrameData => {
                self.pending_frame_data.extend_from_slice(&packet.data);
                self.attempt_decode();
            }
        }
    }

    pub fn frames(&mut self) -> impl Iterator<Item = RgbImage> + '_ {
        self.pending_frames.drain(..)
    }
}

pub fn test_for(vid: PathBuf, out_dir: &str) -> Result<()> {
    let mut cap = VideoCapture::from_file(vid.to_str().unwrap(), cv::videoio::CAP_ANY)?;

    let (before_img_path, stage_two_path, out_file) = {
        let mut out_dir = PathBuf::from(out_dir);
        out_dir.push(PathBuf::from(vid).file_name().unwrap());

        if !out_dir.exists() {
            std::fs::create_dir(&out_dir)?;
        }

        let mut before_img_path = out_dir.clone();
        before_img_path.push("before.png");

        let mut stage_two_path = out_dir.clone();
        stage_two_path.push("out.bmp");

        let mut out_file = out_dir.clone();
        out_file.push("out.bin");

        (before_img_path, stage_two_path, out_file)
    };

    let mut frame1 = Mat::default();
    let mut frame2 = Mat::default();

    assert!(cap.read(&mut frame1)?);
    assert!(!frame1.empty());
    assert!(cap.read(&mut frame2)?);
    assert!(!frame2.empty());

    mat_to_image(&frame2)?.save(before_img_path)?;

    let before_pt1 = Instant::now();

    let pt0_old = serialize_stage_0(
        &RgbImage::from_pixel(
            frame1.size().unwrap().width as u32,
            frame1.size().unwrap().height as u32,
            Rgb([0; 3]),
        ),
        &mat_to_image(&frame1).unwrap(),
    );
    let pt0 = serialize_stage_0(
        &mat_to_image(&frame1).unwrap(),
        &mat_to_image(&frame2).unwrap(),
    );
    let pt1 = serialize_stage_1(Some(pt0_old), pt0);
    let pt2 = serialize_stage_2(pt1);

    let after_pt1 = Instant::now();
    let before_pt2 = Instant::now();

    let pt3 = serialize_stage_3(pt2.clone());
    let pt4 = serialize_stage_4(pt3);
    let pt5 = serialize_stage_5(pt4);

    let after_pt2 = Instant::now();

    println!("Serialization pt 1 took {:?}", (after_pt1 - before_pt1) + (after_pt2 - before_pt2));

    display_stage_2(&pt2).save(stage_two_path).unwrap();

    OpenOptions::new().create(true).write(true).open(out_file)?.write_all(&pt5)?;

    println!(
        "minimum time to do one frame at 60fps is {}ms",
        1f64 / 60f64 * 1000f64
    );

    Ok(())
}
