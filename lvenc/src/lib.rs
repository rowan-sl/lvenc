mod utils;
mod packet;
mod grid;
mod decoder;

use std::mem::MaybeUninit;

use bitvec::prelude::*;
use image::{Rgb, RgbImage};
use utils::*;
use packet::*;
use grid::Grid;
pub use decoder::{Decoder, Ready};


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

//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntermediatePixelNonPointerOrSameAsLast {
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
pub enum IntermediatePixelNonPointer {
    /// same as last may not point to a pointer, as that would be b a d, nor can it point to another SameAsLast,
    /// but it kinda can since the SameAsLast is processed before the check happens
    SameAsLast(IntermediatePixelNonPointerOrSameAsLast),
    Other(IntermediatePixelNonPointerOrSameAsLast),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntermediatePixel {
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
pub enum Pointer {
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
pub enum RepType {
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

                pixels.into_iter().map(|i| { if i == [0; 3] {panic!()} else {i}}).flatten().for_each(|d_rgb| {
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

pub fn serialize_stage_0_new(
    last_img: &RgbImage,
    img: &RgbImage,
) -> Vec<IntermediatePixelNonPointerOrSameAsLast> {
    img.clone().enumerate_pixels().zip(last_img.enumerate_pixels()).map(|((_, _, next), (_, _, last))| {
        if pix_small_difference(*last, *next) {
            IntermediatePixelNonPointerOrSameAsLast::SmallChange {
                change: [
                    i8::try_from(next.0[0] as i16 - last.0[0] as i16).unwrap(),
                    i8::try_from(next.0[1] as i16 - last.0[1] as i16).unwrap(),
                    i8::try_from(next.0[2] as i16 - last.0[2] as i16).unwrap(),
                ],
                new: next.0,
            }
        } else {
            IntermediatePixelNonPointerOrSameAsLast::Pixel([
                next.0[0], next.0[1], next.0[2],
            ])
        }
    }).collect()
}

pub fn serialize_stage_1_new(
    last: Option<Vec<IntermediatePixelNonPointerOrSameAsLast>>,
    next: Vec<IntermediatePixelNonPointerOrSameAsLast>,
) -> Vec<IntermediatePixelNonPointer> {
    if let Some(last) = last {
        last.into_iter().zip(next.into_iter()).map(|(l, n)| {
            if l.clone().into_inner() == n.clone().into_inner() {
                IntermediatePixelNonPointer::SameAsLast(l)
            } else {
                IntermediatePixelNonPointer::Other(n)
            }
        }).collect()
    } else {
        next.into_iter().map(|i| {
            IntermediatePixelNonPointer::Other(i)
        }).collect()
    }
}


pub fn serialize_stage_2_new(
    next: Vec<IntermediatePixelNonPointer>,
    image_size: (usize, usize),
) -> Grid<IntermediatePixel> {
    let mut next_grid = Grid::from_iter(image_size.0, image_size.1, next.into_iter()).unwrap();
    let mut res_grid = Grid::<MaybeUninit<IntermediatePixel>>::filled_with_fn(image_size.0, image_size.1, || {MaybeUninit::uninit()});

    'pxl_loop: for (x, y, pixel) in next_grid.clone().into_row_major_iter() {
        // println!("{} {}", x, y);
        for (o_x, o_y, i) in [(x.saturating_sub(1), y, 0), (x, y.saturating_sub(1), 1)] {
            if o_x == x && o_y == y {
                continue;
            }

            let at_other = next_grid.get(o_x, o_y);

            let inner_at_other = match at_other.clone() {
                IntermediatePixelNonPointer::SameAsLast(x) => x,
                IntermediatePixelNonPointer::Other(x) => x,
            };

            let inner_at_this = match pixel.clone() {
                IntermediatePixelNonPointer::SameAsLast(x) => x,
                IntermediatePixelNonPointer::Other(x) => x,
            };

            if inner_at_other.into_inner() == inner_at_this.into_inner() {
                res_grid
                    .set(
                        x,
                        y,
                        MaybeUninit::new(IntermediatePixel::Pointer {
                            direction: i == 1,
                            target: at_other,
                        }),
                    );
                continue 'pxl_loop;
            }
        }

        res_grid
            .set(x, y, MaybeUninit::new(IntermediatePixel::Other(pixel)));
}

    res_grid.transform_into(|i| { unsafe { i.assume_init() } })
}

pub fn display_stage_2(stage_2: &Grid<IntermediatePixel>) -> RgbImage {
    RgbImage::from_fn(
        stage_2.width() as u32,
        stage_2.height() as u32,
        |col, row| match stage_2
            .get(row as usize, col as usize)
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

pub fn serialize_stage_3_new(
    next: Grid<IntermediatePixel>
) -> Vec<IntermediatePixel> {
    next.into_vec()
}

pub fn serialize_stage_4(
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


pub fn serialize_stage_5(
    stage_4: Vec<RepType>
) -> Vec<u8> {
    let mut res = BitVec::<u8, Lsb0>::new();
    for i in stage_4 {
        res.append(&mut i.serialize());
    }
    res.into_vec()
}



#[derive(Debug)]
pub struct Encoder {
    /// number of bytes to send in a packet. if zero, a entire image of bytes will be sent in one go
    bytes_per_packet: usize,
    last_frame: RgbImage,
    last_stage_0: Option<Vec<IntermediatePixelNonPointerOrSameAsLast>>,
    frame_size: (u32, u32),
    pending_packets: Vec<Packet>,
}

impl Encoder {
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            bytes_per_packet: 0,
            last_frame: RgbImage::from_pixel(width, height, Rgb([0; 3])),
            last_stage_0: None,
            frame_size: (width, height),
            pending_packets: vec![Packet {metadata: PacketMetadata::Init { frame_size: (width, height) }, data: vec![]}]
        }
    }

    pub fn packets(&mut self) -> impl Iterator<Item = Packet> + '_ {
        self.pending_packets.drain(..)
    }

    pub fn encode_frame(&mut self, frame: RgbImage) {
        let pt0 = serialize_stage_0_new(
            &self.last_frame,
            &frame,
        );
        self.last_frame = frame;
        let pt1 = serialize_stage_1_new(self.last_stage_0.take(), pt0.clone());
        self.last_stage_0 = Some(pt0);
        let pt2 = serialize_stage_2_new(pt1, (self.frame_size.0 as usize, self.frame_size.1 as usize));

        let pt3 = serialize_stage_3_new(pt2.clone());
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

