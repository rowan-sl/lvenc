mod decoder;
mod grid;
mod packet;
mod utils;

use std::mem::MaybeUninit;

use bitvec::prelude::*;
pub use decoder::{Decoder, Ready};
use grid::Grid;
use image::{Rgb, RgbImage};
pub use packet::*;
// use utils::*;

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
///! removed
///! TODO add a 4th case or remove one of the first cases, to improve encoding
///! NOTE: this was removed because it was way too slow and not that good?
///!     small_change(num: u8,) /* folowed by `num` small scale pixel changes ([i4; 3] vs [u8; 3] or 15 bits vs 24 bits).
///!                                 if this is applicable can be determined using the `pix_small_difference` function
///!                                 i4 is represented as (sign + first 4 bits of i8 as u8*/
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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntermediatePixel {
    Pointer {
        /// 1 is up, 0 is left
        direction: bool,
        /// a pointer can however point to a SameAsLast
        target: [u8; 3],
    },
    /// same as last may not point to a pointer, as that would be b a d, nor can it point to another SameAsLast,
    /// but it kinda can since the SameAsLast is processed before the check happens
    SameAsLast([u8; 3]),
    Pixel([u8; 3]),
}

impl IntermediatePixel {
    pub fn into_inner(self) -> [u8; 3] {
        match self {
            Self::SameAsLast(x) => x,
            Self::Pixel(x) => x,
            Self::Pointer { target, .. } => target,
        }
    }
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
    /// 1 is up, 0 is left
    Pointer(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RepType {
    SameAsLast(u8),
    Pixels(Vec<[u8; 3]>), // len MUST NOT exceed 255
    Pointer(Pointer),
}

impl RepType {
    pub fn serialize(self, res: &mut BitVec<u8, Lsb0>) {
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
                res.extend_from_raw_slice(&[
                    u8::try_from(pixels.len()).expect("pixels len must be less than 255")
                ]);
                // pixels
                res.extend_from_raw_slice(&pixels.into_iter().flatten().collect::<Vec<u8>>());
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
                        res.extend_from_raw_slice(&[u8::try_from(ptrs.len())
                            .expect("Pointer vec len must be less than 255")]);
                        //poiners
                        res.extend(ptrs);
                    }
                }
            }
        }
    }
}

pub unsafe fn serialize_stage_0(
    last_img: &RgbImage,
    img: &RgbImage,
) -> Grid<IntermediatePixel> {
    let image_size = (img.width(), img.height());
    let mut res = Grid::<MaybeUninit<IntermediatePixel>>::filled_with(image_size.0 as usize, image_size.1 as usize, MaybeUninit::uninit());

    'pxl_loop: for ((x, y, next), (_, _, last)) in img.enumerate_pixels().zip(last_img.enumerate_pixels()) {
        for (o_x, o_y, i) in [(x.saturating_sub(1), y, false), (x, y.saturating_sub(1), true)] {
            if o_x == x && o_y == y {
                continue;
            }

            let at_other = res.get_unchecked(o_x as usize, o_y as usize).assume_init();

            if at_other.into_inner() == next.0 {
                res.set_unchecked(
                    x as usize,
                    y as usize,
                    MaybeUninit::new(IntermediatePixel::Pointer {
                        direction: i,
                        target: at_other.into_inner(),
                    }),
                );
                continue 'pxl_loop;
            }
        }

        res.set_unchecked(x as usize, y as usize, MaybeUninit::new(
            if *last == *next {
                IntermediatePixel::SameAsLast(last.0)
            } else {
                IntermediatePixel::Pixel([next.0[0], next.0[1], next.0[2]])
            }
        ));
    }

    res.transform_into(|i| i.assume_init())
}

pub fn display_stage_2(stage_2: &Grid<IntermediatePixel>) -> RgbImage {
    RgbImage::from_fn(
        stage_2.width() as u32,
        stage_2.height() as u32,
        |col, row| match unsafe { stage_2.get_unchecked(row as usize, col as usize) } {
            IntermediatePixel::Pointer { direction, .. } => {
                if direction {
                    Rgb([255, 0, 0])
                } else {
                    Rgb([0, 0, 255])
                }
            }
            IntermediatePixel::SameAsLast(..) => Rgb([0, 255, 0]),
            other => Rgb(other.into_inner()),
        },
    )
}

pub fn serialize_stage_4(stage_3: Grid<IntermediatePixel>) -> Vec<u8> {
    let mut iterator = stage_3.into_vec().into_iter();
    let mut current: RepType = match iterator.next().unwrap() {
        IntermediatePixel::SameAsLast(..) => RepType::SameAsLast(1),
        IntermediatePixel::Pixel(px) => RepType::Pixels(vec![px]),
        IntermediatePixel::Pointer { .. } => unreachable!(),
    };

    let mut ser = BitVec::<u8, Lsb0>::new();

    for i in iterator {
        match i {
            IntermediatePixel::Pointer { direction, .. } => {
                if let RepType::Pointer(ref mut ptr) = current {
                    match ptr {
                        Pointer::Repeat(ref mut count, ref c_dir) => {
                            if *c_dir == direction {
                                if *count == u8::MAX {
                                    current.serialize(&mut ser);
                                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                                } else {
                                    *count += 1;
                                }
                            } else {
                                //TODO make this a real value
                                if *count < 10 {
                                    // less than some ammount of repetitions, so it is better just to switch over to a series variant instead
                                    let mut ptrs = vec![*c_dir; *count as usize];
                                    ptrs.push(direction);
                                    current = RepType::Pointer(Pointer::Series(ptrs));
                                } else {
                                    // more than some ammount of reps, so it is better to make a new set of reps to go on with
                                    current.serialize(&mut ser);
                                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                                }
                            }
                        }
                        Pointer::Series(ref mut ptrs) => {
                            if ptrs.len() == 255 {
                                current.serialize(&mut ser);
                                current = RepType::Pointer(Pointer::Repeat(1, direction));
                            } else {
                                ptrs.push(direction);
                            }
                        }
                    }
                } else {
                    current.serialize(&mut ser);
                    current = RepType::Pointer(Pointer::Repeat(1, direction));
                }
            }
            IntermediatePixel::SameAsLast(..) => {
                if let RepType::SameAsLast(ref mut count) = current {
                    if *count == 255 {
                        current.serialize(&mut ser);
                        current = RepType::SameAsLast(1);
                    } else {
                        *count += 1;
                    }
                } else {
                    current.serialize(&mut ser);
                    current = RepType::SameAsLast(1);
                }
            }
            IntermediatePixel::Pixel(pixel) => {
                if let RepType::Pixels(ref mut pixels) = current {
                    if pixels.len() == 255 {
                        current.serialize(&mut ser);
                        current = RepType::Pixels(vec![pixel]);
                    } else {
                        pixels.push(pixel);
                    }
                } else {
                    current.serialize(&mut ser);
                    current = RepType::Pixels(vec![pixel]);
                }
            }
        }
    }

    current.serialize(&mut ser);
    ser.into_vec()
}

#[derive(Debug)]
pub struct Encoder {
    /// number of bytes to send in a packet. if zero, a entire image of bytes will be sent in one go
    bytes_per_packet: usize,
    last_frame: RgbImage,
    pending_packets: Vec<Packet>,
}

impl Encoder {
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            bytes_per_packet: 0,
            last_frame: RgbImage::from_pixel(width, height, Rgb([0; 3])),
            pending_packets: vec![Packet {
                metadata: PacketMetadata::Init {
                    frame_size: (width, height),
                },
                data: vec![],
            }],
        }
    }

    pub fn packets(&mut self) -> impl Iterator<Item = Packet> + '_ {
        self.pending_packets.drain(..)
    }

    pub fn encode_frame(&mut self, frame: RgbImage) {
        let pt0 = unsafe { serialize_stage_0(&self.last_frame, &frame) };
        self.last_frame = frame;

        let serialized = serialize_stage_4(pt0);

        if self.bytes_per_packet == 0 {
            self.pending_packets.push(Packet {
                metadata: PacketMetadata::NewFrame(serialized.len()),
                data: serialized,
            });
        } else {
            let mut iter = serialized.chunks(self.bytes_per_packet);
            self.pending_packets.push(Packet {
                metadata: PacketMetadata::NewFrame(serialized.len()),
                data: iter.next().unwrap().to_vec(),
            });
            for packet_data in iter {
                self.pending_packets.push(Packet {
                    metadata: PacketMetadata::FrameData,
                    data: packet_data.to_vec(),
                });
            }
        }
    }
}
