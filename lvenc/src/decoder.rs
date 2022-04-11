use crate::{grid::Grid, packet::*, utils::*, Pointer, RepType, SingleRepType};
use bitvec::prelude::*;
use image::{Rgb, RgbImage};

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

    unsafe fn attempt_decode(&mut self) {
        let bytes_for_next_frame = self.bytes_for_next_frame.unwrap();
        if self.pending_frame_data.len() >= bytes_for_next_frame {
            let mut frame_data = self
                .pending_frame_data
                .drain(0..bytes_for_next_frame)
                .collect::<BitVec<u8, Lsb0>>()
                .into_iter();
            let mut decoded_reptypes: Vec<RepType> = vec![];

            while frame_data.len() >= 8 {
                // println!("{}", frame_data.len());
                match (frame_data.next().unwrap(), frame_data.next().unwrap()) {
                    (false, false) => {
                        let num_of_same = collect_byte(&mut frame_data);
                        decoded_reptypes.push(RepType::SameAsLast(num_of_same));
                    }
                    (true, false) => {
                        let mut pixels = vec![];
                        for _ in 0..collect_byte(&mut frame_data) {
                            pixels.push([
                                collect_byte(&mut frame_data),
                                collect_byte(&mut frame_data),
                                collect_byte(&mut frame_data),
                            ]);
                        }
                        decoded_reptypes.push(RepType::Pixels(pixels));
                    }
                    (false, true) => {
                        let mut changes = vec![];
                        for _ in 0..collect_byte(&mut frame_data) {
                            changes.push([
                                collect_i4(&mut frame_data),
                                collect_i4(&mut frame_data),
                                collect_i4(&mut frame_data),
                            ]);
                        }
                        decoded_reptypes.push(RepType::SmallChange(changes));
                    }
                    (true, true) => {
                        if frame_data.next().unwrap() {
                            // series of different ptrs
                            let mut ptrs = vec![];
                            for _ in 0..collect_byte(&mut frame_data) {
                                ptrs.push(frame_data.next().unwrap());
                            }
                            decoded_reptypes.push(RepType::Pointer(Pointer::Series(ptrs)));
                        } else {
                            // repetition of one
                            let count = collect_byte(&mut frame_data);
                            let direction = frame_data.next().unwrap();
                            decoded_reptypes
                                .push(RepType::Pointer(Pointer::Repeat(count, direction)));
                        }
                    }
                }
            }

            let single_reptypes = Grid::<SingleRepType>::from_iter(
                self.frame_size.unwrap().0 as usize,
                self.frame_size.unwrap().1 as usize,
                decoded_reptypes
                    .into_iter()
                    .map(|i| match i {
                        RepType::Pixels(pixels) => pixels
                            .into_iter()
                            .map(|i| SingleRepType::Pixel(i))
                            .collect::<Vec<_>>(),
                        RepType::SmallChange(changes) => changes
                            .into_iter()
                            .map(|i| SingleRepType::SmallChange(i))
                            .collect::<Vec<_>>(),
                        RepType::SameAsLast(num) => std::iter::repeat(SingleRepType::SameAsLast)
                            .take(num as usize)
                            .collect::<Vec<_>>(),
                        RepType::Pointer(ptr) => match ptr {
                            Pointer::Repeat(num, direction) => {
                                std::iter::repeat(SingleRepType::Pointer(direction))
                                    .take(num as usize)
                                    .collect::<Vec<_>>()
                            }
                            Pointer::Series(ptrs) => ptrs
                                .into_iter()
                                .map(|i| SingleRepType::Pointer(i))
                                .collect::<Vec<_>>(),
                        },
                    })
                    .flatten(),
            )
            .unwrap();

            let mut frame = self.last_frame.take().unwrap();

            for (x, y, rt) in single_reptypes.into_row_major_iter() {
                match rt {
                    SingleRepType::Pixel(pixel) => {
                        frame.get_pixel_mut(x as u32, y as u32).0 = pixel;
                    }
                    SingleRepType::Pointer(direction) => {
                        *frame.get_pixel_mut(x as u32, y as u32) = if direction {
                            *frame.get_pixel_mut(x as u32, y as u32 - 1)
                        } else {
                            *frame.get_pixel_mut(x as u32 - 1, y as u32)
                        };
                    }
                    SingleRepType::SmallChange(change) => {
                        let at = frame.get_pixel_mut(x as u32, y as u32);
                        // println!("{:?} + {:?}", at.0, change);
                        at.0[0] = u8::try_from(i16::from(at.0[0]) + i16::from(change[0])).unwrap();
                        at.0[1] = u8::try_from(i16::from(at.0[1]) + i16::from(change[1])).unwrap();
                        at.0[2] = u8::try_from(i16::from(at.0[2]) + i16::from(change[2])).unwrap();
                    }
                    SingleRepType::SameAsLast => { /* hehe */ }
                }
            }

            self.last_frame = Some(frame.clone());
            self.pending_frames.push(frame);
            self.bytes_for_next_frame = None;
        }
    }

    /// # Saftey
    ///
    /// its probably fine...
    pub fn feed_packet(&mut self, packet: Packet) {
        // TODO remove panics
        match packet.metadata {
            PacketMetadata::Init { frame_size } => {
                self.frame_size = Some(frame_size);
                self.last_frame = Some(RgbImage::from_pixel(
                    frame_size.0,
                    frame_size.1,
                    Rgb([0; 3]),
                ));
            }
            PacketMetadata::NewFrame(num_bytes) => {
                if let None = self.bytes_for_next_frame {
                    self.bytes_for_next_frame = Some(num_bytes);
                    self.pending_frame_data.clear();
                } else {
                    panic!("new frame header sent, but the last frame has not been completed");
                }
                self.pending_frame_data.extend_from_slice(&packet.data);
                unsafe { self.attempt_decode(); }
            }
            PacketMetadata::FrameData => {
                self.pending_frame_data.extend_from_slice(&packet.data);
                unsafe { self.attempt_decode(); }
            }
        }
    }

    pub fn frames(&mut self) -> impl Iterator<Item = RgbImage> + '_ {
        self.pending_frames.drain(..)
    }

    pub fn next_frame(&mut self) -> Option<RgbImage> {
        if self.pending_frames.is_empty() {
            None
        } else {
            Some(self.pending_frames.remove(0))
        }
    }
}

/// if `Decoder == Ready`, this means there are frames ready to consume
pub struct Ready;

impl PartialEq<Ready> for Decoder {
    fn eq(&self, _: &Ready) -> bool {
        !self.pending_frames.is_empty()
    }
}
