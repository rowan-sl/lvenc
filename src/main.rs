use lvenc::utils::mat_to_image;
use opencv::prelude::*;
use opencv::videoio::VideoCapture;
use serde::{Serialize, Deserialize};
use image::RgbImage;
use std::time::Instant;
use bitvec::prelude::*;

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
struct Pixel ([u8; 3]);

impl From<Pixel> for EncodedCell {
    fn from(px: Pixel) -> Self {
        if px == BLACK_PIXEL {
            Self::BlackPixel
        } else if px == WHITE_PIXEL {
            Self::WhitePixel
        } else {
            Self::Pixel(px)
        }
    }
}

impl From<image::Rgb<u8>> for EncodedCell {
    fn from(rgb: image::Rgb<u8>) -> Self {
        let px: Pixel = rgb.into();
        px.into()
    }
}

const BLACK_PIXEL: Pixel = Pixel([0,0,0]);
const WHITE_PIXEL: Pixel = Pixel([255,255,255]);

const BLACK_PIXEL_RGB: image::Rgb<u8> = image::Rgb([0,0,0]);
const WHITE_PIXEL_RGB: image::Rgb<u8> = image::Rgb([255,255,255]);

impl From<image::Rgb<u8>> for Pixel {
    fn from(other: image::Rgb<u8>) -> Self {
        Self (
            [
                other.0[0],
                other.0[1],
                other.0[2],
            ]
        )
    }
}

impl From<Pixel> for image::Rgb<u8> {
    fn from(other: Pixel) -> Self {
        Self (
            [
                other.0[0],
                other.0[1],
                other.0[2],
            ]
        )
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
enum EncodedCell {
    BlackPixel,
    WhitePixel,

    Pixel ( Pixel ),

    /// can be a repitition of any pixel type
    Repitition,
    Repititions ( u32 ),
}

impl EncodedCell {
    pub fn is_rep(&self) -> bool {
        match self {
            EncodedCell::Repitition |
            EncodedCell::Repititions(_) => true,
            _ => false,
        }
    }
}

/// Some notes: pixel order is left-right bottom-top starting at bottom left
#[derive(Serialize, Deserialize, Debug, Clone)]
enum EncodedImage {
    FirstImage {
        width: u32, //height can be found by running `pixels.len()/width`
        pixels: Vec<Pixel>
    },
    IncompleteImage {
        cells: Vec<EncodedCell>
    }
}


fn encode(last: Option<RgbImage>, next: RgbImage) -> EncodedImage {
    if let Some(last) = last {
        let mut last_pixels = last.pixels();
        let mut next_pixels = next.pixels();

        let mut current_element: Option<EncodedCell> = None;
        let mut encoded_elements: Vec<EncodedCell> = vec![];

        loop {
            match (
                last_pixels.next(),
                next_pixels.next(),
            ) {
                (Some(last_px), Some(next_px)) => {
                    if current_element.is_none() {
                        if *next_px == BLACK_PIXEL_RGB {
                            current_element = Some(EncodedCell::BlackPixel);
                        } else if *next_px == WHITE_PIXEL_RGB {
                            current_element = Some(EncodedCell::WhitePixel);
                        } else {
                            current_element = Some(EncodedCell::Pixel((*next_px).into()));
                        }
                    } else {
                        if next_px == last_px {
                            // no change in pixels for this frame
                            if current_element.unwrap().is_rep() {
                                match current_element.unwrap() {
                                    // next is the same as the last so this means we need to add one more repitition.
                                    // that happens here
                                    EncodedCell::Repitition => {
                                        current_element = Some(EncodedCell::Repititions(2));
                                    }
                                    EncodedCell::Repititions(reps) => {
                                        if reps+1 < u32::MAX - 1 {
                                            current_element = Some(EncodedCell::Repititions(reps+1));
                                        } else {
                                            encoded_elements.push(current_element.unwrap());
                                            current_element = Some(EncodedCell::Repitition);
                                        }
                                    }
                                    _ => unreachable!()
                                }
                            } else {
                                //current is not a repitition, and next = last, so that must mean that current = last
                                //this means that we make it a repitition
                                current_element = Some(EncodedCell::Repitition);
                            }
                        } else {
                            // changes in pixels
                            if current_element.unwrap().is_rep() {
                                // currently it is a rep, so end that and save it, then change current to new
                                encoded_elements.push(current_element.unwrap());
                                current_element = Some((*next_px).into());
                            } else {
                                // not a rep, so save current and update it to next
                                encoded_elements.push(current_element.unwrap());
                                current_element = Some((*next_px).into());
                            }
                        }
                    }
                }
                (None, None) => {
                    encoded_elements.push(current_element.unwrap());
                    break;
                }
                _ => {
                    panic!("Last and next images are of different sizes!");
                }
            }
        }
        EncodedImage::IncompleteImage {
            cells: encoded_elements
        }
    } else {
        let mut pixels: Vec<Pixel> = Vec::new();
        for pixel in next.pixels() {
            pixels.push((*pixel).into());
        }
        EncodedImage::FirstImage {
            width: next.width(),
            pixels
        }
    }
}

fn cerealize(to_ser: EncodedImage) -> Vec<u8> {
    let mut cereal: BitVec<Lsb0, u8> = BitVec::new();
    match to_ser {
        EncodedImage::FirstImage { width, pixels} => {
            let hight: u32 = (pixels.len()/width as usize).try_into().unwrap();
            cereal.push(true);
            cereal.extend_from_bitslice(hight.view_bits::<Lsb0>());
            for pixel in pixels {
                cereal.extend_from_raw_slice(&pixel.0);
            }
        }
        EncodedImage::IncompleteImage { cells} => {
            cereal.push(false);
            for cell in cells {
                match cell {
                    // different encodings can be of different sizes, but they must be able to be diferentiated
                    EncodedCell::BlackPixel => {
                        cereal.push(false);// is it a standard pixel
                        cereal.push(false);// is it a rep
                        cereal.push(false);// false = black, true = white
                    }
                    EncodedCell::WhitePixel => {
                        cereal.push(false);
                        cereal.push(false);
                        cereal.push(true);
                    }
                    EncodedCell::Repitition => {
                        cereal.push(false);// is it a standard
                        cereal.push(true);// is it rep
                        cereal.push(false);//rep more than once
                    }
                    EncodedCell::Repititions(num_reps) => {
                        cereal.push(false);
                        cereal.push(true);
                        cereal.push(true);

                        let num_reps_bits = num_reps.view_bits::<Lsb0>();
                        let trailing = num_reps_bits.last_one();
                        if let Some(amnt_trailing) = trailing {
                            if amnt_trailing == 0 {
                                panic!()
                            }
                            //this is fine, because the number of bits in u32 (which this represeents at max) is less than 255
                            let small_trailing: u8 = amnt_trailing.try_into().unwrap();
                            let small_trailing_bits = small_trailing.view_bits::<Lsb0>();
                            // store the number of bits after this (takes 5 bits max to store this)
                            for i in 0..5 {
                                cereal.push(*small_trailing_bits.get(i).unwrap());
                            }
                            // extend the data, but only the data up to the last one (length of this stored in last step)
                            cereal.extend_from_bitslice(num_reps_bits.split_at(amnt_trailing+1).0);
                        } else {
                            eprintln!("Number of repeititions is zero! this should not happen");
                            unreachable!();
                        }
                    }
                    EncodedCell::Pixel(pix) => {
                        cereal.push(true);
                        cereal.extend_from_bitslice(pix.0[0].view_bits::<Lsb0>());
                        cereal.extend_from_bitslice(pix.0[1].view_bits::<Lsb0>());
                        cereal.extend_from_bitslice(pix.0[2].view_bits::<Lsb0>());
                    }
                }
            }
        }
    }
    cereal.into_vec()
}

fn main() {
    let mut cap = VideoCapture::from_file(
        "salmon_cannon.mp4",
        opencv::videoio::CAP_ANY
    ).unwrap();

    let mut frame = Mat::default();

    assert!(cap.read(&mut frame).unwrap());
    let img1 = mat_to_image(&frame).unwrap();

    assert!(cap.read(&mut frame).unwrap());
    let img2 = mat_to_image(&frame).unwrap();

    let img1copy = img1.clone();

    let before1 = Instant::now();
    let encoded1 = encode(None, img1copy);
    let encode1 = Instant::now();
    dbg!(cerealize(encoded1).len());
    let after1 = Instant::now();

    let before2 = Instant::now();
    let encoded2 = encode(Some(img1), img2);
    let encode2 = Instant::now();
    dbg!(cerealize(encoded2).len());
    let after2 = Instant::now();

    dbg!(encode1-before1);
    dbg!(after1-encode1);

    dbg!(encode2-before2);
    dbg!(after2-encode2);
}