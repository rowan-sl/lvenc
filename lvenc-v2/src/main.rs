mod imageiter;
mod utils;

extern crate anyhow;
extern crate array2d;
extern crate image;
extern crate opencv;
extern crate thiserror;
extern crate bitvec;

use std::io::Write;
use std::path::PathBuf;
use std::{hint::unreachable_unchecked, fs::OpenOptions};
use std::time::Instant;
use bitvec::prelude::*;
use anyhow::Result;
use array2d::Array2D;
use cv::prelude::*;
use cv::videoio::VideoCapture;
use image::{Rgb, RgbImage};
use opencv as cv;
use utils::mat_to_image;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResCell {
    RGB(Rgb<u8>),
    // SmallChange(u8, u8, u8),
    // pointer left or up (rgb contained is not used in serializing)
    PtrL(Rgb<u8>),
    PtrU(Rgb<u8>),
}

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
    const ACCEPTABLE: i16 = 15;// 4 bit + one for signedness
    (px1.0[0].diff(px2.0[0]).abs() <= ACCEPTABLE && px1.0[1].diff(px2.0[1]).abs() <= ACCEPTABLE && px1.0[2].diff(px2.0[2]).abs() <= ACCEPTABLE)
    &&
    (px1.0[0] != 0 || px1.0[0] != 0 || px1.0[0] != 0)
}


pub fn display_stage_1(src: &Array2D<Option<ResCell>>) -> RgbImage {
    RgbImage::from_fn(src.num_columns() as u32, src.num_rows() as u32, |x, y| {
        match src.get(y as usize, x as usize).unwrap().as_ref().unwrap() {
            ResCell::RGB(rgb) => *rgb,
            ResCell::PtrL(..) => Rgb([0, 0, 255]),
            ResCell::PtrU(..) => Rgb([255, 0, 0]),
        }
    })
}

/// takes a opencv Mat image and converts it into a grid of ResCells, for later conversion
pub fn seri_stage_1(frame: Mat, before_loc: PathBuf) -> Result<Array2D<Option<ResCell>>> {
    let size = frame.size()?;
    println!("{:?}", size);
    let mut res_arr =
        Array2D::<Option<ResCell>>::filled_with(None, size.height as usize, size.width as usize);

    let im = mat_to_image(&frame)?;
    im.save(before_loc)?;
    println!("width: {}, height: {}", im.width(), im.height());

    'main: for (x, y, px) in im.enumerate_pixels() {
        for (o_x, o_y, i) in [(x.saturating_sub(1), y, 0), (x, y.saturating_sub(1), 1)] {
            if o_x == x && o_y == y {
                continue;
            }
            if let Some(Some(other_c)) = res_arr.get(o_y as usize, o_x as usize) {
                let other_rgb = match *other_c {
                    ResCell::RGB(v) => v,
                    ResCell::PtrL(v) => v,
                    ResCell::PtrU(v) => v,
                };
                if px == &other_rgb {
                    res_arr
                        .set(
                            y as usize,
                            x as usize,
                            Some(if i == 0 {
                                // x-1
                                ResCell::PtrL(other_rgb)
                            } else if i == 1 {
                                // y-1
                                ResCell::PtrU(other_rgb)
                            } else {
                                unsafe { unreachable_unchecked() };
                            }),
                        )
                        .unwrap();
                    continue 'main;
                }
            }
        }
        res_arr
            .set(y as usize, x as usize, Some(ResCell::RGB(*px)))
            .unwrap();
    }

    Ok(res_arr)
}

/// flattens the grid into a vec of its elements
pub fn seri_stage_2(grid: Array2D<Option<ResCell>>) -> Vec<ResCell> {
    grid.elements_row_major_iter().map(|v| {*v.as_ref().unwrap()}).collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResCellRepeater {
    // none here means u8::max reps
    Repetition(ResCell, Option<u8>),
    Item(ResCell)
}

pub fn seri_stage_3(mut elems: Vec<ResCell>) -> Vec<ResCellRepeater> {
    let mut res = vec![];

    let mut repetitions = 0u8;
    let mut last_cell = elems.remove(0);

    for elem in elems.into_iter() {
        if last_cell == elem {
            repetitions += 1;
            if repetitions == u8::MAX {
                res.push(ResCellRepeater::Repetition(elem, None));
                repetitions = 0;
            }
        } else {
            if repetitions > 0 {
                res.push(ResCellRepeater::Repetition(elem, Some(repetitions)));
                repetitions = 0;
            }
            res.push(ResCellRepeater::Item(last_cell));
            last_cell = elem;
        }
    }

    res
}

pub fn serialize_rescell(r: ResCell, buf: &mut BitVec<u8, Msb0>) {

    //bits
    // is ptr
    //        no -> encoded r g b
    //        yes -> l = false u = true
    match r {
        ResCell::RGB(rgb) => {
            buf.push(false);
            buf.extend_from_raw_slice(&rgb.0)
        }
        ResCell::PtrL(..) => {
            buf.push(true);
            buf.push(false);
        }
        ResCell::PtrU(..) => {
            buf.push(true);
            buf.push(true);
        }
    }
}

pub fn seri_stage_4(elems: Vec<ResCellRepeater>) -> Vec<u8> {
    let mut bytes = BitVec::<u8, Msb0>::new();

    /*
    is rep
        n -> serialized cell
        y -> max reps?
                    n -> num reps (u8), item
                    y -> item  
    */
    for elem in elems {
        match elem {
            ResCellRepeater::Item(item) => {
                bytes.push(false);
                serialize_rescell(item, &mut bytes);
            }
            ResCellRepeater::Repetition(item, reps) => {
                bytes.push(true);
                match reps {
                    Some(n) => {
                        bytes.push(false);
                        bytes.extend_from_raw_slice(&[n]);
                    }
                    None => {
                        bytes.push(true);
                    }
                }
                serialize_rescell(item, &mut bytes);
            }
        }
    }

    bytes.into()
}

fn test_for(vid: PathBuf, out_dir: &str) -> Result<()> {
    let mut cap = VideoCapture::from_file(vid.to_str().unwrap(), cv::videoio::CAP_ANY)?;

    let (before_img_path, stage_one_path, out_file) = {
        let mut out_dir = PathBuf::from(out_dir);
        out_dir.push(PathBuf::from(vid).file_name().unwrap());

        if !out_dir.exists() {
            std::fs::create_dir(&out_dir)?;
        }

        let mut before_img_path = out_dir.clone();
        before_img_path.push("before.png");

        let mut stage_one_path = out_dir.clone();
        stage_one_path.push("out.bmp");

        let mut out_file = out_dir.clone();
        out_file.push("out.bin");

        (
            before_img_path,
            stage_one_path,
            out_file,
        )
    };

    let mut frame = Mat::default();

    assert!(cap.read(&mut frame)?);
    assert!(!frame.empty());

    
    let b = Instant::now();
    let pt1 = seri_stage_1(frame, before_img_path)?;
    display_stage_1(&pt1).save(stage_one_path)?;
    let pt2 = seri_stage_2(pt1);
    let pt3 = seri_stage_3(pt2);

    let mut last = None;
    let mut small = 0;
    let mut large = 0;
    for elem in &pt3 {
        match last {
            None => last = Some(*elem),
            Some(last_val) => {
                let vals = match (*elem, last_val) {
                    (ResCellRepeater::Item(ResCell::RGB(rgb1)), ResCellRepeater::Item(ResCell::RGB(rgb2))) => (rgb1, rgb2),
                    _ => continue
                };

                if pix_small_difference(vals.0, vals.1) {
                    // println!("{} {} {}", vals.0[0].diff(vals.1[0]), vals.0[1].diff(vals.1[1]), vals.0[2].diff(vals.1[2]));
                    small += 1;
                } else {
                    large += 1;
                }
                
                last = Some(*elem);
            }
        }
    }
    println!("small differences: {}, large differences {}\nratio of small:large is {}", small, large, small as f64 / large as f64);

    let pt4 = seri_stage_4(pt3);
    println!("Serialization pt 1 took {:?}", Instant::now()-b);

    OpenOptions::new().create(true).write(true).open(out_file)?.write_all(&pt4)?;

    println!(
        "minimum time to do one frame at 60fps is {}ms",
        1f64 / 60f64 * 1000f64
    );

    Ok(())
}

fn main() -> Result<()> {
    for source_video in std::fs::read_dir("videos")? {
        let path = source_video?.path();
        println!("\n\ntesting video {}", path.display());
        test_for(path, "out")?;
    }
    Ok(())
}
