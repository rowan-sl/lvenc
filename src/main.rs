mod utils;

use lvenc::*;
use anyhow::Result;
use cv::prelude::*;
use cv::videoio::VideoCapture;
// use image::{Rgb, RgbImage};
use opencv as cv;
// use std::io::Write;
use std::path::PathBuf;
use std::time::{Instant, Duration};
// use std::fs::OpenOptions;
use stati::prelude::*;
use utils::mat_to_image;

fn main() -> Result<()> {
    for source_video in std::fs::read_dir("videos")? {
        let path = source_video?.path();
        println!("\n\ntesting video {}", path.display());
        // test_visualization(path.clone(), "out")?;
        test_full(path)?;
    }
    Ok(())
}

// pub fn test_visualization(vid: PathBuf, out_dir: &str) -> Result<()> {
//     let mut cap = VideoCapture::from_file(vid.to_str().unwrap(), cv::videoio::CAP_ANY)?;

//     let (before_img_path, stage_two_path, out_file) = {
//         let mut out_dir = PathBuf::from(out_dir);
//         out_dir.push(PathBuf::from(vid).file_name().unwrap());

//         if !out_dir.exists() {
//             std::fs::create_dir(&out_dir)?;
//         }

//         let mut before_img_path = out_dir.clone();
//         before_img_path.push("before.png");

//         let mut stage_two_path = out_dir.clone();
//         stage_two_path.push("out.bmp");

//         let mut out_file = out_dir.clone();
//         out_file.push("out.bin");

//         (before_img_path, stage_two_path, out_file)
//     };

//     let mut frame1 = Mat::default();
//     let mut frame2 = Mat::default();

//     assert!(cap.read(&mut frame1)?);
//     assert!(!frame1.empty());
//     assert!(cap.read(&mut frame2)?);
//     assert!(!frame2.empty());

//     mat_to_image(&frame2)?.save(before_img_path)?;

//     let before_pt1 = Instant::now();

//     let pt0_old = serialize_stage_0_new(
//         &RgbImage::from_pixel(
//             frame1.size().unwrap().width as u32,
//             frame1.size().unwrap().height as u32,
//             Rgb([0; 3]),
//         ),
//         &mat_to_image(&frame1).unwrap(),
//     );
//     let pt0 = serialize_stage_0_new(
//         &mat_to_image(&frame1).unwrap(),
//         &mat_to_image(&frame2).unwrap(),
//     );
//     let pt1 = serialize_stage_1_new(Some(pt0_old), pt0);
//     let pt2 = serialize_stage_2_new(pt1, (frame1.size().unwrap().width as usize, frame1.size().unwrap().height as usize));

//     let after_pt1 = Instant::now();
//     let before_pt2 = Instant::now();

//     let pt3 = serialize_stage_3_new(pt2.clone());
//     let pt4 = serialize_stage_4(pt3);
//     let pt5 = serialize_stage_5(pt4);

//     let after_pt2 = Instant::now();

//     println!("Serialization pt 1 took {:?}", (after_pt1 - before_pt1) + (after_pt2 - before_pt2));

//     display_stage_2(&pt2).save(stage_two_path).unwrap();

//     OpenOptions::new().create(true).write(true).open(out_file)?.write_all(&pt5)?;

//     println!(
//         "minimum time to do one frame at 60fps is {}ms",
//         1f64 / 60f64 * 1000f64
//     );

//     Ok(())
// }

pub fn test_full(vid: PathBuf) -> Result<()> {
    let mut cap = VideoCapture::from_file(vid.to_str().unwrap(), cv::videoio::CAP_ANY)?;

    let mut frame = Mat::default();

    let mut total_encoding_time = Duration::ZERO;
    let mut total_decoding_time = Duration::ZERO;
    let mut longest_individual_encoding_frametime = Duration::ZERO;
    let mut longest_individual_decoding_frametime = Duration::ZERO;
    let mut shortest_individual_encoding_frametime = Duration::MAX;
    let mut shortest_individual_decoding_frametime = Duration::MAX;
    let mut total_video_size: usize = 0;
    let original_video_size = std::fs::metadata(vid.clone())?.len();
    let fps = cap.get(opencv::videoio::CAP_PROP_FPS)?;
    let num_frames = cap.get(opencv::videoio::CAP_PROP_FRAME_COUNT)? as u32;
    let width = cap.get(opencv::videoio::CAP_PROP_FRAME_WIDTH)? as u32;
    let height = cap.get(opencv::videoio::CAP_PROP_FRAME_HEIGHT)? as u32;
    let video_length = Duration::from_secs_f64(num_frames as f64 / fps);
    let max_time_per_frame = 1f64 / fps * 1000f64;

    let mut encoder = Encoder::new(width, height);
    let mut decoder = Decoder::new();

    let mut bar_manager = stati::BarManager::new();
    let mut encoding_progress = bar_manager.register(stati::bars::SimpleBar::new("Encoding video", num_frames as usize));

    let mut frame_n = 0usize;
    loop {
        if !cap.read(&mut frame)? || frame.empty() {
            break;
        }
        let frame_image = mat_to_image(&frame)?;
        let before_encoding = Instant::now();
        encoder.encode_frame(frame_image.clone());
        let after_encoding = Instant::now();
        let encoding_time = after_encoding - before_encoding;
        if encoding_time > longest_individual_encoding_frametime {
            longest_individual_encoding_frametime = encoding_time;
        } else if encoding_time < shortest_individual_encoding_frametime {
            shortest_individual_encoding_frametime = encoding_time;
        }
        let before_decoding = Instant::now();
        for packet in encoder.packets() {
            total_video_size = total_video_size.checked_add(packet.len()).unwrap();
            decoder.feed_packet(packet);
        }
        let after_decoding = Instant::now();
        let decoding_time = after_decoding - before_decoding;
        if decoding_time > longest_individual_decoding_frametime {
            longest_individual_decoding_frametime = decoding_time;
        } else if decoding_time < shortest_individual_decoding_frametime {
            shortest_individual_decoding_frametime = decoding_time;
        }
        total_encoding_time += encoding_time;
        total_decoding_time += decoding_time;
        let decoded_frame = decoder.next_frame().unwrap();
        if frame_image != decoded_frame {
            frame_image.save("borked_in.png")?;
            decoded_frame.save("borked_out.png")?;
            panic!("input image did not match resulting image");
        }
        frame_n += 1;
        encoding_progress.bar().set_progress(frame_n);
        encoding_progress.bar().set_name(format!("Encoding video {frame_n}/{num_frames}"));
        bar_manager.print();
    }

    let average_encoding_time = total_encoding_time / num_frames;
    let average_decoding_time = total_decoding_time / num_frames;
    let total_time = total_decoding_time + total_encoding_time;

    println!("Results for {vid:?}");
    println!("took {total_time:?} to process {num_frames} frames");
    println!("total encoding time of {total_encoding_time:?}, min {shortest_individual_encoding_frametime:?}, max {longest_individual_encoding_frametime:?}, avg {average_encoding_time:?}");
    println!("total decoding time of {total_decoding_time:?}, min {shortest_individual_decoding_frametime:?}, max {longest_individual_decoding_frametime:?}, avg {average_decoding_time:?}");
    println!("maximum acceptable time to process one frame is aprox {max_time_per_frame:?}");
    println!("original video size is {}, encoded size is {}", original_video_size, total_video_size);
    println!("video stats:");
    println!("resolution {width}x{height} at {fps} fps");
    println!("video length: {video_length:?}");

    Ok(())
}
